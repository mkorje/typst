//! Utilities for color font handling

use std::fmt::Write as _;
use std::io::Read;

use read_fonts::TableProvider;
use read_fonts::types::{BoundingBox, GlyphId, Point as RfPoint};
use skrifa::MetadataProvider;
use skrifa::bitmap::{BitmapData, BitmapGlyph, BitmapStrikes, Origin};
use skrifa::color::{
    Brush, ColorPainter, ColorStop, CompositeMode, Extend, Transform as ColorTransform,
};
use skrifa::instance::{LocationRef, Size as ScalerSize};
use skrifa::outline::{DrawSettings, OutlinePen};
use typst_syntax::Span;
use usvg::tiny_skia_path;
use xmlwriter::XmlWriter;

use crate::foundations::Bytes;
use crate::layout::{Abs, Frame, FrameItem, Point, Rect, Size};
use crate::text::Font;
use crate::visualize::{
    ExchangeFormat, FixedStroke, Geometry, Image, RasterImage, Shape, SvgImage,
};

/// Whether this glyph should be rendered via simple outlining instead of via
/// `glyph_frame`.
pub fn should_outline(font: &Font, glyph_id: u16) -> bool {
    let face = font.fontations();
    let gid = GlyphId::new(u32::from(glyph_id));

    let has_outline_table =
        face.glyf().is_ok() || face.cff().is_ok() || face.cff2().is_ok();
    if !has_outline_table {
        return false;
    }

    let has_png = BitmapStrikes::new(face)
        .glyph_for_size(ScalerSize::unscaled(), gid)
        .is_some_and(|bg| matches!(bg.data, BitmapData::Png(_)));
    if has_png {
        return false;
    }

    if face.color_glyphs().get(gid).is_some() {
        return false;
    }

    if let Ok(svg) = face.svg()
        && matches!(svg.glyph_data(gid), Ok(Some(_)))
    {
        return false;
    }

    true
}

/// A frame that can draw a glyph.
#[derive(Clone)]
pub struct GlyphFrame {
    pub upem: Abs,
    pub item: GlyphFrameItem,
}

impl GlyphFrame {
    /// The font unit square.
    pub fn size(&self) -> Size {
        Size::splat(self.upem)
    }
}

impl From<GlyphFrame> for Frame {
    fn from(g: GlyphFrame) -> Self {
        let mut frame = Frame::soft(Size::splat(g.upem));
        match g.item {
            GlyphFrameItem::Tofu(pos, shape) => {
                frame.push(pos, FrameItem::Shape(shape, Span::detached()))
            }
            GlyphFrameItem::Image(pos, image, size) => {
                frame.push(pos, FrameItem::Image(image, size, Span::detached()))
            }
        }
        frame
    }
}

/// The glyph item that is drawn.
#[derive(Clone)]
pub enum GlyphFrameItem {
    /// A fallback rectangle.
    Tofu(Point, Shape),
    /// An image glyph.
    Image(Point, Image, Size),
}

impl GlyphFrameItem {
    /// The position of the glyph item inside the parent frame.
    pub fn pos(&self) -> Point {
        match *self {
            GlyphFrameItem::Tofu(pos, _) => pos,
            GlyphFrameItem::Image(pos, _, _) => pos,
        }
    }
}

/// Returns a frame representing a glyph and whether it is a fallback tofu
/// frame.
///
/// Should only be called on glyphs for which [`should_outline`] returns false.
///
/// The glyphs are sized in font units, [`text.item.size`] is not taken into
/// account.
///
/// [`text.item.size`]: crate::text::TextItem::size
#[comemo::memoize]
pub fn glyph_frame(font: &Font, glyph_id: u16) -> Option<GlyphFrame> {
    let upem = Abs::pt(font.units_per_em());
    let gid = GlyphId::new(u32::from(glyph_id));

    if let Some(frame) = draw_glyph(font, upem, gid) {
        return Some(frame);
    }

    // Generate a fallback tofu if the glyph couldn't be drawn, unless it is
    // the space glyph. Then, an empty frame does the job. (This happens for
    // some rare CBDT fonts, which don't define a bitmap for the space, but
    // also don't have a glyf or CFF table.)
    let not_space = font.fontations().charmap().map(' ') != Some(gid);
    not_space.then(|| draw_fallback_tofu(font, upem, gid))
}

/// Tries to draw a glyph.
fn draw_glyph(font: &Font, upem: Abs, gid: GlyphId) -> Option<GlyphFrame> {
    let face = font.fontations();

    if let Some(bitmap) =
        BitmapStrikes::new(face).glyph_for_size(ScalerSize::unscaled(), gid)
        && matches!(bitmap.data, BitmapData::Png(_))
        && let Some(item) = draw_raster_glyph(font, upem, &bitmap)
    {
        return Some(GlyphFrame { upem, item });
    }

    if face.color_glyphs().get(gid).is_some()
        && let Some(item) = draw_colr_glyph(font, upem, gid)
    {
        return Some(GlyphFrame { upem, item });
    }

    if let Ok(svg_table) = face.svg()
        && matches!(svg_table.glyph_data(gid), Ok(Some(_)))
        && let Some(item) = draw_svg_glyph(font, upem, gid)
    {
        return Some(GlyphFrame { upem, item });
    }

    None
}

/// Draws a fallback tofu box with the advance width of the glyph.
fn draw_fallback_tofu(font: &Font, upem: Abs, gid: GlyphId) -> GlyphFrame {
    let advance = font
        .fontations()
        .glyph_metrics(ScalerSize::unscaled(), LocationRef::default())
        .advance_width(gid)
        .map(|advance| Abs::pt(advance as f64))
        .unwrap_or(upem / 3.0);
    let inset = 0.15 * advance;
    let height = 0.7 * upem;
    let pos = Point::new(inset, upem - height);
    let size = Size::new(advance - inset * 2.0, height);
    let thickness = upem / 20.0;
    let stroke = FixedStroke { thickness, ..Default::default() };
    let shape = Geometry::Rect(size).stroked(stroke);
    GlyphFrame { upem, item: GlyphFrameItem::Tofu(pos, shape) }
}

/// Draws a raster glyph in a frame.
///
/// Supports only PNG images.
fn draw_raster_glyph(
    font: &Font,
    upem: Abs,
    bitmap: &BitmapGlyph,
) -> Option<GlyphFrameItem> {
    let BitmapData::Png(png_data) = bitmap.data else { return None };
    let data = Bytes::new(png_data.to_vec());
    let image = Image::plain(RasterImage::plain(data, ExchangeFormat::Png).ok()?);

    let pixels_per_em = bitmap.ppem_x as f64;
    let raster_x = bitmap.inner_bearing_x as f64;

    // Apple Color emoji doesn't provide offset information (or at least
    // not in a way ttf-parser understood), so we artificially shift their
    // baseline to make it look good.
    let raster_y_negated = if font.info().family.to_lowercase() == "apple color emoji" {
        20.0
    } else {
        // The original implementation negated `raster_image.y` from
        // ttf-parser. For sbix glyphs (placement origin bottom-left),
        // ttf-parser's `y` was the sbix `originOffsetY` directly; for
        // CBDT/EBDT (top-left), it was `-(height + bearing_y)`.
        match bitmap.placement_origin {
            Origin::BottomLeft => -(bitmap.inner_bearing_y as f64),
            Origin::TopLeft => bitmap.height as f64 + bitmap.inner_bearing_y as f64,
        }
    };

    let position = Point::new(
        upem * raster_x / pixels_per_em,
        upem * raster_y_negated / pixels_per_em,
    );
    let aspect_ratio = image.width() / image.height();
    let size = Size::new(upem, upem * aspect_ratio);
    Some(GlyphFrameItem::Image(position, image, size))
}

/// Draws a glyph from the COLR table into the frame.
fn draw_colr_glyph(font: &Font, upem: Abs, gid: GlyphId) -> Option<GlyphFrameItem> {
    let svg_string = colr_glyph_to_svg(font, gid)?;

    let head = font.fontations().head().ok()?;
    let x_min = head.x_min() as f64;
    let y_max = head.y_max() as f64;
    let width = (head.x_max() as f64) - x_min;
    let height = y_max - (head.y_min() as f64);

    let data = Bytes::from_string(svg_string);
    let image = Image::plain(SvgImage::new(data).ok()?);

    let y_shift = Abs::pt(upem.to_pt() - y_max);
    let position = Point::new(Abs::pt(x_min), y_shift);
    let size = Size::new(Abs::pt(width), Abs::pt(height));
    Some(GlyphFrameItem::Image(position, image, size))
}

/// Convert a COLR glyph into an SVG file.
fn colr_glyph_to_svg(font: &Font, gid: GlyphId) -> Option<String> {
    let face = font.fontations();
    let head = face.head().ok()?;
    let x_min = head.x_min() as f64;
    let y_max = head.y_max() as f64;
    let width = (head.x_max() as f64) - x_min;
    let height = y_max - (head.y_min() as f64);
    let tx = -x_min;
    let ty = -y_max;

    let mut svg = XmlWriter::new(xmlwriter::Options::default());
    svg.start_element("svg");
    svg.write_attribute("xmlns", "http://www.w3.org/2000/svg");
    svg.write_attribute("xmlns:xlink", "http://www.w3.org/1999/xlink");
    svg.write_attribute("width", &width);
    svg.write_attribute("height", &height);
    svg.write_attribute_fmt("viewBox", format_args!("0 0 {width} {height}"));

    svg.start_element("g");
    svg.write_attribute_fmt(
        "transform",
        format_args!("matrix(1 0 0 -1 0 0) matrix(1 0 0 1 {tx} {ty})"),
    );

    let color_glyph = face.color_glyphs().get(gid)?;

    let mut painter = GlyphPainter {
        face,
        svg: &mut svg,
        path_buf: String::with_capacity(256),
        gradient_index: 1,
        clip_path_index: 1,
        palette_index: 0,
        transform: ColorTransform::default(),
        transforms_stack: vec![ColorTransform::default()],
    };

    color_glyph.paint(LocationRef::default(), &mut painter).ok()?;
    svg.end_element();

    Some(svg.end_document())
}

/// Draws an SVG glyph in a frame.
fn draw_svg_glyph(font: &Font, upem: Abs, gid: GlyphId) -> Option<GlyphFrameItem> {
    // TODO: Our current conversion of the SVG table works for Twitter Color Emoji,
    // but might not work for others. See also: https://github.com/RazrFalcon/resvg/pull/776
    let mut data = font.fontations().svg().ok()?.glyph_data(gid).ok()??;

    // Decompress SVGZ.
    let mut decoded = vec![];
    if data.starts_with(&[0x1f, 0x8b]) {
        let mut decoder = flate2::read::GzDecoder::new(data);
        decoder.read_to_end(&mut decoded).ok()?;
        data = &decoded;
    }

    // Parse and simplify the SVG.
    let xml = std::str::from_utf8(data).ok()?;
    let document = roxmltree::Document::parse(xml).ok()?;
    let opts = usvg::Options::default();
    let tree = usvg::Tree::from_xmltree(&document, &opts).ok()?;
    let mut data = tree.to_string(&usvg::WriteOptions {
        indent: usvg::Indent::None,
        attributes_indent: usvg::Indent::None,
        ..Default::default()
    });

    // The SVG coordinates and the font coordinates are not the same: the Y axis
    // is mirrored. But the origin of the axes are the same (which means that
    // the horizontal axis in the SVG document corresponds to the baseline). See
    // the reference for more details:
    // https://learn.microsoft.com/en-us/typography/opentype/spec/svg#coordinate-systems-and-glyph-metrics
    //
    // Using this SVG directly can result in a cropped glyph. In order to avoid
    // clipping issues, we apply a translate transform so the top-left corner of
    // the bounding box is moved into the origin (0, 0) to make it fully fit
    // into the view port defined by `viewBox="0 0 width height"`, like a
    // conventional SVG.
    let bbox = tree.root().bounding_box();
    let view_box = Rect::new(
        Point::new(Abs::pt(bbox.left() as f64), Abs::pt(bbox.top() as f64)),
        Point::new(Abs::pt(bbox.right() as f64), Abs::pt(bbox.bottom() as f64)),
    );
    fixup_svg(&mut data, view_box);

    let data = Bytes::from_string(data);
    let image = Image::plain(SvgImage::new(data).ok()?);

    let position = Point::new(view_box.min.x, view_box.min.y + upem);
    let size = view_box.size();
    Some(GlyphFrameItem::Image(position, image, size))
}

/// Replace or insert the size attributes (viewBox, width and height), and
/// insert a group with a transform that translates the `viewBox` so that the
/// top-left point is at the origin (0, 0).
fn fixup_svg(svg: &mut String, view_box: Rect) {
    let mut viewbox_range = None;
    let mut width_range = None;
    let mut height_range = None;

    let mut s = unscanny::Scanner::new(svg);
    s.eat_until("<svg");
    s.expect("<svg");

    let svg_attr_start = s.cursor();

    while !s.eat_if('>') && !s.done() {
        s.eat_whitespace();
        let start = s.cursor();

        let attr_name = s.eat_until('=').trim();
        // Eat the equal sign and the quote.
        s.expect('=');
        s.eat_until('"');
        s.expect('"');

        while !s.eat_if('"') && !s.done() {
            s.eat();
        }

        match attr_name {
            "viewBox" => viewbox_range = Some(start..s.cursor()),
            "width" => width_range = Some(start..s.cursor()),
            "height" => height_range = Some(start..s.cursor()),
            _ => {}
        }
    }

    let svg_body_start = s.cursor();
    let Some(svg_body_end) = svg.rfind("</svg>") else {
        return;
    };

    svg.insert_str(svg_body_end, "</g>");
    svg.insert_str(
        svg_body_start,
        &format!(
            r#"<g transform="translate({} {})">"#,
            -view_box.min.x.to_pt(),
            -view_box.min.y.to_pt()
        ),
    );

    let size = view_box.size();
    let mut edits = [
        (
            viewbox_range,
            format!("viewBox=\"0 0 {} {}\"", size.x.to_pt(), size.y.to_pt(),),
        ),
        (width_range, format!("width=\"{}\"", size.x.to_pt())),
        (height_range, format!("height=\"{}\"", size.y.to_pt())),
    ];

    // Sort edits by ranges; missing ranges will be moved to the start.
    edits.sort_by_key(|(range, _)| range.clone().map(|r| r.start));

    // Replace or insert the attribute. Iterate in reverse, so the modifying the
    // string doesn't affect the ranges of the edits that are applied later on.
    for (range, str) in edits.into_iter().rev() {
        if let Some(range) = range {
            svg.replace_range(range, &str);
        } else {
            svg.insert_str(svg_attr_start, &str);
            svg.insert(svg_attr_start, ' ');
        }
    }
}

struct ColrBuilder<'a>(&'a mut String);

impl ColrBuilder<'_> {
    fn finish(&mut self) {
        if !self.0.is_empty() {
            self.0.pop(); // remove trailing space
        }
    }
}

impl OutlinePen for ColrBuilder<'_> {
    fn move_to(&mut self, x: f32, y: f32) {
        write!(self.0, "M {x} {y} ").unwrap()
    }

    fn line_to(&mut self, x: f32, y: f32) {
        write!(self.0, "L {x} {y} ").unwrap()
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        write!(self.0, "Q {x1} {y1} {x} {y} ").unwrap()
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        write!(self.0, "C {x1} {y1} {x2} {y2} {x} {y} ").unwrap()
    }

    fn close(&mut self) {
        self.0.push_str("Z ")
    }
}

// NOTE: This is only a best-effort translation of COLR into SVG. It's not feature-complete
// and it's also not possible to make it feature-complete using just raw SVG features.
pub(crate) struct GlyphPainter<'a> {
    pub(crate) face: &'a read_fonts::FontRef<'a>,
    pub(crate) svg: &'a mut XmlWriter,
    pub(crate) path_buf: String,
    pub(crate) gradient_index: usize,
    pub(crate) clip_path_index: usize,
    pub(crate) palette_index: u16,
    pub(crate) transform: ColorTransform,
    pub(crate) transforms_stack: Vec<ColorTransform>,
}

impl GlyphPainter<'_> {
    /// Outlines the given glyph into `path_buf`. Returns `true` if any drawing
    /// commands were emitted.
    fn outline_into_buf(&mut self, gid: GlyphId) -> bool {
        self.path_buf.clear();
        let outlines = self.face.outline_glyphs();
        let Some(outline) = outlines.get(gid) else { return false };
        let mut builder = ColrBuilder(&mut self.path_buf);
        if outline
            .draw(
                DrawSettings::unhinted(ScalerSize::unscaled(), LocationRef::default()),
                &mut builder,
            )
            .is_err()
        {
            return false;
        }
        builder.finish();
        !self.path_buf.is_empty()
    }

    /// Resolves a CPAL palette index and additional alpha into an `(r, g, b,
    /// alpha)` tuple. The `0xFFFF` palette index represents the foreground
    /// color and is mapped to opaque black for parity with the previous
    /// `ttf-parser`-based code.
    fn resolve_color(&self, palette_index: u16, alpha: f32) -> (u8, u8, u8, f32) {
        if palette_index == 0xFFFF {
            return (0, 0, 0, alpha);
        }
        if let Some(palette) = self.face.color_palettes().get(self.palette_index)
            && let Some(color) = palette.colors().get(palette_index as usize)
        {
            let combined_alpha = (color.alpha as f32 / 255.0) * alpha;
            return (color.red, color.green, color.blue, combined_alpha);
        }
        (0, 0, 0, alpha)
    }

    fn write_gradient_stops(&mut self, stops: &[ColorStop]) {
        for stop in stops {
            let (r, g, b, a) = self.resolve_color(stop.palette_index, stop.alpha);
            self.svg.start_element("stop");
            self.svg.write_attribute("offset", &stop.offset);
            self.svg
                .write_attribute_fmt("stop-color", format_args!("rgb({r}, {g}, {b})"));
            self.svg.write_attribute("stop-opacity", &a);
            self.svg.end_element();
        }
    }

    fn write_transform_attribute(&mut self, name: &str, ts: ColorTransform) {
        if ts == ColorTransform::default() {
            return;
        }
        self.svg.write_attribute_fmt(
            name,
            format_args!(
                "matrix({} {} {} {} {} {})",
                ts.xx, ts.yx, ts.xy, ts.yy, ts.dx, ts.dy
            ),
        );
    }

    fn write_spread_method_attribute(&mut self, extend: Extend) {
        let value = match extend {
            Extend::Pad => "pad",
            Extend::Repeat => "repeat",
            Extend::Reflect => "reflect",
            _ => "pad",
        };
        self.svg.write_attribute("spreadMethod", &value);
    }

    fn paint_solid(
        &mut self,
        palette_index: u16,
        alpha: f32,
        outline_transform: ColorTransform,
    ) {
        let (r, g, b, a) = self.resolve_color(palette_index, alpha);
        self.svg.start_element("path");
        self.svg
            .write_attribute_fmt("fill", format_args!("rgb({r}, {g}, {b})"));
        self.svg.write_attribute("fill-opacity", &a);
        self.write_transform_attribute("transform", outline_transform);
        self.svg.write_attribute("d", &self.path_buf);
        self.svg.end_element();
    }

    fn paint_linear_gradient(
        &mut self,
        p0: RfPoint<f32>,
        p1: RfPoint<f32>,
        color_stops: &[ColorStop],
        extend: Extend,
        outline_transform: ColorTransform,
    ) {
        let gradient_id = format!("lg{}", self.gradient_index);
        self.gradient_index += 1;

        let gradient_transform = paint_transform(outline_transform, self.transform);

        // TODO: We ignore p2. Have to apply it somehow.
        // TODO: The way spreadMode works in COLR and SVG is a bit different. In
        // SVG, the spreadMode will always be applied based on x1/y1 and x2/y2.
        // However, in COLR the spreadMode will be applied from the first/last
        // stop. So if we have a gradient with x1=0 x2=1, and a stop at x=0.4
        // and x=0.6, then in SVG we will always see padding, while in COLR we
        // will see the actual spreadMode. We need to account for that somehow.
        self.svg.start_element("linearGradient");
        self.svg.write_attribute("id", &gradient_id);
        self.svg.write_attribute("x1", &p0.x);
        self.svg.write_attribute("y1", &p0.y);
        self.svg.write_attribute("x2", &p1.x);
        self.svg.write_attribute("y2", &p1.y);
        self.svg.write_attribute("gradientUnits", &"userSpaceOnUse");
        self.write_spread_method_attribute(extend);
        self.write_transform_attribute("gradientTransform", gradient_transform);
        self.write_gradient_stops(color_stops);
        self.svg.end_element();

        self.svg.start_element("path");
        self.svg
            .write_attribute_fmt("fill", format_args!("url(#{gradient_id})"));
        self.write_transform_attribute("transform", outline_transform);
        self.svg.write_attribute("d", &self.path_buf);
        self.svg.end_element();
    }

    fn paint_radial_gradient(
        &mut self,
        c0: RfPoint<f32>,
        r0: f32,
        c1: RfPoint<f32>,
        r1: f32,
        color_stops: &[ColorStop],
        extend: Extend,
        outline_transform: ColorTransform,
    ) {
        let gradient_id = format!("rg{}", self.gradient_index);
        self.gradient_index += 1;

        let gradient_transform = paint_transform(outline_transform, self.transform);

        self.svg.start_element("radialGradient");
        self.svg.write_attribute("id", &gradient_id);
        self.svg.write_attribute("cx", &c1.x);
        self.svg.write_attribute("cy", &c1.y);
        self.svg.write_attribute("r", &r1);
        self.svg.write_attribute("fr", &r0);
        self.svg.write_attribute("fx", &c0.x);
        self.svg.write_attribute("fy", &c0.y);
        self.svg.write_attribute("gradientUnits", &"userSpaceOnUse");
        self.write_spread_method_attribute(extend);
        self.write_transform_attribute("gradientTransform", gradient_transform);
        self.write_gradient_stops(color_stops);
        self.svg.end_element();

        self.svg.start_element("path");
        self.svg
            .write_attribute_fmt("fill", format_args!("url(#{gradient_id})"));
        self.write_transform_attribute("transform", outline_transform);
        self.svg.write_attribute("d", &self.path_buf);
        self.svg.end_element();
    }

    fn fill_with_brush(&mut self, brush: Brush<'_>, outline_transform: ColorTransform) {
        match brush {
            Brush::Solid { palette_index, alpha } => {
                self.paint_solid(palette_index, alpha, outline_transform);
            }
            Brush::LinearGradient { p0, p1, color_stops, extend } => {
                self.paint_linear_gradient(
                    p0,
                    p1,
                    color_stops,
                    extend,
                    outline_transform,
                );
            }
            Brush::RadialGradient { c0, r0, c1, r1, color_stops, extend } => {
                self.paint_radial_gradient(
                    c0,
                    r0,
                    c1,
                    r1,
                    color_stops,
                    extend,
                    outline_transform,
                );
            }
            Brush::SweepGradient { .. } => {
                // SVG 1.1 has no direct sweep gradient counterpart.
            }
        }
    }

    fn clip_with_path(&mut self, path: &str, outline_transform: ColorTransform) {
        let clip_id = format!("cp{}", self.clip_path_index);
        self.clip_path_index += 1;

        self.svg.start_element("clipPath");
        self.svg.write_attribute("id", &clip_id);
        self.svg.start_element("path");
        self.write_transform_attribute("transform", outline_transform);
        self.svg.write_attribute("d", &path);
        self.svg.end_element();
        self.svg.end_element();

        self.svg.start_element("g");
        self.svg
            .write_attribute_fmt("clip-path", format_args!("url(#{clip_id})"));
    }
}

fn paint_transform(
    outline_transform: ColorTransform,
    transform: ColorTransform,
) -> ColorTransform {
    let outline = tiny_skia_path::Transform::from_row(
        outline_transform.xx,
        outline_transform.yx,
        outline_transform.xy,
        outline_transform.yy,
        outline_transform.dx,
        outline_transform.dy,
    );

    let gradient = tiny_skia_path::Transform::from_row(
        transform.xx,
        transform.yx,
        transform.xy,
        transform.yy,
        transform.dx,
        transform.dy,
    );

    // In theory, this could fail. But the transform shouldn't ever be
    // uninvertible, so let's ignore it.
    let result = outline.invert().unwrap_or_default().pre_concat(gradient);

    ColorTransform {
        xx: result.sx,
        yx: result.ky,
        xy: result.kx,
        yy: result.sy,
        dx: result.tx,
        dy: result.ty,
    }
}

impl ColorPainter for GlyphPainter<'_> {
    fn push_transform(&mut self, transform: ColorTransform) {
        self.transforms_stack.push(self.transform);
        self.transform = self.transform * transform;
    }

    fn pop_transform(&mut self) {
        if let Some(t) = self.transforms_stack.pop() {
            self.transform = t;
        }
    }

    fn fill(&mut self, brush: Brush<'_>) {
        // Reuse whatever path was last placed in `path_buf` (typically the
        // outline emitted by the most recent `fill_glyph` or
        // `push_clip_glyph`). An enclosing `<g clip-path>` from a prior
        // `push_clip_*` provides the actual clipping.
        let outline_transform = self.transform;
        self.fill_with_brush(brush, outline_transform);
    }

    fn fill_glyph(
        &mut self,
        glyph_id: GlyphId,
        brush_transform: Option<ColorTransform>,
        brush: Brush<'_>,
    ) {
        if !self.outline_into_buf(glyph_id) {
            return;
        }
        let outline_transform = self.transform;
        let saved = self.transform;
        if let Some(bt) = brush_transform {
            self.transform = self.transform * bt;
        }
        self.fill_with_brush(brush, outline_transform);
        self.transform = saved;
    }

    fn push_clip_glyph(&mut self, glyph_id: GlyphId) {
        if self.outline_into_buf(glyph_id) {
            let path = std::mem::take(&mut self.path_buf);
            self.clip_with_path(&path, self.transform);
            // Keep the outline available so a subsequent `fill` can reuse it.
            self.path_buf = path;
        } else {
            // Maintain group balance even if outlining failed.
            self.svg.start_element("g");
        }
    }

    fn push_clip_box(&mut self, clip_box: BoundingBox<f32>) {
        let x_min = clip_box.x_min;
        let x_max = clip_box.x_max;
        let y_min = clip_box.y_min;
        let y_max = clip_box.y_max;
        let path = format!(
            "M {x_min} {y_min} L {x_max} {y_min} L {x_max} {y_max} L {x_min} {y_max} Z"
        );
        self.clip_with_path(&path, self.transform);
    }

    fn pop_clip(&mut self) {
        self.svg.end_element();
    }

    fn push_layer(&mut self, mode: CompositeMode) {
        self.svg.start_element("g");
        // TODO: Need to figure out how to represent the other blend modes
        // in SVG.
        let mode = match mode {
            CompositeMode::SrcOver => "normal",
            CompositeMode::Screen => "screen",
            CompositeMode::Overlay => "overlay",
            CompositeMode::Darken => "darken",
            CompositeMode::Lighten => "lighten",
            CompositeMode::ColorDodge => "color-dodge",
            CompositeMode::ColorBurn => "color-burn",
            CompositeMode::HardLight => "hard-light",
            CompositeMode::SoftLight => "soft-light",
            CompositeMode::Difference => "difference",
            CompositeMode::Exclusion => "exclusion",
            CompositeMode::Multiply => "multiply",
            CompositeMode::HslHue => "hue",
            CompositeMode::HslSaturation => "saturation",
            CompositeMode::HslColor => "color",
            CompositeMode::HslLuminosity => "luminosity",
            _ => "normal",
        };
        self.svg.write_attribute_fmt(
            "style",
            format_args!("mix-blend-mode: {mode}; isolation: isolate"),
        );
    }

    fn pop_layer(&mut self) {
        self.svg.end_element();
    }
}
