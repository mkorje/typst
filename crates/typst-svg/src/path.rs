use ecow::EcoString;
use typst_library::layout::{Abs, Angle, Point, Ratio, Size};
use typst_utils::defer;

use crate::write::{SvgFormatter, SvgWrite};

/// A builder for SVG path using relative coordinates.
pub struct SvgPathBuilder {
    path: EcoString,
    scale: Ratio,
    /// The point of the last move, used to close paths with the `Z` command.
    last_close_point: Point,
    /// The end of the previous draw command, used for relative draw commands.
    last_point: Point,
    /// Whether any drawing command has been emitted by the outline pen.
    has_outline: bool,
}

impl SvgPathBuilder {
    /// Creates a path builder with an initial `M` command.
    pub fn with_translate(pos: Point) -> Self {
        let mut path = EcoString::from("M ");
        let mut f = SvgFormatter::new(&mut path);
        f.push_nums([pos.x.to_pt(), pos.y.to_pt()]);

        Self {
            path,
            scale: Ratio::one(),
            last_close_point: pos,
            last_point: Point::zero(),
            has_outline: false,
        }
    }

    /// Creates a path builder with a scale and an initial `M 0 0` command.
    pub fn with_scale(scale: Ratio) -> Self {
        Self {
            path: EcoString::from("M 0 0"),
            scale,
            last_close_point: Point::zero(),
            last_point: Point::zero(),
            has_outline: false,
        }
    }

    /// Creates an empty path builder.
    pub fn empty() -> Self {
        Self {
            path: EcoString::new(),
            scale: Ratio::one(),
            last_close_point: Point::zero(),
            last_point: Point::zero(),
            has_outline: false,
        }
    }

    /// Whether the outline pen received any drawing commands.
    pub fn has_outline(&self) -> bool {
        self.has_outline
    }

    /// Finish building the path.
    pub fn finsish(self) -> EcoString {
        self.path
    }

    fn write(&mut self) -> SvgFormatter<'_, EcoString> {
        SvgFormatter::new(&mut self.path)
    }

    fn map(&self, pos: Point) -> Point {
        self.scale.get() * (pos - self.last_point)
    }

    /// Create a rectangle path. The rectangle is created with the top-left
    /// corner at (0, 0). The width and height are the size of the rectangle.
    pub fn rect(&mut self, size: Size) {
        self.move_to(Point::zero());
        self.line_to(Point::with_y(size.y));
        self.line_to(size.to_point());
        self.line_to(Point::with_x(size.x));
        self.close();
    }

    /// Creates an arc path.
    pub fn arc(
        &mut self,
        radius: Size,
        x_axis_rot: Angle,
        large_arc_flag: u32,
        sweep_flag: u32,
        pos: Point,
    ) {
        let mut builder = defer(self, |b| b.last_point = pos);

        let radius = builder.map(radius.to_point());
        let pos = builder.map(pos);

        let mut f = builder.write();
        f.push_str("a ");
        f.push_nums([
            radius.x.to_pt(),
            radius.y.to_pt(),
            x_axis_rot.to_deg(),
            large_arc_flag as f64,
            sweep_flag as f64,
            pos.x.to_pt(),
            pos.y.to_pt(),
        ]);
    }

    pub fn move_to(&mut self, pos: Point) {
        let mut builder = defer(self, |b| {
            b.last_point = pos;
            b.last_close_point = pos;
        });

        let pos = builder.map(pos);
        if pos != Point::zero() {
            let mut f = builder.write();
            f.push_str("m ");
            f.push_nums([pos.x.to_pt(), pos.y.to_pt()]);
        }
    }

    pub fn line_to(&mut self, pos: Point) {
        let mut builder = defer(self, |b| b.last_point = pos);

        let pos = builder.map(pos);

        let mut f = builder.write();
        if pos.x != Abs::zero() && pos.y != Abs::zero() {
            f.push_str("l ");
            f.push_nums([pos.x.to_pt(), pos.y.to_pt()]);
        } else if pos.x != Abs::zero() {
            f.push_str("h ");
            f.push_num(pos.x.to_pt());
        } else if pos.y != Abs::zero() {
            f.push_str("v ");
            f.push_num(pos.y.to_pt());
        }
    }

    pub fn curve_to(&mut self, p1: Point, p2: Point, end: Point) {
        let mut builder = defer(self, |b| b.last_point = end);

        let p1 = builder.map(p1);
        let p2 = builder.map(p2);
        let end = builder.map(end);

        let mut f = builder.write();
        f.push_str("c ");
        f.push_nums([
            p1.x.to_pt(),
            p1.y.to_pt(),
            p2.x.to_pt(),
            p2.y.to_pt(),
            end.x.to_pt(),
            end.y.to_pt(),
        ]);
    }

    pub fn quad_to(&mut self, p1: Point, end: Point) {
        let mut builder = defer(self, |b| b.last_point = end);

        let p1 = builder.map(p1);
        let end = builder.map(end);

        let mut f = builder.write();
        f.push_str("q ");
        f.push_nums([p1.x.to_pt(), p1.y.to_pt(), end.x.to_pt(), end.y.to_pt()]);
    }

    pub fn close(&mut self) {
        self.write().push_str("Z ");
        self.last_point = self.last_close_point;
    }
}

mod outline {
    use typst_library::layout::{Abs, Point};

    use crate::path::SvgPathBuilder;

    /// A builder for SVG path. This is used to build the path for a glyph.
    impl skrifa::outline::OutlinePen for SvgPathBuilder {
        fn move_to(&mut self, x: f32, y: f32) {
            self.has_outline = true;
            self.move_to(point(x, y));
        }

        fn line_to(&mut self, x: f32, y: f32) {
            self.has_outline = true;
            self.line_to(point(x, y));
        }

        fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
            self.has_outline = true;
            self.quad_to(point(x1, y1), point(x, y));
        }

        fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
            self.has_outline = true;
            self.curve_to(point(x1, y1), point(x2, y2), point(x, y));
        }

        fn close(&mut self) {
            self.has_outline = true;
            // Some glyph paths differ from the previous `ttf_parser`-based
            // output in that an explicit segment back to the contour's start
            // is missing before `Z`. The visual result is identical (SVG `Z`
            // implies that line), but the exact byte representation differs.
            // Naively emitting `line_to(start)` here makes things worse — the
            // pattern of when `ttf_parser` did vs. didn't emit that segment
            // doesn't reduce to `last_point != last_close_point`.
            // if self.last_point != self.last_close_point {
            //     self.line_to(self.last_close_point);
            // }
            self.close();
        }
    }

    /// Helper to create points.
    fn point(x: f32, y: f32) -> Point {
        Point::new(Abs::pt(x as f64), Abs::pt(y as f64))
    }
}
