use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};

use comemo::Tracked;
use typst_utils::Numeric;

use crate::layout::{Abs, Axes, Rel, Sides, Size};

/// A single region to layout into.
#[derive(Debug, Copy, Clone, Hash)]
pub struct Region {
    /// The size of the region.
    pub size: Size,
    /// Whether elements should expand to fill the regions instead of shrinking
    /// to fit the content.
    pub expand: Axes<bool>,
}

impl Region {
    /// Create a new region.
    pub fn new(size: Size, expand: Axes<bool>) -> Self {
        Self { size, expand }
    }
}

impl From<Region> for Regions<'_> {
    fn from(region: Region) -> Self {
        Regions::new(region.size, region.size.y, &[], None, region.expand)
    }
}

/// A sequence of regions to layout into.
///
/// A *region* is a contiguous rectangular space in which elements
/// can be laid out. All regions within a `Regions` object have the
/// same width. This means that it is not currently possible to, for
/// instance, have content wrap to the side of a floating element.
///
/// Layout should find out as little as possible about the regions: Instead of
/// reading the available height, it should ask the most specific question it
/// can, like whether some content [fits](Self::fits). The exact heights are
/// only available through the blunt [`height`](Self::height),
/// [`backlog`](Self::backlog), and [`last`](Self::last).
///
/// This matters because regions can be _linked_ to other regions that are
/// tracked (see [`RegionsLink`]). Then, the questions are forwarded to the
/// tracked regions, so that a memoized layout only depends on the answers to
/// the questions it asked instead of on the exact regions.
#[derive(Copy, Clone)]
pub struct Regions<'a> {
    /// Whether elements should expand to fill the regions instead of shrinking
    /// to fit the content.
    pub expand: Axes<bool>,
    /// The width of all regions.
    width: Abs,
    /// Where the heights come from.
    kind: Kind<'a>,
}

/// Where the heights of [`Regions`] come from.
#[derive(Copy, Clone)]
enum Kind<'a> {
    /// The heights are known.
    Explicit {
        /// The remaining height of the first region.
        height: Abs,
        /// The full height of the first region for relative sizing.
        full: Abs,
        /// The followup regions.
        future: Future<'a>,
    },
    /// The heights are derived from other regions, which are asked about them.
    Derived(Derived<'a>),
}

/// Regions whose heights are derived from other regions.
///
/// The remaining height of the first region is the height of the `index`-th
/// outer region, reduced by `shift` and clamped between `lower` and `upper`.
/// This represents any sequence of the operations on the first region
/// ([`consume`](Regions::consume), [`limit`](Regions::limit), and so on). The
/// heights of all other regions are reduced by `inset`.
#[derive(Copy, Clone)]
struct Derived<'a> {
    /// The regions the heights are derived from.
    outer: &'a (dyn Outer + 'a),
    /// The index of the outer region that is the first region.
    index: usize,
    /// How much the remaining height of the first region is reduced by.
    shift: Abs,
    /// A lower bound for the remaining height of the first region.
    lower: Abs,
    /// An upper bound for the remaining height of the first region.
    upper: Abs,
    /// How much the heights of all regions are reduced by.
    inset: Abs,
    /// The full height of the first region. It doesn't change when content
    /// shifts, so it is read once instead of being asked about repeatedly.
    full: Abs,
    /// The followup regions, if they are set explicitly.
    future: Option<Future<'a>>,
}

/// Explicitly known followup regions.
#[derive(Copy, Clone, Hash)]
struct Future<'a> {
    /// The heights of followup regions.
    backlog: &'a [Abs],
    /// The height of the final region that is repeated once the backlog is
    /// drained.
    last: Option<Abs>,
    /// The remaining heights of the first repetitions of the final region,
    /// if they are predicted to have less space. They are still repetitions,
    /// which matters for [`may_progress`](Regions::may_progress).
    predicted: &'a [Abs],
}

impl Future<'_> {
    /// What kind of region there is `i > 0` breaks after the first one.
    fn slot(&self, i: usize) -> Slot {
        if i <= self.backlog.len() {
            Slot::Finite
        } else if self.last.is_some() {
            Slot::Repeat
        } else {
            Slot::End
        }
    }

    /// The full height of region `i > 0`, if there is one.
    fn full(&self, i: usize) -> Option<Abs> {
        self.backlog.get(i - 1).copied().or(self.last)
    }

    /// The remaining height of region `i > 0`, if there is one.
    fn height(&self, i: usize) -> Option<Abs> {
        let full = self.full(i)?;
        Some(match i.checked_sub(self.backlog.len() + 1) {
            Some(j) => self.predicted.get(j).copied().unwrap_or(full),
            None => full,
        })
    }

    /// The number of regions, including the first one, after which all
    /// regions are plain repetitions of the final region.
    fn len(&self) -> usize {
        1 + self.backlog.len() + self.predicted.len()
    }

    /// Advances to the next region, returning its remaining and full height.
    fn advance(&mut self) -> Option<(Abs, Abs)> {
        let next = (self.height(1)?, self.full(1)?);
        if let Some((_, tail)) = self.backlog.split_first() {
            self.backlog = tail;
        } else if let Some((_, tail)) = self.predicted.split_first() {
            self.predicted = tail;
        }
        Some(next)
    }
}

/// How to find out about a region's remaining height.
enum Lookup<'a> {
    /// The height is known.
    Known(Abs),
    /// There is no such region.
    End,
    /// The height is derived from other regions.
    Derived(Derived<'a>),
}

/// What kind of region there is at some index.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Slot {
    /// A region that is part of the finite sequence of regions. The first
    /// region is always of this kind.
    Finite,
    /// The final region, which is repeated indefinitely.
    Repeat,
    /// There is no region.
    End,
}

/// An owned copy of the regions after the first one of some [`Regions`].
///
/// Obtained with [`Regions::followup`] and applied to a first region with
/// [`Regions::with_followup`], which keeps the kinds of the regions.
#[derive(Debug, Clone, Default, PartialEq, Hash)]
pub struct Followup {
    /// The heights of the followup regions before the final, repeated one.
    pub backlog: Vec<Abs>,
    /// The remaining heights of the first repetitions of the final region
    /// that are predicted to have less space (see
    /// [`Regions::with_predicted`]).
    pub predicted: Vec<Abs>,
    /// The height of the final region that is repeated once the backlog and
    /// the predictions are drained.
    pub last: Option<Abs>,
}

impl Followup {
    /// The same regions with all heights mapped with `f`.
    pub fn map(mut self, mut f: impl FnMut(Abs) -> Abs) -> Self {
        for height in self.backlog.iter_mut().chain(&mut self.predicted) {
            *height = f(*height);
        }
        self.last = self.last.map(f);
        self
    }

    /// The same regions, preceded by regions with the given heights.
    pub fn prepend(mut self, heights: impl IntoIterator<Item = Abs>) -> Self {
        self.backlog.splice(0..0, heights);
        self
    }
}

impl<'a> Regions<'a> {
    /// Create a new sequence of regions from the remaining size of the first
    /// region, its full height, the heights of the followup regions, and the
    /// height of the final region that is repeated indefinitely, if any.
    pub fn new(
        size: Size,
        full: Abs,
        backlog: &'a [Abs],
        last: Option<Abs>,
        expand: Axes<bool>,
    ) -> Self {
        let future = Future { backlog, last, predicted: &[] };
        Self {
            expand,
            width: size.x,
            kind: Kind::Explicit { height: size.y, full, future },
        }
    }

    /// Create a new sequence of same-size regions that repeats indefinitely.
    pub fn repeat(size: Size, expand: Axes<bool>) -> Self {
        Self::new(size, size.y, &[], Some(size.y), expand)
    }

    /// Create regions that are the linked, tracked regions.
    pub fn link<'r: 'a>(link: &'a RegionsLink<'a, 'r>) -> Self {
        // The properties that don't change when content shifts are asked
        // about in a single question.
        let (width, expand, full) = link.0.tracked_base();
        Self {
            expand,
            width,
            kind: Kind::Derived(Derived {
                outer: link,
                index: 0,
                shift: Abs::zero(),
                lower: -Abs::inf(),
                upper: Abs::inf(),
                inset: Abs::zero(),
                full,
                future: None,
            }),
        }
    }

    /// The width of the regions.
    pub fn width(&self) -> Abs {
        self.width
    }

    /// The full height of the first region, for relative sizing.
    pub fn full(&self) -> Abs {
        self.full_at(0)
    }

    /// The base size, which doesn't take into account that the regions is
    /// already partially used up.
    ///
    /// This is also used for relative sizing.
    pub fn base(&self) -> Size {
        Size::new(self.width, self.full())
    }

    /// Whether content of the given height fits into the remaining space of
    /// the first region.
    pub fn fits(&self, height: Abs) -> bool {
        self.fits_at(0, height)
    }

    /// The given height, limited to the remaining height of the first region.
    ///
    /// Reveals the remaining height only if it is smaller than the given one.
    pub fn limited(&self, height: Abs) -> Abs {
        self.below_at(0, height).unwrap_or(height)
    }

    /// The size of content that used the given size in the first region: The
    /// region's size on the axes that `expand`, otherwise the used size,
    /// limited to the region's size.
    ///
    /// Reveals the remaining height only if necessary.
    pub fn fit(&self, used: Size, expand: Axes<bool>) -> Size {
        Size::new(
            if expand.x { self.width } else { used.x.min(self.width) },
            if expand.y { self.height() } else { self.limited(used.y) },
        )
    }

    /// Whether content of the given height fits into the remaining height of
    /// the region after the first one, if there is one. That region's
    /// remaining height is its full height unless it is predicted to have
    /// less space (see [`with_predicted`](Self::with_predicted)).
    pub fn fits_next(&self, height: Abs) -> bool {
        self.fits_at(1, height)
    }

    /// Whether the remaining height of the first region is finite.
    pub fn is_finite(&self) -> bool {
        self.finite_at(0)
    }

    /// Whether the first region is full and a region break is called for.
    pub fn is_full(&self) -> bool {
        self.fits_into_at(0, Abs::zero()) && self.may_progress()
    }

    /// Whether a region break is permitted.
    pub fn may_break(&self) -> bool {
        self.slot_at(1) != Slot::End
    }

    /// Whether there is a region `index` breaks after the first one.
    pub fn has_region(&self, index: usize) -> bool {
        index == 0 || self.slot_at(index) != Slot::End
    }

    /// Whether there are followup regions before the final, repeated one.
    pub fn has_backlog(&self) -> bool {
        self.slot_at(1) == Slot::Finite
    }

    /// Whether calling `next()` may improve a situation where there is a lack
    /// of space.
    pub fn may_progress(&self) -> bool {
        self.may_progress_at(0, Abs::zero())
    }

    /// The remaining height of the first region.
    ///
    /// This is a blunt question: Prefer [`fits`](Self::fits) and the like
    /// where possible.
    pub fn height(&self) -> Abs {
        self.height_at(0)
    }

    /// The remaining size of the first region.
    ///
    /// This is a blunt question: Prefer [`fits`](Self::fits) and the like
    /// where possible.
    pub fn size(&self) -> Size {
        Size::new(self.width, self.height())
    }

    /// The heights of the followup regions before the final, repeated one.
    ///
    /// This includes the first repetitions of the final region if they are
    /// predicted to have less space (see
    /// [`with_predicted`](Self::with_predicted)), so that regions recreated
    /// from the backlog and [`last`](Self::last) have the right heights. The
    /// number of such repetitions at the end of the backlog is given by
    /// [`predicted`](Self::predicted).
    ///
    /// This is a blunt question: Prefer [`may_break`](Self::may_break) and
    /// the like where possible.
    pub fn backlog(&self) -> impl Iterator<Item = Abs> + '_ {
        (1..self.len_at(0)).map(|i| self.height_at(i))
    }

    /// The height of the final region that is repeated once the backlog is
    /// drained.
    ///
    /// This is a blunt question: Prefer [`may_break`](Self::may_break) and
    /// the like where possible.
    pub fn last(&self) -> Option<Abs> {
        match self.kind {
            Kind::Explicit { future, .. } => future.last,
            Kind::Derived(d) => match d.future {
                Some(future) => future.last,
                None => d.outer.last().map(|last| last - d.inset),
            },
        }
    }

    /// How many of the regions at the end of the [`backlog`](Self::backlog)
    /// are repetitions of the final region that are predicted to have less
    /// space (see [`with_predicted`](Self::with_predicted)).
    ///
    /// This is a blunt question.
    pub fn predicted(&self) -> usize {
        (1..self.len_at(0))
            .filter(|&i| self.slot_at(i) == Slot::Repeat)
            .count()
    }

    /// An owned copy of the regions after the first one.
    ///
    /// This is a blunt question.
    pub fn followup(&self) -> Followup {
        let mut backlog: Vec<_> = self.backlog().collect();
        let predicted = backlog.split_off(backlog.len() - self.predicted());
        Followup { backlog, predicted, last: self.last() }
    }

    /// The sizes of the first and all following regions, equivalently to what
    /// would be produced by calling [`next()`](Self::next) repeatedly until
    /// all regions are exhausted. This iterator may be infinite.
    ///
    /// This is a blunt question.
    pub fn iter(&self) -> impl Iterator<Item = Size> + '_ {
        let first = std::iter::once(self.size());
        let last = self.last();
        let rest = self.backlog().chain(last.into_iter().cycle());
        first.chain(rest.map(|h| Size::new(self.width, h)))
    }

    /// The same regions with a different width.
    pub fn with_width(self, width: Abs) -> Self {
        Self { width, ..self }
    }

    /// The same regions with different expansion.
    pub fn with_expand(self, expand: Axes<bool>) -> Self {
        Self { expand, ..self }
    }

    /// The same regions with a different full height of the first region.
    pub fn with_full(mut self, full: Abs) -> Self {
        match &mut self.kind {
            Kind::Explicit { full: f, .. } => *f = full,
            Kind::Derived(d) => d.full = full,
        }
        self
    }

    /// The same first region with different followup regions.
    pub fn with_future<'b>(self, backlog: &'b [Abs], last: Option<Abs>) -> Regions<'b>
    where
        'a: 'b,
    {
        self.with_predicted(backlog, last, &[])
    }

    /// The same first region with different followup regions, of which the
    /// first repetitions of the final region are predicted to have less
    /// space: Their remaining heights are `predicted`.
    ///
    /// Unlike with more `backlog` regions, the predicted regions remain
    /// repetitions, so moving on to them only makes progress if they differ
    /// from the final region (see [`may_progress`](Self::may_progress)).
    pub fn with_predicted<'b>(
        self,
        backlog: &'b [Abs],
        last: Option<Abs>,
        predicted: &'b [Abs],
    ) -> Regions<'b>
    where
        'a: 'b,
    {
        // Predictions that don't differ from the final region are none.
        let mut predicted = predicted;
        while let [rest @ .., p] = predicted
            && Some(*p) == last
        {
            predicted = rest;
        }

        let future = Future { backlog, last, predicted };
        let kind = match self.kind {
            Kind::Explicit { height, full, .. } => {
                Kind::Explicit { height, full, future }
            }
            Kind::Derived(d) => Kind::Derived(Derived { future: Some(future), ..d }),
        };
        Regions { kind, ..self }
    }

    /// The same first region, followed by the given regions instead of the
    /// ones after it.
    pub fn with_followup<'b>(self, followup: &'b Followup) -> Regions<'b>
    where
        'a: 'b,
    {
        self.with_predicted(&followup.backlog, followup.last, &followup.predicted)
    }

    /// Uses up the given height of the first region.
    pub fn consume(&mut self, height: Abs) {
        match &mut self.kind {
            Kind::Explicit { height: h, .. } => *h -= height,
            Kind::Derived(d) => {
                d.shift += height;
                d.lower -= height;
                d.upper -= height;
            }
        }
    }

    /// Limits the remaining height of the first region to at most the given
    /// height.
    pub fn limit(&mut self, height: Abs) {
        match &mut self.kind {
            Kind::Explicit { height: h, .. } => h.set_min(height),
            Kind::Derived(d) => {
                d.lower.set_min(height);
                d.upper.set_min(height);
            }
        }
    }

    /// Raises the remaining height of the first region to at least the given
    /// height.
    pub fn at_least(&mut self, height: Abs) {
        match &mut self.kind {
            Kind::Explicit { height: h, .. } => h.set_max(height),
            Kind::Derived(d) => {
                d.lower.set_max(height);
                d.upper.set_max(height);
            }
        }
    }

    /// Create new regions where all sizes are shrunk by an inset, relative to
    /// the respective size.
    pub fn shrink<'v>(
        &self,
        inset: &Sides<Rel<Abs>>,
        buf: &'v mut Followup,
    ) -> Regions<'v>
    where
        'a: 'v,
    {
        let summed = inset.sum_by_axis();
        match self.kind {
            // Absolute vertical insets can be applied without knowing the
            // heights.
            Kind::Derived(d) if summed.y.rel.is_zero() && d.future.is_none() => {
                let amount = summed.y.abs;
                let width = self.width - summed.x.relative_to(self.width);
                Regions {
                    expand: self.expand,
                    width,
                    kind: Kind::Derived(Derived {
                        shift: d.shift + amount,
                        lower: d.lower - amount,
                        upper: d.upper - amount,
                        inset: d.inset + amount,
                        full: d.full - amount,
                        ..d
                    }),
                }
            }
            _ => self.map(buf, |size| size - summed.relative_to(size)),
        }
    }

    /// The same regions with explicitly known heights.
    ///
    /// This is a blunt question. It is useful for layout that does exact
    /// arithmetic with the heights, since derived regions only reproduce the
    /// heights up to floating point precision.
    pub fn materialize<'v>(&self, buf: &'v mut Followup) -> Regions<'v>
    where
        'a: 'v,
    {
        match self.kind {
            Kind::Explicit { .. } => *self,
            Kind::Derived(_) => self.map(buf, |size| size),
        }
    }

    /// Create new regions where all sizes are mapped with `f`.
    ///
    /// Note that since all regions must have the same width, the width returned
    /// by `f` is ignored for the backlog and the final region.
    ///
    /// This is a blunt question.
    pub fn map<'v, F>(&self, buf: &'v mut Followup, mut f: F) -> Regions<'v>
    where
        F: FnMut(Size) -> Size,
    {
        let x = self.width;
        *buf = self.followup().map(|y| f(Size::new(x, y)).y);
        let full = f(Size::new(x, self.full())).y;
        Regions::new(f(self.size()), full, &[], None, self.expand).with_followup(buf)
    }

    /// Advance to the next region if there is any.
    pub fn next(&mut self) {
        match &mut self.kind {
            Kind::Explicit { height, full, future } => {
                if let Some((h, f)) = future.advance() {
                    *height = h;
                    *full = f;
                }
            }
            Kind::Derived(d) => {
                if let Some(mut future) = d.future {
                    // Once past the first region, regions with explicitly set
                    // followup regions are fully explicit.
                    if let Some((height, full)) = future.advance() {
                        self.kind = Kind::Explicit { height, full, future };
                    }
                } else if d.outer.slot(d.index + 1) != Slot::End {
                    d.index += 1;
                    d.shift = d.inset;
                    d.lower = -Abs::inf();
                    d.upper = Abs::inf();
                    d.full = d.outer.full(d.index) - d.inset;
                }
            }
        }
    }

    /// What kind of region there is `i` breaks after the first one.
    fn slot_at(&self, i: usize) -> Slot {
        if i == 0 {
            return Slot::Finite;
        }
        match self.kind {
            Kind::Explicit { future, .. } => future.slot(i),
            Kind::Derived(d) => match d.future {
                Some(future) => future.slot(i),
                None => d.outer.slot(d.index + i),
            },
        }
    }

    /// How to find out about the remaining height of the region `i` breaks
    /// after the first one.
    fn lookup(&self, i: usize) -> Lookup<'a> {
        let future = match self.kind {
            Kind::Explicit { height, .. } if i == 0 => return Lookup::Known(height),
            Kind::Explicit { future, .. } => future,
            Kind::Derived(d) => match d.future {
                Some(future) if i > 0 => future,
                _ => return Lookup::Derived(d),
            },
        };
        future.height(i).map_or(Lookup::End, Lookup::Known)
    }

    /// Whether content of the given height fits into region `i`.
    fn fits_at(&self, i: usize, height: Abs) -> bool {
        let d = match self.lookup(i) {
            Lookup::Known(h) => return h.fits(height),
            Lookup::End => return false,
            Lookup::Derived(d) => d,
        };
        if i > 0 {
            return d.outer.fits(d.index + i, height + d.inset);
        }
        if d.lower.fits(height) {
            true
        } else if !d.upper.fits(height) {
            false
        } else {
            d.outer.fits(d.index, d.shift + height)
        }
    }

    /// Whether the height of region `i` fits into the given height.
    fn fits_into_at(&self, i: usize, height: Abs) -> bool {
        let d = match self.lookup(i) {
            Lookup::Known(h) => return height.fits(h),
            Lookup::End => return false,
            Lookup::Derived(d) => d,
        };
        if i > 0 {
            return d.outer.fits_into(d.index + i, height + d.inset);
        }
        if height.fits(d.upper) {
            true
        } else if !height.fits(d.lower) {
            false
        } else {
            d.outer.fits_into(d.index, d.shift + height)
        }
    }

    /// The height of region `i` if it is smaller than the given height.
    fn below_at(&self, i: usize, height: Abs) -> Option<Abs> {
        let d = match self.lookup(i) {
            Lookup::Known(h) => return (h < height).then_some(h),
            Lookup::End => return None,
            Lookup::Derived(d) => d,
        };
        if i > 0 {
            return d.outer.below(d.index + i, height + d.inset).map(|h| h - d.inset);
        }
        if d.lower >= height {
            return None;
        }
        // The remaining height is the clamped height, so the result is the
        // clamped minimum of the given height and the unclamped height.
        let unclamped = match d.outer.below(d.index, d.shift + height) {
            Some(h) => h - d.shift,
            None => height,
        };
        let clamped = unclamped.max(d.lower.min(height)).min(d.upper.min(height));
        (clamped < height).then_some(clamped)
    }

    /// The (remaining) height of region `i`.
    fn height_at(&self, i: usize) -> Abs {
        let d = match self.lookup(i) {
            Lookup::Known(h) => return h,
            Lookup::End => return Abs::zero(),
            Lookup::Derived(d) => d,
        };
        if i > 0 {
            return d.outer.height(d.index + i) - d.inset;
        }
        if d.lower == d.upper {
            return d.lower;
        }
        (d.outer.height(d.index) - d.shift).max(d.lower).min(d.upper)
    }

    /// The full height of region `i`.
    fn full_at(&self, i: usize) -> Abs {
        match self.kind {
            Kind::Explicit { full, .. } if i == 0 => full,
            Kind::Explicit { future, .. } => future.full(i).unwrap_or_default(),
            Kind::Derived(d) => match (i, d.future) {
                (0, _) => d.full,
                (1.., Some(future)) => future.full(i).unwrap_or_default(),
                _ => d.outer.full(d.index + i) - d.inset,
            },
        }
    }

    /// Whether the (remaining) height of region `i` is finite.
    fn finite_at(&self, i: usize) -> bool {
        let d = match self.lookup(i) {
            Lookup::Known(h) => return h.is_finite(),
            Lookup::End => return true,
            Lookup::Derived(d) => d,
        };
        if i == 0 && d.upper.is_finite() {
            return true;
        }
        if i == 0 && d.lower == d.upper {
            return d.lower.is_finite();
        }
        d.outer.finite(d.index + i)
    }

    /// Whether advancing past region `i` after using up the given height of it
    /// may improve a situation where there is a lack of space.
    fn may_progress_at(&self, i: usize, used: Abs) -> bool {
        if let Kind::Derived(d) = self.kind
            && d.future.is_none()
            && (i > 0 || (d.lower == -Abs::inf() && d.upper == Abs::inf()))
        {
            let used = if i == 0 { d.shift - d.inset + used } else { used };
            return d.outer.may_progress(d.index + i, used);
        }
        match self.slot_at(i + 1) {
            Slot::Finite => true,
            // Predictions of the repeated region don't count: Moving on only
            // helps if there is less space than in a fresh final region.
            Slot::Repeat => {
                self.last().is_some_and(|last| self.height_at(i) - used != last)
            }
            Slot::End => false,
        }
    }

    /// The number of regions, starting at region `i`, after which all regions
    /// are plain repetitions of the final region.
    fn len_at(&self, i: usize) -> usize {
        match self.kind {
            Kind::Explicit { future, .. } => future.len().saturating_sub(i).max(1),
            Kind::Derived(d) => match d.future {
                Some(future) => future.len().saturating_sub(i).max(1),
                None => d.outer.len(d.index + i),
            },
        }
    }
}

/// The questions that can be asked about [`Regions`] with tracking. See
/// [`Regions`] for what the questions mean. Most take the index of the region
/// they are about.
#[comemo::track]
#[expect(clippy::elidable_lifetime_names, reason = "required for `comemo::track`")]
impl<'a> Regions<'a> {
    /// The width, the expansion, and the full height of the first region,
    /// bundled into one question since they are always needed together.
    fn tracked_base(&self) -> (Abs, Axes<bool>, Abs) {
        (self.width, self.expand, self.full())
    }

    fn tracked_slot(&self, i: usize) -> Slot {
        self.slot_at(i)
    }

    fn tracked_fits(&self, i: usize, height: Abs) -> bool {
        self.fits_at(i, height)
    }

    fn tracked_fits_into(&self, i: usize, height: Abs) -> bool {
        self.fits_into_at(i, height)
    }

    fn tracked_below(&self, i: usize, height: Abs) -> Option<Abs> {
        self.below_at(i, height)
    }

    fn tracked_height(&self, i: usize) -> Abs {
        self.height_at(i)
    }

    fn tracked_full(&self, i: usize) -> Abs {
        self.full_at(i)
    }

    fn tracked_finite(&self, i: usize) -> bool {
        self.finite_at(i)
    }

    fn tracked_may_progress(&self, i: usize, used: Abs) -> bool {
        self.may_progress_at(i, used)
    }

    fn tracked_last(&self) -> Option<Abs> {
        self.last()
    }

    fn tracked_len(&self, i: usize) -> usize {
        self.len_at(i)
    }
}

/// A link to tracked regions, from which other regions can derive their
/// heights with [`Regions::link`].
pub struct RegionsLink<'a, 'r>(Tracked<'a, Regions<'r>>);

impl<'a, 'r> RegionsLink<'a, 'r> {
    /// Create a link to tracked regions.
    pub fn new(regions: Tracked<'a, Regions<'r>>) -> Self {
        Self(regions)
    }
}

/// Regions that derived regions can ask questions. Erases the lifetime
/// invariance of [`Tracked`].
trait Outer {
    fn slot(&self, i: usize) -> Slot;
    fn fits(&self, i: usize, height: Abs) -> bool;
    fn fits_into(&self, i: usize, height: Abs) -> bool;
    fn below(&self, i: usize, height: Abs) -> Option<Abs>;
    fn height(&self, i: usize) -> Abs;
    fn full(&self, i: usize) -> Abs;
    fn finite(&self, i: usize) -> bool;
    fn may_progress(&self, i: usize, used: Abs) -> bool;
    fn last(&self) -> Option<Abs>;
    fn len(&self, i: usize) -> usize;
}

impl Outer for RegionsLink<'_, '_> {
    fn slot(&self, i: usize) -> Slot {
        self.0.tracked_slot(i)
    }

    fn fits(&self, i: usize, height: Abs) -> bool {
        self.0.tracked_fits(i, height)
    }

    fn fits_into(&self, i: usize, height: Abs) -> bool {
        self.0.tracked_fits_into(i, height)
    }

    fn below(&self, i: usize, height: Abs) -> Option<Abs> {
        self.0.tracked_below(i, height)
    }

    fn height(&self, i: usize) -> Abs {
        self.0.tracked_height(i)
    }

    fn full(&self, i: usize) -> Abs {
        self.0.tracked_full(i)
    }

    fn finite(&self, i: usize) -> bool {
        self.0.tracked_finite(i)
    }

    fn may_progress(&self, i: usize, used: Abs) -> bool {
        self.0.tracked_may_progress(i, used)
    }

    fn last(&self) -> Option<Abs> {
        self.0.tracked_last()
    }

    fn len(&self, i: usize) -> usize {
        self.0.tracked_len(i)
    }
}

/// Regions are hashed by their heights, for example to be passed to a memoized
/// function. For derived regions, that's a blunt question about all of them
/// (see [`materialize`](Regions::materialize)). Tracking them instead only
/// asks the questions that the memoized function asks.
impl Hash for Regions<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Kind::Explicit { height, full, future } = self.kind else {
            let mut buf = Followup::default();
            self.materialize(&mut buf).hash(state);
            return;
        };
        self.expand.hash(state);
        self.width.hash(state);
        height.hash(state);
        full.hash(state);
        future.hash(state);
    }
}

impl Debug for Regions<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind {
            Kind::Explicit { height, future, .. } => {
                f.write_str("Regions ")?;
                let mut list = f.debug_list();
                let mut prev = height;
                list.entry(&Size::new(self.width, height));
                for &h in future.backlog.iter().chain(future.predicted) {
                    list.entry(&Size::new(self.width, h));
                    prev = h;
                }
                if let Some(last) = future.last {
                    if last != prev {
                        list.entry(&Size::new(self.width, last));
                    }
                    list.entry(&(..));
                }
                list.finish()
            }
            Kind::Derived(d) => f
                .debug_struct("Regions")
                .field("width", &self.width)
                .field("index", &d.index)
                .field("shift", &d.shift)
                .field("lower", &d.lower)
                .field("upper", &d.upper)
                .field("inset", &d.inset)
                .finish_non_exhaustive(),
        }
    }
}

#[cfg(test)]
mod tests {
    use comemo::Track;

    use super::*;

    const EXPAND: Axes<bool> = Axes { x: false, y: false };

    fn pt(v: f64) -> Abs {
        Abs::pt(v)
    }

    #[test]
    fn test_regions_end() {
        // A single region: There is nothing after it.
        let regions = Regions::new(Size::splat(pt(10.0)), pt(10.0), &[], None, EXPAND);
        assert_eq!(regions.slot_at(1), Slot::End);
        assert!(!regions.fits_at(1, Abs::zero()));
        assert!(!regions.fits_into_at(1, pt(5.0)));
        assert_eq!(regions.below_at(1, pt(5.0)), None);
        assert_eq!(regions.height_at(1), Abs::zero());
        assert!(!regions.fits_next(Abs::zero()));
        assert!(!regions.may_progress());

        // The same when derived from tracked regions.
        let link = RegionsLink::new(regions.track());
        let derived = Regions::link(&link);
        assert!(!derived.fits_at(1, Abs::zero()));
        assert_eq!(derived.below_at(1, pt(5.0)), None);
        assert!(!derived.may_progress());
    }

    #[test]
    fn test_regions_predicted() {
        let size = Size::splat(pt(100.0));
        let predicted = [pt(100.0), pt(40.0), pt(100.0)];
        let regions = Regions::repeat(size, EXPAND).with_predicted(
            &[],
            Some(pt(100.0)),
            &predicted,
        );

        // Predicted repetitions have their predicted heights, but remain
        // repetitions of the final region.
        assert_eq!(regions.height_at(2), pt(40.0));
        assert_eq!(regions.full_at(2), pt(100.0));
        assert_eq!(regions.slot_at(1), Slot::Repeat);
        assert!(!regions.has_backlog());

        // Moving on from a fresh repetition makes no progress, even if a
        // later one is predicted to be smaller. Moving on from a smaller one
        // does.
        assert!(!regions.may_progress());
        assert!(!regions.may_progress_at(1, Abs::zero()));
        assert!(regions.may_progress_at(2, Abs::zero()));

        // The trailing prediction that doesn't differ is dropped. The others
        // are part of the blunt backlog.
        assert_eq!(regions.backlog().collect::<Vec<_>>(), [pt(100.0), pt(40.0)]);
        assert_eq!(regions.predicted(), 2);

        // Advancing consumes the predictions.
        let mut next = regions;
        next.next();
        next.next();
        assert_eq!(next.height(), pt(40.0));
        assert_eq!(next.full(), pt(100.0));
        next.next();
        assert_eq!(next.height(), pt(100.0));
        assert_eq!(next.predicted(), 0);

        // Derived regions ask about the predictions.
        let link = RegionsLink::new(regions.track());
        let derived = Regions::link(&link);
        assert_eq!(derived.height_at(2), pt(40.0));
        assert!(!derived.may_progress());
        assert_eq!(derived.backlog().collect::<Vec<_>>(), [pt(100.0), pt(40.0)]);
        assert_eq!(derived.predicted(), 2);
    }

    #[test]
    fn test_regions_followup() {
        let size = Size::splat(pt(100.0));
        let backlog = [pt(80.0)];
        let predicted = [pt(40.0), pt(100.0), pt(60.0)];
        let regions = Regions::new(size, pt(100.0), &backlog, Some(pt(100.0)), EXPAND)
            .with_predicted(&backlog, Some(pt(100.0)), &predicted);

        // The followup regions keep their kinds.
        let followup = regions.followup();
        assert_eq!(followup.backlog, [pt(80.0)]);
        assert_eq!(followup.predicted, [pt(40.0), pt(100.0), pt(60.0)]);
        assert_eq!(followup.last, Some(pt(100.0)));

        // Applying them to another first region reproduces the regions after
        // it, including whether moving on to them counts as progress.
        let other = Regions::new(Size::splat(pt(50.0)), pt(50.0), &[], None, EXPAND)
            .with_followup(&followup);
        for i in 1..6 {
            assert_eq!(other.height_at(i), regions.height_at(i));
            assert_eq!(other.full_at(i), regions.full_at(i));
            assert_eq!(other.slot_at(i), regions.slot_at(i));
            assert_eq!(
                other.may_progress_at(i, Abs::zero()),
                regions.may_progress_at(i, Abs::zero()),
            );
        }

        // Mapping and prepending.
        let mapped = followup.clone().map(|h| h - pt(10.0)).prepend([pt(5.0)]);
        assert_eq!(mapped.backlog, [pt(5.0), pt(70.0)]);
        assert_eq!(mapped.predicted, [pt(30.0), pt(90.0), pt(50.0)]);
        assert_eq!(mapped.last, Some(pt(90.0)));
    }

    #[test]
    fn test_regions_hash_derived() {
        let predicted = [pt(40.0)];
        let regions = Regions::repeat(Size::splat(pt(100.0)), EXPAND).with_predicted(
            &[],
            Some(pt(100.0)),
            &predicted,
        );

        // Derived regions hash like their materialized counterparts.
        let link = RegionsLink::new(regions.track());
        let derived = Regions::link(&link);
        let mut buf = Followup::default();
        assert_eq!(
            typst_utils::hash128(&derived),
            typst_utils::hash128(&derived.materialize(&mut buf)),
        );
        assert_eq!(typst_utils::hash128(&derived), typst_utils::hash128(&regions));
    }
}
