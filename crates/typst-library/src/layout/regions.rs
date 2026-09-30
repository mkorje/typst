use std::fmt::{self, Debug, Formatter};

use crate::layout::{Abs, Axes, Size};

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
        Regions {
            size: region.size,
            expand: region.expand,
            full: region.size.y,
            backlog: &[],
            last: None,
            predicted: 0,
        }
    }
}

/// A sequence of regions to layout into.
///
/// A *region* is a contiguous rectangular space in which elements
/// can be laid out. All regions within a `Regions` object have the
/// same width, namely `self.size.x`. This means that it is not
/// currently possible to, for instance, have content wrap to the
/// side of a floating element.
#[derive(Copy, Clone, Hash)]
pub struct Regions<'a> {
    /// The remaining size of the first region.
    pub size: Size,
    /// Whether elements should expand to fill the regions instead of shrinking
    /// to fit the content.
    pub expand: Axes<bool>,
    /// The full height of the region for relative sizing.
    pub full: Abs,
    /// The height of followup regions. The width is the same for all regions.
    pub backlog: &'a [Abs],
    /// The height of the final region that is repeated once the backlog is
    /// drained. The width is the same for all regions.
    pub last: Option<Abs>,
    /// How many regions at the end of the backlog are repetitions of the final
    /// region that are predicted to have less space. Their full height is
    /// that of the final region. Unlike other backlog regions, moving on to
    /// them only makes progress if the current region has less space than a
    /// fresh final region (see [`may_progress`](Self::may_progress)).
    pub predicted: usize,
}

impl Regions<'_> {
    /// Create a new sequence of same-size regions that repeats indefinitely.
    pub fn repeat(size: Size, expand: Axes<bool>) -> Self {
        Self {
            size,
            full: size.y,
            backlog: &[],
            last: Some(size.y),
            predicted: 0,
            expand,
        }
    }

    /// The base size, which doesn't take into account that the regions is
    /// already partially used up.
    ///
    /// This is also used for relative sizing.
    pub fn base(&self) -> Size {
        Size::new(self.size.x, self.full)
    }

    /// Create new regions where all sizes are mapped with `f`.
    ///
    /// Note that since all regions must have the same width, the width returned
    /// by `f` is ignored for the backlog and the final region.
    pub fn map<'v, F>(&self, backlog: &'v mut Vec<Abs>, mut f: F) -> Regions<'v>
    where
        F: FnMut(Size) -> Size,
    {
        let x = self.size.x;
        backlog.clear();
        backlog.extend(self.backlog.iter().map(|&y| f(Size::new(x, y)).y));
        Regions {
            size: f(self.size),
            full: f(Size::new(x, self.full)).y,
            backlog,
            last: self.last.map(|y| f(Size::new(x, y)).y),
            predicted: self.predicted,
            expand: self.expand,
        }
    }

    /// Whether the first region is full and a region break is called for.
    pub fn is_full(&self) -> bool {
        Abs::zero().fits(self.size.y) && self.may_progress()
    }

    /// Whether a region break is permitted.
    pub fn may_break(&self) -> bool {
        !self.backlog.is_empty() || self.last.is_some()
    }

    /// Whether there are followup regions before the final, repeated one,
    /// not counting its predicted repetitions.
    pub fn has_backlog(&self) -> bool {
        self.backlog.len() > self.predicted
    }

    /// Whether calling `next()` may improve a situation where there is a lack
    /// of space.
    pub fn may_progress(&self) -> bool {
        self.has_backlog()
            || self.last.is_some_and(|height| !self.size.y.approx_eq(height))
    }

    /// Drops the predicted repetitions of the final region at the end of the
    /// backlog that don't have less space than it, since they are no
    /// predictions.
    pub fn trim_predicted(&mut self) {
        while self.predicted > 0
            && let [rest @ .., height] = self.backlog
            && Some(*height) == self.last
        {
            self.backlog = rest;
            self.predicted -= 1;
        }
    }

    /// Advance to the next region if there is any.
    pub fn next(&mut self) {
        if let Some((&height, tail)) = self.backlog.split_first() {
            self.full = if self.has_backlog() {
                height
            } else {
                self.predicted -= 1;
                self.last.unwrap_or(height)
            };
            self.backlog = tail;
            self.size.y = height;
        } else if let Some(height) = self.last {
            self.size.y = height;
            self.full = height;
        }
    }

    /// An iterator that returns the sizes of the first and all following
    /// regions, equivalently to what would be produced by calling
    /// [`next()`](Self::next) repeatedly until all regions are exhausted.
    /// This iterator may be infinite.
    pub fn iter(&self) -> impl Iterator<Item = Size> + '_ {
        let first = std::iter::once(self.size);
        let backlog = self.backlog.iter();
        let last = self.last.iter().cycle();
        first.chain(backlog.chain(last).map(|&h| Size::new(self.size.x, h)))
    }
}

impl Debug for Regions<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Regions ")?;
        let mut list = f.debug_list();
        let mut prev = self.size.y;
        list.entry(&self.size);
        for &height in self.backlog {
            list.entry(&Size::new(self.size.x, height));
            prev = height;
        }
        if let Some(last) = self.last {
            if last != prev {
                list.entry(&Size::new(self.size.x, last));
            }
            list.entry(&(..));
        }
        list.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pt(v: f64) -> Abs {
        Abs::pt(v)
    }

    #[test]
    fn test_regions_predicted() {
        let backlog = [pt(80.0), pt(100.0), pt(40.0), pt(100.0)];
        let mut regions = Regions {
            predicted: 3,
            last: Some(pt(100.0)),
            backlog: &backlog,
            ..Regions::repeat(Size::splat(pt(100.0)), Axes::splat(false))
        };

        // The trailing prediction that doesn't differ is dropped.
        regions.trim_predicted();
        assert_eq!(regions.backlog, [pt(80.0), pt(100.0), pt(40.0)]);
        assert_eq!(regions.predicted, 2);

        // A backlog region follows, so moving on makes progress.
        assert!(regions.has_backlog());
        assert!(regions.may_progress());

        // Moving on from a fresh repetition makes no progress, even if a
        // later one is predicted to be smaller.
        regions.next();
        assert_eq!((regions.size.y, regions.full), (pt(80.0), pt(80.0)));
        assert!(regions.may_progress());
        regions.next();
        assert_eq!((regions.size.y, regions.full), (pt(100.0), pt(100.0)));
        assert!(!regions.has_backlog());
        assert!(!regions.may_progress());

        // Predicted repetitions have their predicted heights and the full
        // height of the final region. Moving on from a smaller one makes
        // progress.
        regions.next();
        assert_eq!((regions.size.y, regions.full), (pt(40.0), pt(100.0)));
        assert_eq!(regions.predicted, 0);
        assert!(regions.may_progress());
        regions.next();
        assert_eq!((regions.size.y, regions.full), (pt(100.0), pt(100.0)));
    }
}
