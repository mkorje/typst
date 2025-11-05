/// A processed collection of [`MathItem`]s.
#[derive(Debug, Default, Clone)]
pub struct MathRun<'a>(Vec<MathItem<'a>>);

impl<'a> MathRun<'a> {
    /// Takes the given [`MathItem`]s and do some basic processing.
    pub fn new(items: Vec<MathItem<'a>>) -> MathRun<'a> {
        let iter = items.into_iter().peekable();
        let mut last: Option<usize> = None;
        let mut space: Option<MathItem> = None;
        let mut resolved: Vec<MathItem> = vec![];

        for mut item in iter {
            match item {
                // Keep space only if supported by spaced items.
                MathItem::Space => {
                    if last.is_some() {
                        space = Some(item);
                    }
                    continue;
                }

                // Explicit spacing disables automatic spacing.
                MathItem::Spacing(width, weak) => {
                    last = None;
                    space = None;

                    if weak {
                        match resolved.last_mut() {
                            None => continue,
                            Some(MathItem::Spacing(prev, true)) => {
                                *prev = (*prev).max(width);
                                continue;
                            }
                            Some(_) => {}
                        }
                    }

                    resolved.push(item);
                    continue;
                }

                // Alignment points are resolved later.
                MathItem::Align => {
                    resolved.push(item);
                    continue;
                }

                // New line, new things.
                MathItem::Linebreak => {
                    resolved.push(item);
                    space = None;
                    last = None;
                    continue;
                }

                _ => {}
            }

            // Convert variable operators into binary operators if something
            // precedes them and they are not preceded by a operator or comparator.
            if item.class() == MathClass::Vary
                && matches!(
                    last.map(|i| resolved[i].class()),
                    Some(
                        MathClass::Normal
                            | MathClass::Alphabetic
                            | MathClass::Closing
                            | MathClass::Fence
                    )
                )
            {
                item.set_class(MathClass::Binary);
            }

            // Insert spacing between the last and this non-ignorant item.
            if !item.is_ignorant() {
                if let Some(i) = last
                    && let Some(s) = spacing(&mut resolved[i], space.take(), &mut item)
                {
                    resolved.insert(i + 1, s);
                }

                last = Some(resolved.len());
            }

            resolved.push(item);
        }

        if let Some(MathItem::Spacing(_, true)) = resolved.last() {
            resolved.pop();
        }

        Self(resolved)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, MathItem<'a>> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, MathItem<'a>> {
        self.0.iter_mut()
    }

    pub fn is_multiline(&self) -> bool {
        self.iter().any(|item| matches!(item, MathItem::Linebreak))
    }

    /// Split by linebreaks, and copy [`MathItem`]s into rows.
    pub fn rows(&self) -> Vec<Self> {
        let mut rows: Vec<_> = self
            .0
            .split(|item| matches!(item, MathItem::Linebreak))
            .map(|slice| Self(slice.to_vec()))
            .collect();
        rows.pop_if(|row| row.0.is_empty());
        rows
    }

    pub fn alignments(&self) -> (Vec<Vec<Self>>, usize) {
        let mut max_len = 0;
        let matrix = self
            .rows()
            .into_iter()
            .map(|row| {
                let cells: Vec<_> = row
                    .0
                    .split(|item| matches!(item, MathItem::Align))
                    .map(|slice| Self(slice.to_vec()))
                    .collect();
                max_len = max_len.max(cells.len());
                cells
            })
            .collect();
        (matrix, max_len)
    }

    pub fn row_count(&self) -> usize {
        let mut count =
            1 + self.0.iter().filter(|f| matches!(f, MathItem::Linebreak)).count();

        // A linebreak at the very end does not introduce an extra row.
        if let Some(f) = self.0.last()
            && matches!(f, MathItem::Linebreak)
        {
            count -= 1
        }
        count
    }

    pub fn into_item(&self, styles: StyleChain<'a>) -> MathItem<'a> {
        let mut items = self.0.clone();
        if items.len() == 1 {
            items.pop().unwrap()
        } else {
            GroupItem::new(items, styles).into()
        }
    }

    pub fn class(&self) -> MathClass {
        if self.0.len() == 1 {
            self.0.first().map(|item| item.class()).unwrap_or(MathClass::Normal)
        } else {
            MathClass::Normal
        }
    }
}

/// Create the spacing between two items in a given style.
fn spacing<'a>(
    l: &mut MathItem,
    space: Option<MathItem<'a>>,
    r: &mut MathItem,
) -> Option<MathItem<'a>> {
    use MathClass::*;

    let script = |f: &MathItem| f.size().is_some_and(|s| s <= MathSize::Script);

    let mut l_class = l.class();
    let mut r_class = r.class();
    if let MathItem::Fenced(fenced) = l
        && fenced.close.is_some()
    {
        l_class = Closing;
    } else if let MathItem::Group(group) = l
        && let Some(last) = group.items.0.last()
    {
        l_class = last.class();
    }
    if let MathItem::Fenced(fenced) = r
        && fenced.open.is_some()
    {
        r_class = Opening;
    } else if let MathItem::Group(group) = r
        && let Some(first) = group.items.0.first()
    {
        r_class = first.class();
    }

    match (l_class, r_class) {
        // No spacing before punctuation; thin spacing after punctuation, unless
        // in script size.
        (_, Punctuation) => {}
        (Punctuation, _) if !script(l) => l.set_rspace(Some(THIN)),

        // No spacing after opening delimiters and before closing delimiters.
        (Opening, _) | (_, Closing) => {}

        // Thick spacing around relations, unless followed by a another relation
        // or in script size.
        (Relation, Relation) => {}
        (Relation, _) if !script(l) => l.set_rspace(Some(THICK)),
        (_, Relation) if !script(r) => r.set_lspace(Some(THICK)),

        // Medium spacing around binary operators, unless in script size.
        (Binary, _) if !script(l) => l.set_rspace(Some(MEDIUM)),
        (_, Binary) if !script(r) => r.set_lspace(Some(MEDIUM)),

        // Thin spacing around large operators, unless to the left of
        // an opening delimiter. TeXBook, p170
        (Large, Opening | Fence) => {}
        (Large, _) => l.set_rspace(Some(THIN)),

        (_, Large) => r.set_lspace(Some(THIN)),

        // Spacing around spaced frames.
        _ if (l.is_spaced() || r.is_spaced()) => return space.clone(),

        _ => {}
    };

    return None;
}
