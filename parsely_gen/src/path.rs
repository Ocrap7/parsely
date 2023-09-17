use std::{borrow::Borrow, sync::Arc};

#[derive(Debug, Clone, Hash, Eq)]
pub struct PathSegment(Arc<str>);

impl Borrow<str> for PathSegment {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl AsRef<str> for PathSegment {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl PathSegment {
    pub fn new(name: &str) -> PathSegment {
        PathSegment(Arc::from(name))
    }
}

impl PartialEq for PathSegment {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<str> for PathSegment {
    fn eq(&self, other: &str) -> bool {
        self.0.as_ref() == other
    }
}

/// Represents an absolute path in the symbol tree
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path(Arc<[PathSegment]>);

impl Path {
    pub fn from_path(path: &parsely_parser::expression::Path) -> Path {
        path.segments
            .iter()
            .map(|seg| PathSegment::new(&seg.value))
            .into()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a PathSegment> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn last(&self) -> &PathSegment {
        self.0.last().unwrap()
    }

    pub fn simple_mangle(&self) -> String {
        self.0.join(".")
    }
}

impl AsRef<[PathSegment]> for Path {
    fn as_ref(&self) -> &[PathSegment] {
        self.0.as_ref()
    }
}

impl<I: Iterator<Item = PathSegment>> From<I> for Path {
    fn from(value: I) -> Self {
        Path(Arc::from_iter(value))
    }
}
