use std::{
    collections::HashMap,
    io,
    num::NonZeroU32,
    path::{Path, PathBuf},
};

use tree_sitter::{Point, Range};

/// A unique identifier for a source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceId(NonZeroU32);

impl SourceId {
    pub fn get(&self) -> u32 {
        self.0.get()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Source {
    File {
        path: PathBuf,
        contents: ariadne::Source,
    },
    Stdin {
        contents: ariadne::Source,
    },
    Memory {
        contents: ariadne::Source,
    },

    None,
}

pub struct SourceManager {
    sources: Vec<Source>,
    free_indices: Vec<SourceId>,
    file_map: HashMap<PathBuf, SourceId>,
}

impl SourceManager {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
            free_indices: Vec::new(),
            file_map: HashMap::new(),
        }
    }

    pub fn remove(&mut self, id: SourceId) {
        match self.sources[id.0.get() as usize - 1] {
            Source::File { ref path, .. } => {
                self.file_map.remove(path);
            }
            _ => (),
        }

        self.sources[id.0.get() as usize - 1] = Source::None;
        self.free_indices.push(id);
    }

    pub fn text_for(&self, id: SourceId) -> Option<&str> {
        self.get_source(id).and_then(|source| match source {
            Source::File { contents, .. } => Some(contents.text()),
            Source::Stdin { contents } => Some(contents.text()),
            Source::Memory { contents } => Some(contents.text()),

            _ => None,
        })
    }
    pub fn open_file(&mut self, path: impl AsRef<Path>) -> io::Result<SourceId> {
        let path = path.as_ref().to_path_buf();
        let contents = std::fs::read_to_string(&path)?;
        let id = self
            .free_indices
            .pop()
            .unwrap_or_else(|| SourceId(NonZeroU32::new(self.sources.len() as u32 + 1).unwrap()));
        self.sources.push(Source::File {
            path: path.clone(),
            contents: ariadne::Source::from(contents),
        });
        self.file_map.insert(path.clone(), id);
        Ok(id)
    }

    pub fn get_source(&self, id: SourceId) -> Option<&Source> {
        self.sources.get(id.0.get() as usize - 1)
    }

    pub fn get_source_by_path(&self, path: impl AsRef<Path>) -> Option<&Source> {
        self.file_map
            .get(path.as_ref())
            .and_then(|id| self.get_source(*id))
    }

    pub fn add_stdin_source(&mut self, contents: String) -> SourceId {
        let id = self
            .free_indices
            .pop()
            .unwrap_or_else(|| SourceId(NonZeroU32::new(self.sources.len() as u32 + 1).unwrap()));
        self.sources.push(Source::Stdin {
            contents: ariadne::Source::from(contents),
        });
        id
    }

    pub fn add_memory_source(&mut self, contents: String) -> SourceId {
        let id = SourceId(NonZeroU32::new(self.sources.len() as u32 + 1).unwrap());
        self.sources.push(Source::Memory {
            contents: ariadne::Source::from(contents),
        });
        id
    }
}

/// A span represents a range of bytes in a source file, along with the corresponding start and end points.
/// It is used to track the location of tokens or other elements in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Optional source identifier, which can be used to associate
    /// the span with a specific file and its contents.
    pub source_id: Option<SourceId>,
    pub start_byte: usize,
    pub end_byte: usize,
}

impl chumsky::span::Span for Span {
    type Offset = usize;
    type Context = Option<SourceId>;

    fn context(&self) -> Self::Context {
        self.source_id
    }

    fn end(&self) -> Self::Offset {
        self.end_byte
    }

    fn start(&self) -> Self::Offset {
        self.start_byte
    }

    fn to_end(&self) -> Self
    where
        Self: Sized,
    {
        Self {
            start_byte: self.end_byte,
            ..*self
        }
    }

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            source_id: context,
            start_byte: range.start,
            end_byte: range.end,
        }
    }

    fn union(&self, other: Self) -> Self
    where
        Self::Context: PartialEq + core::fmt::Debug,
        Self::Offset: Ord,
        Self: Sized,
    {
        if self.source_id != other.source_id {
            panic!(
                "Cannot union spans from different sources: {:?} and {:?}",
                self.source_id, other.source_id
            );
        }

        Self {
            source_id: self.source_id,
            start_byte: self.start_byte.min(other.start_byte),
            end_byte: self.end_byte.max(other.end_byte),
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            source_id: None,
            start_byte: 0,
            end_byte: 0,
        }
    }
}

impl Span {
    pub fn new(
        source_id: Option<SourceId>,
        start_byte: usize,
        end_byte: usize,
        _start_point: Point,
        _end_point: Point,
    ) -> Self {
        Self {
            source_id,
            start_byte,
            end_byte,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.start_byte == self.end_byte
    }

    pub fn length(&self) -> usize {
        (self.end_byte - self.start_byte) as usize
    }

    pub fn contains(&self, other: &Span) -> bool {
        self.source_id == other.source_id
            && self.start_byte <= other.start_byte
            && self.end_byte >= other.end_byte
    }

    pub fn overlaps(&self, other: &Span) -> bool {
        self.source_id == other.source_id
            && self.start_byte < other.end_byte
            && self.end_byte > other.start_byte
    }

    pub fn union_(&self, other: &Span) -> Option<Span> {
        if self.source_id != other.source_id {
            return None;
        }
        Some(Span {
            source_id: self.source_id,
            start_byte: self.start_byte.min(other.start_byte),
            end_byte: self.end_byte.max(other.end_byte),
        })
    }

    pub fn intersection(&self, other: &Span) -> Option<Span> {
        if self.source_id != other.source_id {
            return None;
        }
        if !self.overlaps(other) {
            return None;
        }
        Some(Span {
            source_id: self.source_id,
            start_byte: self.start_byte.max(other.start_byte),
            end_byte: self.end_byte.min(other.end_byte),
        })
    }

    pub fn range(&self) -> Range {
        Range {
            start_byte: self.start_byte,
            end_byte: self.end_byte,
            start_point: Point { row: 0, column: 0 },
            end_point: Point { row: 0, column: 0 },
        }
    }
}

pub struct SpanDisplay<'a> {
    pub span: &'a Span,
    pub source_manager: &'a SourceManager,
}

impl ariadne::Span for Span {
    type SourceId = Option<SourceId>;
    fn source(&self) -> &Self::SourceId {
        &self.source_id
    }

    fn contains(&self, offset: usize) -> bool {
        self.start_byte <= offset && offset < self.end_byte
    }

    fn end(&self) -> usize {
        self.end_byte
    }

    fn is_empty(&self) -> bool {
        self.start_byte == self.end_byte
    }

    fn len(&self) -> usize {
        self.end_byte - self.start_byte
    }

    fn start(&self) -> usize {
        self.start_byte
    }
}

impl ariadne::Cache<Option<SourceId>> for &SourceManager {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &Option<SourceId>,
    ) -> Result<&ariadne::Source<Self::Storage>, impl std::fmt::Debug> {
        match id {
            Some(id) => {
                if let Some(source) = self.get_source(*id) {
                    match source {
                        Source::File { contents, .. } => Ok(contents),
                        Source::Stdin { contents } => Ok(contents),
                        Source::Memory { contents } => Ok(contents),
                        Source::None => Err("Source is None".to_owned()),
                    }
                } else {
                    Err("Source not found".to_owned())
                }
            }

            None => Err("No source ID provided".into()),
        }
    }

    fn display<'a>(&self, id: &'a Option<SourceId>) -> Option<impl std::fmt::Display + 'a> {
        id.and_then(|source_id| {
            self.get_source(source_id).map(|source| match source {
                Source::File { path, .. } => path.to_string_lossy().to_string(),
                Source::Stdin { .. } => "<stdin>".to_string(),
                Source::Memory { .. } => "<memory>".to_string(),
                Source::None => "<none>".to_string(),
            })
        })
    }
}
impl ariadne::Cache<SourceId> for &SourceManager {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &SourceId,
    ) -> Result<&ariadne::Source<Self::Storage>, impl std::fmt::Debug> {
        if let Some(source) = self.get_source(*id) {
            match source {
                Source::File { contents, .. } => Ok(contents),
                Source::Stdin { contents } => Ok(contents),
                Source::Memory { contents } => Ok(contents),
                Source::None => Err("Source is None".to_owned()),
            }
        } else {
            Err("Source not found".to_owned())
        }
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<impl std::fmt::Display + 'a> {
        self.get_source(*id).map(|source| match source {
            Source::File { path, .. } => path.to_string_lossy().to_string(),
            Source::Stdin { .. } => "<stdin>".to_string(),
            Source::None => "<none>".to_string(),
            Source::Memory { .. } => "<memory>".to_string(),
        })
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn with_span(self, span: Span) -> Self {
        Self {
            value: self.value,
            span,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {:?}", self.value, self.span)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at {:?}", self.value, self.span)
    }
}
