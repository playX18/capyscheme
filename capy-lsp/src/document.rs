use lsp_types::{Position, Range, TextDocumentContentChangeEvent};

use crate::protocol::DocumentFacts;

#[derive(Debug, Clone)]
pub struct Document {
    pub version: i32,
    pub text: String,
    pub dirty: bool,
    pub facts: Option<DocumentFacts>,
}

impl Document {
    pub fn new(_uri: lsp_types::Url, version: i32, text: String) -> Self {
        Self {
            version,
            text,
            dirty: true,
            facts: None,
        }
    }

    pub fn apply_changes(&mut self, version: i32, changes: Vec<TextDocumentContentChangeEvent>) {
        self.version = version;
        for change in changes {
            if let Some(range) = change.range {
                apply_range_change(&mut self.text, range, &change.text);
            } else {
                self.text = change.text;
            }
        }
        self.dirty = true;
    }
}

pub fn apply_range_change(text: &mut String, range: Range, replacement: &str) {
    let start = position_to_byte(text, range.start);
    let end = position_to_byte(text, range.end);
    if start <= end && end <= text.len() {
        text.replace_range(start..end, replacement);
    }
}

pub fn position_to_byte(text: &str, position: Position) -> usize {
    let mut byte = 0usize;
    for (line_idx, line) in text.split_inclusive('\n').enumerate() {
        if line_idx == position.line as usize {
            return byte + utf16_col_to_byte(line, position.character);
        }
        byte += line.len();
    }
    text.len()
}

pub fn byte_to_position(text: &str, target: usize) -> Position {
    let target = target.min(text.len());
    let mut byte = 0usize;
    for (line_idx, line) in text.split_inclusive('\n').enumerate() {
        let next = byte + line.len();
        if target <= next {
            return Position {
                line: line_idx as u32,
                character: byte_to_utf16_col(line, target - byte),
            };
        }
        byte = next;
    }
    Position {
        line: text.lines().count().saturating_sub(1) as u32,
        character: 0,
    }
}

pub fn range_for_bytes(text: &str, start: usize, end: usize) -> Range {
    Range {
        start: byte_to_position(text, start),
        end: byte_to_position(text, end),
    }
}

pub fn word_at_position(text: &str, position: Position) -> Option<(String, Range)> {
    let byte = position_to_byte(text, position);
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }
    let mut start = byte.min(bytes.len());
    if start == bytes.len() {
        start = start.saturating_sub(1);
    }
    if !is_ident_byte(bytes[start]) && start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    if !is_ident_byte(bytes[start]) {
        return None;
    }

    let mut left = start;
    while left > 0 && is_ident_byte(bytes[left - 1]) {
        left -= 1;
    }
    let mut right = start;
    while right < bytes.len() && is_ident_byte(bytes[right]) {
        right += 1;
    }
    Some((
        text[left..right].to_string(),
        range_for_bytes(text, left, right),
    ))
}

fn utf16_col_to_byte(line: &str, utf16_col: u32) -> usize {
    let mut units = 0u32;
    for (idx, ch) in line.char_indices() {
        if units >= utf16_col {
            return idx;
        }
        if ch == '\n' {
            return idx;
        }
        units += ch.len_utf16() as u32;
    }
    line.len()
}

fn byte_to_utf16_col(line: &str, byte_col: usize) -> u32 {
    line[..byte_col.min(line.len())]
        .chars()
        .take_while(|ch| *ch != '\n')
        .map(|ch| ch.len_utf16() as u32)
        .sum()
}

fn is_ident_byte(byte: u8) -> bool {
    !byte.is_ascii_whitespace()
        && !matches!(
            byte,
            b'(' | b')' | b'[' | b']' | b'"' | b'\'' | b'`' | b',' | b';'
        )
}

#[cfg(test)]
mod tests {
    use lsp_types::Position;

    use super::{apply_range_change, word_at_position};

    #[test]
    fn applies_full_range_change() {
        let mut text = "(define x 1)\n".to_string();
        apply_range_change(
            &mut text,
            lsp_types::Range {
                start: Position {
                    line: 0,
                    character: 8,
                },
                end: Position {
                    line: 0,
                    character: 9,
                },
            },
            "answer",
        );
        assert_eq!(text, "(define answer 1)\n");
    }

    #[test]
    fn finds_word_at_position() {
        let text = "(define answer 42)";
        let (word, _) = word_at_position(
            text,
            Position {
                line: 0,
                character: 10,
            },
        )
        .unwrap();
        assert_eq!(word, "answer");
    }
}
