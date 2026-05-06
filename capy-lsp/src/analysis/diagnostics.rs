use crate::{
    document::range_for_bytes,
    protocol::{DiagnosticFact, DiagnosticSeverityFact},
};

pub(super) fn balance_diagnostics(text: &str) -> Vec<DiagnosticFact> {
    let mut diagnostics = Vec::new();
    let mut stack: Vec<(char, usize)> = Vec::new();
    let mut in_string = false;
    let mut escape = false;
    let mut comment = false;

    for (idx, ch) in text.char_indices() {
        if comment {
            if ch == '\n' {
                comment = false;
            }
            continue;
        }
        if in_string {
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            ';' => comment = true,
            '"' => in_string = true,
            '(' | '[' => stack.push((ch, idx)),
            ')' => match stack.pop() {
                Some(('(', _)) => {}
                Some((_, start)) => {
                    diagnostics.push(unmatched(text, start, "mismatched closing ')'"))
                }
                None => diagnostics.push(unmatched(text, idx, "unmatched closing ')'")),
            },
            ']' => match stack.pop() {
                Some(('[', _)) => {}
                Some((_, start)) => {
                    diagnostics.push(unmatched(text, start, "mismatched closing ']'"))
                }
                None => diagnostics.push(unmatched(text, idx, "unmatched closing ']'")),
            },
            _ => {}
        }
    }

    if in_string {
        diagnostics.push(DiagnosticFact {
            range: range_for_bytes(text, text.len().saturating_sub(1), text.len()),
            severity: DiagnosticSeverityFact::Error,
            message: "unterminated string literal".into(),
            source: Some("capy-lsp".into()),
            code: Some("syntax".into()),
        });
    }
    for (_, start) in stack {
        diagnostics.push(unmatched(text, start, "unclosed list"));
    }
    diagnostics
}

fn unmatched(text: &str, byte: usize, message: &str) -> DiagnosticFact {
    DiagnosticFact {
        range: range_for_bytes(text, byte, byte + 1),
        severity: DiagnosticSeverityFact::Error,
        message: message.into(),
        source: Some("capy-lsp".into()),
        code: Some("syntax".into()),
    }
}
