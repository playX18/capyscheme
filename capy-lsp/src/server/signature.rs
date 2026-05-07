use lsp_types::{
    Documentation, ParameterInformation, ParameterLabel, Position, SignatureInformation,
};

use crate::{
    document::position_to_byte,
    protocol::{DocumentFacts, SymbolKindFact},
};

use super::convert::completion_fact_detail;

#[derive(Debug, Clone)]
pub(super) struct CallableSignature {
    pub(super) label: String,
    pub(super) params: Vec<String>,
}

#[derive(Debug)]
pub(super) struct CallContext {
    pub(super) name: String,
    pub(super) active_parameter: u32,
}

pub(super) fn completion_detail(facts: &DocumentFacts, name: &str) -> Option<String> {
    facts
        .completions
        .iter()
        .find(|item| item.label == name)
        .and_then(completion_fact_detail)
}

pub(super) fn callable_signature(name: &str, detail: Option<&str>) -> Option<CallableSignature> {
    let detail = detail?;
    for line in detail
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
    {
        if let Some(signature) = signature_from_line(name, line) {
            return Some(signature);
        }
    }
    None
}

pub(super) fn completion_snippet(
    kind: SymbolKindFact,
    label: &str,
    signature: &CallableSignature,
) -> Option<String> {
    if kind != SymbolKindFact::Function || signature.params.is_empty() {
        return None;
    }
    let placeholders = signature
        .params
        .iter()
        .enumerate()
        .map(|(idx, param)| format!("${{{}:{}}}", idx + 1, snippet_escape(param)))
        .collect::<Vec<_>>()
        .join(" ");
    Some(format!("{label} {placeholders}"))
}

pub(super) fn signature_information(
    signature: &CallableSignature,
    documentation: Option<&str>,
) -> SignatureInformation {
    SignatureInformation {
        label: signature.label.clone(),
        documentation: documentation
            .filter(|documentation| !documentation.is_empty())
            .map(|documentation| Documentation::String(documentation.to_string())),
        parameters: Some(
            signature
                .params
                .iter()
                .map(|param| ParameterInformation {
                    label: ParameterLabel::Simple(param.clone()),
                    documentation: None,
                })
                .collect(),
        ),
        active_parameter: None,
    }
}

pub(super) fn enclosing_call(text: &str, position: Position) -> Option<CallContext> {
    let byte = position_to_byte(text, position);
    let open = find_enclosing_open(text, byte)?;
    let segment = &text[open + 1..byte];
    let (name, operator_end) = first_token(segment)?;
    Some(CallContext {
        name,
        active_parameter: active_parameter_index(&segment[operator_end..]),
    })
}

fn signature_from_line(name: &str, line: &str) -> Option<CallableSignature> {
    for start in line.match_indices('(').map(|(idx, _)| idx) {
        let Some(end) = matching_paren(line, start) else {
            continue;
        };
        if let Some(signature) = parse_signature(name, &line[start..=end]) {
            return Some(signature);
        }
    }
    None
}

fn matching_paren(text: &str, start: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    for (idx, ch) in text[start..].char_indices() {
        let idx = start + idx;
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn parse_signature(name: &str, text: &str) -> Option<CallableSignature> {
    let inner = text.strip_prefix('(')?.strip_suffix(')')?.trim();
    let tokens = signature_tokens(inner);
    let (head, params) = tokens.split_first()?;
    if head != name {
        return None;
    }
    let params = params
        .iter()
        .filter(|param| param.as_str() != ".")
        .map(|param| param.trim_matches(|ch| matches!(ch, '[' | ']' | '(' | ')')))
        .filter(|param| !param.is_empty())
        .map(str::to_owned)
        .collect();
    Some(CallableSignature {
        label: text.to_string(),
        params,
    })
}

fn signature_tokens(text: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut start = None;
    let mut depth = 0usize;
    for (idx, ch) in text.char_indices() {
        if ch.is_whitespace() && depth == 0 {
            if let Some(token_start) = start.take() {
                tokens.push(text[token_start..idx].to_string());
            }
            continue;
        }
        if start.is_none() {
            start = Some(idx);
        }
        match ch {
            '(' | '[' => depth += 1,
            ')' | ']' => depth = depth.saturating_sub(1),
            _ => {}
        }
    }
    if let Some(token_start) = start {
        tokens.push(text[token_start..].to_string());
    }
    tokens
}

fn snippet_escape(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('$', "\\$")
        .replace('}', "\\}")
}

fn find_enclosing_open(text: &str, byte: usize) -> Option<usize> {
    let mut stack = Vec::new();
    let mut in_string = false;
    let mut in_comment = false;
    let mut escaped = false;
    for (idx, ch) in text[..byte.min(text.len())].char_indices() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            ';' => in_comment = true,
            '"' => in_string = true,
            '(' | '[' => stack.push(idx),
            ')' | ']' => {
                stack.pop();
            }
            _ => {}
        }
    }
    stack.pop()
}

fn first_token(text: &str) -> Option<(String, usize)> {
    let start = text
        .char_indices()
        .find(|(_, ch)| !ch.is_whitespace())
        .map(|(idx, _)| idx)?;
    let end = text[start..]
        .char_indices()
        .find(|(_, ch)| ch.is_whitespace() || matches!(ch, '(' | ')' | '[' | ']'))
        .map(|(idx, _)| start + idx)
        .unwrap_or(text.len());
    Some((text[start..end].to_string(), end))
}

fn active_parameter_index(text: &str) -> u32 {
    let mut started = 0u32;
    let mut depth = 0usize;
    let mut in_token = false;
    let mut in_string = false;
    let mut in_comment = false;
    let mut escaped = false;
    for ch in text.chars() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            ';' => in_comment = true,
            '"' => {
                if depth == 0 && !in_token {
                    started += 1;
                    in_token = true;
                }
                in_string = true;
            }
            '(' | '[' => {
                if depth == 0 && !in_token {
                    started += 1;
                    in_token = true;
                }
                depth += 1;
            }
            ')' | ']' => depth = depth.saturating_sub(1),
            ch if ch.is_whitespace() && depth == 0 => in_token = false,
            _ if depth == 0 && !in_token => {
                started += 1;
                in_token = true;
            }
            _ => {}
        }
    }
    if text.chars().last().is_some_and(char::is_whitespace) {
        started
    } else {
        started.saturating_sub(1)
    }
}
