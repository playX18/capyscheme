use std::collections::HashMap;

use lsp_types::Url;

use crate::{
    document::range_for_bytes,
    protocol::{LocationFact, ReferenceFact, SymbolFact, SymbolKindFact},
};

use super::syntax::{Token, is_identifier, tokenize_code};

pub(super) fn collect_symbols(text: &str) -> Vec<SymbolFact> {
    let tokens = tokenize_code(text);
    let mut symbols = Vec::new();
    for window in tokens.windows(3) {
        let [open, head, name] = window else {
            continue;
        };
        if open.text != "(" {
            continue;
        }
        match head.text.as_str() {
            "define" => {
                if name.text == "(" {
                    if let Some(function) = token_after(&tokens, name.index + 1) {
                        let mut item = symbol(function, SymbolKindFact::Function, text);
                        if let Some(signature) = signature_after_define_head(&tokens, name.index) {
                            item.detail = Some(signature);
                        }
                        symbols.push(item);
                    }
                } else {
                    symbols.push(symbol(name, SymbolKindFact::Variable, text));
                }
            }
            "define-syntax" | "define-macro" => {
                symbols.push(symbol(name, SymbolKindFact::Macro, text));
            }
            "define-library" | "library" => {
                if name.text == "(" {
                    let module_name = collect_module_name(&tokens, name.index + 1);
                    if let Some(first) = token_after(&tokens, name.index + 1) {
                        let mut module = symbol(first, SymbolKindFact::Module, text);
                        module.name = module_name;
                        module.detail = Some("module".into());
                        symbols.push(module);
                    }
                }
            }
            _ => {}
        }
    }
    symbols
}

pub(super) fn collect_references(
    uri: &Url,
    text: &str,
    symbols: &[SymbolFact],
) -> Vec<ReferenceFact> {
    let definitions: HashMap<&str, &SymbolFact> = symbols
        .iter()
        .map(|symbol| (symbol.name.as_str(), symbol))
        .collect();
    tokenize_code(text)
        .into_iter()
        .filter(|token| is_identifier(&token.text))
        .filter_map(|token| {
            let definition = definitions
                .get(token.text.as_str())
                .map(|symbol| LocationFact {
                    uri: uri.clone(),
                    range: symbol.selection_range,
                });
            Some(ReferenceFact {
                name: token.text,
                uri: uri.clone(),
                range: range_for_bytes(text, token.start, token.end),
                definition,
            })
        })
        .collect()
}

fn symbol(token: &Token, kind: SymbolKindFact, text: &str) -> SymbolFact {
    let range = range_for_bytes(text, token.start, token.end);
    SymbolFact {
        name: token.text.clone(),
        kind,
        range,
        selection_range: range,
        detail: match kind {
            SymbolKindFact::Function => Some("function".into()),
            SymbolKindFact::Variable => Some("binding".into()),
            SymbolKindFact::Macro => Some("macro".into()),
            SymbolKindFact::Module => Some("module".into()),
            SymbolKindFact::Keyword => Some("keyword".into()),
        },
        container: None,
    }
}

fn token_after(tokens: &[Token], index: usize) -> Option<&Token> {
    tokens.iter().find(|token| token.index == index)
}

fn collect_module_name(tokens: &[Token], start_index: usize) -> String {
    let mut parts = Vec::new();
    for token in tokens.iter().filter(|token| token.index >= start_index) {
        if token.text == ")" {
            break;
        }
        if is_identifier(&token.text) {
            parts.push(token.text.clone());
        }
    }
    parts.join(" ")
}

fn signature_after_define_head(tokens: &[Token], open_index: usize) -> Option<String> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    for token in tokens.iter().filter(|token| token.index >= open_index) {
        match token.text.as_str() {
            "(" => {
                depth += 1;
                if depth > 1 {
                    parts.push(token.text.clone());
                }
            }
            ")" => {
                if depth == 0 {
                    break;
                }
                depth -= 1;
                if depth == 0 {
                    return Some(format!("({})", parts.join(" ")));
                }
                parts.push(token.text.clone());
            }
            _ => parts.push(token.text.clone()),
        }
    }
    None
}
