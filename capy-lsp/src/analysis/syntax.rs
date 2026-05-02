#[derive(Debug, Clone)]
pub(super) struct Token {
    pub(super) index: usize,
    pub(super) text: String,
    pub(super) start: usize,
    pub(super) end: usize,
}

pub(super) fn tokenize_code(text: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut index = 0usize;
    let mut iter = text.char_indices().peekable();
    let mut in_comment = false;
    while let Some((start, ch)) = iter.next() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if ch == ';' {
            in_comment = true;
            continue;
        }
        if ch.is_whitespace() {
            continue;
        }
        if matches!(ch, '(' | ')' | '[' | ']') {
            tokens.push(Token {
                index,
                text: match ch {
                    '[' => "(".into(),
                    ']' => ")".into(),
                    _ => ch.to_string(),
                },
                start,
                end: start + ch.len_utf8(),
            });
            index += 1;
            continue;
        }
        if ch == '"' {
            let mut end = start + ch.len_utf8();
            let mut escape = false;
            for (idx, next) in iter.by_ref() {
                end = idx + next.len_utf8();
                if escape {
                    escape = false;
                } else if next == '\\' {
                    escape = true;
                } else if next == '"' {
                    break;
                }
            }
            tokens.push(Token {
                index,
                text: text[start..end].into(),
                start,
                end,
            });
            index += 1;
            continue;
        }

        let mut end = start + ch.len_utf8();
        while let Some((idx, next)) = iter.peek().copied() {
            if next.is_whitespace()
                || matches!(next, '(' | ')' | '[' | ']' | '"' | ';' | '\'' | '`' | ',')
            {
                break;
            }
            iter.next();
            end = idx + next.len_utf8();
        }
        tokens.push(Token {
            index,
            text: text[start..end].into(),
            start,
            end,
        });
        index += 1;
    }
    tokens
}

pub(super) fn is_identifier(text: &str) -> bool {
    !text.is_empty()
        && text != "("
        && text != ")"
        && !text.starts_with('"')
        && !matches!(
            text,
            "define"
                | "define-syntax"
                | "define-macro"
                | "lambda"
                | "let"
                | "let*"
                | "letrec"
                | "letrec*"
                | "library"
                | "define-library"
                | "import"
                | "export"
        )
}
