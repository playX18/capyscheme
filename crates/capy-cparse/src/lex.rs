use crate::token::{Kind, Loc, Punct, Token};

pub fn lex(src: &str, file: u32) -> Vec<Token> {
    let (clean, splices) = splice_lines(src);
    Lexer {
        s: clean.as_bytes(),
        src: &clean,
        i: 0,
        line: 1,
        line_start: 0,
        splices,
        next_splice: 0,
        file,
    }
    .run()
}

fn splice_lines(src: &str) -> (String, Vec<usize>) {
    if !src.contains('\\') {
        return (src.replace("\r\n", "\n"), Vec::new());
    }
    let b = src.as_bytes();
    let mut out = Vec::with_capacity(b.len());
    let mut splices = Vec::new();
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'\\' {
            let mut j = i + 1;
            while j < b.len() && (b[j] == b' ' || b[j] == b'\t') {
                j += 1;
            }
            if j < b.len() && b[j] == b'\r' {
                j += 1;
            }
            if j < b.len() && b[j] == b'\n' {
                splices.push(out.len());
                i = j + 1;
                continue;
            }
        }
        if b[i] == b'\r' && b.get(i + 1) == Some(&b'\n') {
            i += 1;
            continue;
        }
        out.push(b[i]);
        i += 1;
    }

    (String::from_utf8(out).unwrap_or_default(), splices)
}

struct Lexer<'a> {
    s: &'a [u8],
    src: &'a str,
    i: usize,
    line: u32,
    line_start: usize,
    splices: Vec<usize>,
    next_splice: usize,
    file: u32,
}

fn is_ident_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_' || c == b'$' || c >= 0x80
}

fn is_ident_cont(c: u8) -> bool {
    is_ident_start(c) || c.is_ascii_digit()
}

impl Lexer<'_> {
    fn peek(&self, n: usize) -> u8 {
        *self.s.get(self.i + n).unwrap_or(&0)
    }

    fn advance_to(&mut self, to: usize) {
        while self.i < to {
            if self.s[self.i] == b'\n' {
                self.line += 1;
                self.line_start = self.i + 1;
            }
            self.i += 1;
            while self.next_splice < self.splices.len() && self.splices[self.next_splice] < self.i {
                self.line += 1;
                self.next_splice += 1;
            }
        }
    }

    fn run(mut self) -> Vec<Token> {
        let mut out = Vec::new();
        let mut bol = true;
        let mut space = false;
        while self.i < self.s.len() {
            let c = self.s[self.i];
            match c {
                b'\n' => {
                    self.advance_to(self.i + 1);
                    bol = true;
                    space = false;
                }
                b' ' | b'\t' | 0x0b | 0x0c | b'\r' => {
                    self.advance_to(self.i + 1);
                    space = true;
                }
                b'/' if self.peek(1) == b'/' => {
                    let end = self.s[self.i..]
                        .iter()
                        .position(|&b| b == b'\n')
                        .map_or(self.s.len(), |p| self.i + p);
                    self.advance_to(end);
                    space = true;
                }
                b'/' if self.peek(1) == b'*' => {
                    let end = find(&self.s[self.i + 2..], b"*/")
                        .map_or(self.s.len(), |p| self.i + 2 + p + 2);
                    self.advance_to(end);
                    space = true;
                }
                _ => {
                    let loc = Loc {
                        file: self.file,
                        line: self.line,
                        col: (self.i - self.line_start) as u32 + 1,
                    };
                    let start = self.i;
                    let kind = self.token();
                    let mut tok = match kind {
                        Kind::Punct(p) => Token::punct_tok(p),
                        k => Token::new(k, &self.src[start..self.i]),
                    };
                    tok.loc = loc;
                    tok.bol = bol;
                    tok.space = space;
                    out.push(tok);
                    bol = false;
                    space = false;
                }
            }
        }
        out
    }

    fn token(&mut self) -> Kind {
        let c = self.s[self.i];
        let start = self.i;
        if c.is_ascii_digit() || (c == b'.' && self.peek(1).is_ascii_digit()) {
            return self.number();
        }
        if is_ident_start(c) || (c == b'\\' && matches!(self.peek(1), b'u' | b'U')) {
            // String/char prefixes: u8 u U L.
            for p in ["u8", "u", "U", "L"] {
                if self.src[start..].starts_with(p) {
                    let q = self.s.get(start + p.len()).copied();
                    if q == Some(b'"') || q == Some(b'\'') {
                        self.advance_to(start + p.len());
                        return self.quoted();
                    }
                }
            }
            self.ident();
            return Kind::Ident;
        }
        if c == b'"' || c == b'\'' {
            return self.quoted();
        }
        if let Some((p, len)) = self.punct() {
            self.advance_to(start + len);
            return Kind::Punct(p);
        }
        self.advance_to(start + 1);
        if c < 0x80 { Kind::Other } else { Kind::Ident }
    }

    fn punct(&self) -> Option<(Punct, usize)> {
        use Punct::*;
        let (c0, c1, c2, c3) = (self.peek(0), self.peek(1), self.peek(2), self.peek(3));
        Some(match c0 {
            b'[' => (LBracket, 1),
            b']' => (RBracket, 1),
            b'(' => (LParen, 1),
            b')' => (RParen, 1),
            b'{' => (LBrace, 1),
            b'}' => (RBrace, 1),
            b'~' => (Tilde, 1),
            b'?' => (Question, 1),
            b';' => (Semi, 1),
            b',' => (Comma, 1),
            b'.' if c1 == b'.' && c2 == b'.' => (Ellipsis, 3),
            b'.' => (Dot, 1),
            b'-' => match c1 {
                b'>' => (Arrow, 2),
                b'-' => (Dec, 2),
                b'=' => (MinusAssign, 2),
                _ => (Minus, 1),
            },
            b'+' => match c1 {
                b'+' => (Inc, 2),
                b'=' => (PlusAssign, 2),
                _ => (Plus, 1),
            },
            b'&' => match c1 {
                b'&' => (AmpAmp, 2),
                b'=' => (AmpAssign, 2),
                _ => (Amp, 1),
            },
            b'*' if c1 == b'=' => (StarAssign, 2),
            b'*' => (Star, 1),
            b'/' if c1 == b'=' => (SlashAssign, 2),
            b'/' => (Slash, 1),
            b'%' => match c1 {
                b'=' => (PercentAssign, 2),
                b'>' => (RBrace, 2),
                b':' if c2 == b'%' && c3 == b':' => (HashHash, 4),
                b':' => (Hash, 2),
                _ => (Percent, 1),
            },
            b'<' => match c1 {
                b'<' if c2 == b'=' => (ShlAssign, 3),
                b'<' => (Shl, 2),
                b'=' => (Le, 2),
                b':' => (LBracket, 2),
                b'%' => (LBrace, 2),
                _ => (Lt, 1),
            },
            b'>' => match c1 {
                b'>' if c2 == b'=' => (ShrAssign, 3),
                b'>' => (Shr, 2),
                b'=' => (Ge, 2),
                _ => (Gt, 1),
            },
            b'=' if c1 == b'=' => (EqEq, 2),
            b'=' => (Assign, 1),
            b'!' if c1 == b'=' => (Ne, 2),
            b'!' => (Bang, 1),
            b'^' if c1 == b'=' => (CaretAssign, 2),
            b'^' => (Caret, 1),
            b'|' => match c1 {
                b'|' => (PipePipe, 2),
                b'=' => (PipeAssign, 2),
                _ => (Pipe, 1),
            },
            b':' => match c1 {
                b':' => (ColonColon, 2),
                b'>' => (RBracket, 2),
                _ => (Colon, 1),
            },
            b'#' if c1 == b'#' => (HashHash, 2),
            b'#' => (Hash, 1),
            _ => return None,
        })
    }

    fn ident(&mut self) {
        loop {
            let c = self.peek(0);
            if is_ident_cont(c) {
                self.advance_to(self.i + 1);
            } else if c == b'\\' && matches!(self.peek(1), b'u' | b'U') {
                // Universal character name.
                let n = if self.peek(1) == b'u' { 4 } else { 8 };
                if (0..n).all(|k| self.peek(2 + k).is_ascii_hexdigit()) {
                    self.advance_to(self.i + 2 + n);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn number(&mut self) -> Kind {
        self.advance_to(self.i + 1);
        loop {
            let c = self.peek(0);
            let prev = self.s[self.i - 1];
            if matches!(c, b'+' | b'-') && matches!(prev, b'e' | b'E' | b'p' | b'P') {
                self.advance_to(self.i + 1);
            } else if c == b'\'' && is_ident_cont(self.peek(1)) {
                self.advance_to(self.i + 2);
            } else if c == b'.' || is_ident_cont(c) {
                self.advance_to(self.i + 1);
            } else {
                break;
            }
        }
        Kind::Number
    }

    fn quoted(&mut self) -> Kind {
        let q = self.s[self.i];
        self.advance_to(self.i + 1);
        while self.i < self.s.len() {
            let c = self.s[self.i];
            if c == b'\n' {
                break; // unterminated; leave the newline for the main loop
            }
            if c == b'\\' && self.i + 1 < self.s.len() && self.s[self.i + 1] != b'\n' {
                self.advance_to(self.i + 2);
                continue;
            }
            self.advance_to(self.i + 1);
            if c == q {
                break;
            }
        }
        if q == b'"' { Kind::Str } else { Kind::Char }
    }
}

fn find(hay: &[u8], needle: &[u8]) -> Option<usize> {
    hay.windows(needle.len()).position(|w| w == needle)
}
