//! Statement parsing
use super::{PR, Parser};
use crate::ast::*;
use crate::token::{Kind, Punct, Word};
use crate::types::*;
use std::rc::Rc;

impl Parser {
    pub(crate) fn function_body(&mut self, params: &[Param]) -> PR<Vec<Stmt>> {
        self.push_scope();
        self.fn_depth += 1;
        for p in params {
            if let Some(n) = &p.name {
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(n.clone(), Ident::Var(p.ty.clone()));
            }
        }
        let r = self.block_stmts();
        self.fn_depth -= 1;
        self.pop_scope();
        r
    }

    /// `{ stmt* }` in a fresh scope.
    pub(crate) fn block_stmts(&mut self) -> PR<Vec<Stmt>> {
        self.expect(Punct::LBrace)?;
        self.push_scope();
        let mut out = Vec::new();
        let r = loop {
            if self.eat(Punct::RBrace) {
                break Ok(());
            }
            if self.at_eof() {
                break self.err("unterminated block");
            }
            match self.stmt() {
                Ok(s) => out.push(s),
                Err(e) => break Err(e),
            }
        };
        self.pop_scope();
        r.map(|_| out)
    }

    pub(crate) fn stmt(&mut self) -> PR<Stmt> {
        if self.at(Punct::LBrace) {
            return Ok(Stmt::Block(self.block_stmts()?));
        }
        if self.eat(Punct::Semi) {
            return Ok(Stmt::Empty);
        }
        if self.tok().kind == Kind::Ident {
            match self.tok().word {
                Some(Word::If) => {
                    self.bump();
                    self.expect(Punct::LParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(Punct::RParen)?;
                    let then = Box::new(self.scoped_stmt()?);
                    let els = if self.eat_word(Word::Else) {
                        Some(Box::new(self.scoped_stmt()?))
                    } else {
                        None
                    };
                    return Ok(Stmt::If(cond, then, els));
                }
                Some(Word::While) => {
                    self.bump();
                    self.expect(Punct::LParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(Punct::RParen)?;
                    return Ok(Stmt::While(cond, Box::new(self.scoped_stmt()?)));
                }
                Some(Word::Do) => {
                    self.bump();
                    let body = Box::new(self.scoped_stmt()?);
                    self.expect_word(Word::While)?;
                    self.expect(Punct::LParen)?;
                    let cond = self.parse_expr()?;
                    self.expect(Punct::RParen)?;
                    self.expect(Punct::Semi)?;
                    return Ok(Stmt::DoWhile(body, cond));
                }
                Some(Word::For) => {
                    self.bump();
                    self.expect(Punct::LParen)?;
                    self.push_scope();
                    let r = self.for_rest();
                    self.pop_scope();
                    return r;
                }
                Some(Word::Switch) => {
                    self.bump();
                    self.expect(Punct::LParen)?;
                    let e = self.parse_expr()?;
                    self.expect(Punct::RParen)?;
                    return Ok(Stmt::Switch(e, Box::new(self.scoped_stmt()?)));
                }
                Some(Word::Case) => {
                    self.bump();
                    let e = self.parse_cond_pub()?;
                    if self.eat(Punct::Ellipsis) {
                        self.parse_cond_pub()?;
                    }
                    self.expect(Punct::Colon)?;
                    return Ok(Stmt::Case(e, Box::new(self.stmt()?)));
                }
                Some(Word::Default) => {
                    self.bump();
                    self.expect(Punct::Colon)?;
                    return Ok(Stmt::Default(Box::new(self.stmt()?)));
                }
                Some(Word::Break) => {
                    self.bump();
                    self.expect(Punct::Semi)?;
                    return Ok(Stmt::Break);
                }
                Some(Word::Continue) => {
                    self.bump();
                    self.expect(Punct::Semi)?;
                    return Ok(Stmt::Continue);
                }
                Some(Word::Return) => {
                    self.bump();
                    let e = if self.at(Punct::Semi) {
                        None
                    } else {
                        Some(self.parse_expr()?)
                    };
                    self.expect(Punct::Semi)?;
                    return Ok(Stmt::Return(e));
                }
                Some(Word::Goto) => {
                    self.bump();
                    let l = self.ident()?;
                    self.expect(Punct::Semi)?;
                    return Ok(Stmt::Goto(l));
                }
                Some(Word::Asm) => {
                    self.bump();
                    while self.eat_word(Word::Volatile)
                        || self.eat_word(Word::Goto)
                        || self.eat_word(Word::Inline)
                    {}
                    if self.at(Punct::LParen) {
                        self.skip_balanced();
                    }
                    self.eat(Punct::Semi);
                    return Ok(Stmt::Asm);
                }
                Some(Word::StaticAssert) => {
                    self.bump();
                    self.skip_balanced();
                    self.eat(Punct::Semi);
                    return Ok(Stmt::Empty);
                }
                _ => {}
            }

            if self.at_n(1, Punct::Colon) && !self.is_type_start() && !self.at_n(2, Punct::Colon) {
                let l = self.ident()?;
                self.bump();
                let s = if self.at(Punct::RBrace) {
                    Stmt::Empty
                } else {
                    self.stmt()?
                };
                return Ok(Stmt::Label(l, Box::new(s)));
            }
            if self.is_decl_start() {
                return self.local_decl();
            }
        }
        let e = self.parse_expr()?;
        self.expect(Punct::Semi)?;
        Ok(Stmt::Expr(e))
    }

    fn parse_cond_pub(&mut self) -> PR<Expr> {
        self.parse_assign()
    }

    fn scoped_stmt(&mut self) -> PR<Stmt> {
        self.push_scope();
        let r = self.stmt();
        self.pop_scope();
        r
    }

    fn for_rest(&mut self) -> PR<Stmt> {
        let init = if self.eat(Punct::Semi) {
            None
        } else if self.is_decl_start() {
            Some(Box::new(self.local_decl()?))
        } else {
            let e = self.parse_expr()?;
            self.expect(Punct::Semi)?;
            Some(Box::new(Stmt::Expr(e)))
        };
        let cond = if self.at(Punct::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(Punct::Semi)?;
        let step = if self.at(Punct::RParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(Punct::RParen)?;
        let body = Box::new(self.scoped_stmt()?);
        Ok(Stmt::For(init, cond, step, body))
    }

    fn local_decl(&mut self) -> PR<Stmt> {
        let specs = self.decl_specs()?;
        let Some(base) = specs.ty.clone() else {
            return self.err("expected a declaration");
        };
        let mut out = Vec::new();
        if self.eat(Punct::Semi) {
            return Ok(Stmt::Decl(out));
        }
        loop {
            let (name, ty, _) = self.declarator(base.clone(), false)?;
            let name = name.unwrap();
            self.attributes(&mut specs.attrs.clone());
            if self.at_word(Word::Asm) {
                self.bump();
                self.skip_balanced();
            }
            let ty = self.apply_type_attrs(ty, &mut specs.attrs.clone());
            if specs.storage == Some(Storage::Typedef) {
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name, Ident::Typedef(ty));
            } else if matches!(ty, Type::Func(_)) {
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name, Ident::Func(ty));
            } else {
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), Ident::Var(ty.clone()));
                let init = if self.eat(Punct::Assign) {
                    Some(if self.at(Punct::LBrace) {
                        Init::List(self.init_list()?)
                    } else {
                        Init::Expr(self.parse_assign()?)
                    })
                } else {
                    None
                };
                let ty = self.refine_local_type(ty, &init);
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), Ident::Var(ty.clone()));
                out.push(LocalDecl {
                    name: Rc::clone(&name),
                    ty,
                    init,
                    is_static: specs.storage == Some(Storage::Static),
                });
            }
            if !self.eat(Punct::Comma) {
                break;
            }
        }
        self.expect(Punct::Semi)?;
        Ok(Stmt::Decl(out))
    }

    fn refine_local_type(&self, ty: Type, init: &Option<Init>) -> Type {
        match (&ty, init) {
            (Type::Array(e, ArrLen::Incomplete), Some(Init::List(items))) => {
                Type::Array(e.clone(), ArrLen::Fixed(items.len() as u64))
            }
            (Type::Array(e, ArrLen::Incomplete), Some(Init::Expr(Expr::Str(s)))) => {
                Type::Array(e.clone(), ArrLen::Fixed(s.len() as u64 + 1))
            }
            (Type::Unsupported(_), Some(Init::Expr(e))) => self.u.type_of(e).unwrap_or(ty),
            _ => ty,
        }
    }
}
