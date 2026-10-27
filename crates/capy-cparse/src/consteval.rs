use crate::ast::*;
use crate::types::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ConstVal {
    Int(i128, IntKind),
    Float(f64, FloatKind),
    Str(Vec<u8>),
    Ptr(i128),
}

type R<T> = Result<T, String>;

fn err<T>(m: impl Into<String>) -> R<T> {
    Err(m.into())
}

impl Unit {
    pub fn size_t(&self) -> IntKind {
        if self.abi.long == self.abi.ptr {
            IntKind::ULong
        } else {
            IntKind::ULongLong
        }
    }

    pub fn ptrdiff_t(&self) -> IntKind {
        self.size_t().to_signed()
    }

    pub fn wrap(&self, v: i128, k: IntKind) -> i128 {
        let bits = self.abi.int_size(k) * 8;
        if bits >= 128 {
            return v;
        }
        let mask = (1i128 << bits) - 1;
        let low = v & mask;
        if self.abi.is_unsigned(k) || low < (1i128 << (bits - 1)) {
            low
        } else {
            low - (1i128 << bits)
        }
    }

    fn promote(&self, k: IntKind) -> IntKind {
        if k.rank() < IntKind::Int.rank() {
            IntKind::Int
        } else {
            k
        }
    }

    pub fn common_int(&self, a: IntKind, b: IntKind) -> IntKind {
        let (a, b) = (self.promote(a), self.promote(b));
        if a == b {
            return a;
        }
        let (ua, ub) = (self.abi.is_unsigned(a), self.abi.is_unsigned(b));
        if ua == ub {
            return if a.rank() >= b.rank() { a } else { b };
        }
        let (u, s) = if ua { (a, b) } else { (b, a) };
        if u.rank() >= s.rank() {
            u
        } else if self.abi.int_size(s) > self.abi.int_size(u) {
            s
        } else {
            s.to_unsigned()
        }
    }

    pub fn eval(&self, e: &Expr) -> R<ConstVal> {
        use ConstVal::*;
        Ok(match e {
            Expr::Int(v, k) => Int(*v, *k),
            Expr::Char(v, k) => Int(*v, *k),
            Expr::Bool(b) => Int(*b as i128, IntKind::Int),
            Expr::Float(f, k) => Float(*f, *k),
            Expr::Str(s) => Str(s.clone()),
            Expr::Nullptr => Ptr(0),
            Expr::Typed(_, e) => return self.eval(e),
            Expr::Ident(n) => match self.lookup(n) {
                Some(Ident::EnumConst(v, id)) => {
                    let k = match self.enum_underlying(*id) {
                        Type::Int(k) if self.abi.int_size(k) > self.abi.int => k,
                        _ => IntKind::Int,
                    };
                    Int(*v, k)
                }
                _ => return err(format!("'{n}' is not a constant")),
            },
            Expr::Unary(op, x) => {
                let v = self.eval(x)?;
                match (*op, v) {
                    (UnOp::Plus, Int(a, k)) => Int(a, self.promote(k)),
                    (UnOp::Neg, Int(a, k)) => {
                        let k = self.promote(k);
                        Int(self.wrap(-a, k), k)
                    }
                    (UnOp::BitNot, Int(a, k)) => {
                        let k = self.promote(k);
                        Int(self.wrap(!a, k), k)
                    }
                    (UnOp::Not, v) => Int(!truthy(&v)? as i128, IntKind::Int),
                    (UnOp::Plus, Float(f, k)) => Float(f, k),
                    (UnOp::Neg, Float(f, k)) => Float(-f, k),
                    (UnOp::Neg, Ptr(a)) => Ptr(-a),
                    (op, _) => {
                        return err(format!(
                            "unary '{}' on this operand is not constant",
                            op.name()
                        ));
                    }
                }
            }
            Expr::Binary(op, a, b) => return self.eval_binary(*op, a, b),
            Expr::Cond(c, a, b) => {
                let cv = self.eval(c)?;
                let (av, bv) = (self.eval(a), self.eval(b));
                let pick = if truthy(&cv)? { av? } else { bv? };
                // The result has the common type of both arms.
                match (self.type_of(a), self.type_of(b)) {
                    (Some(ta), Some(tb)) => {
                        let t = self.common_type(&ta, &tb);
                        return self.convert(pick, &t);
                    }
                    _ => pick,
                }
            }
            Expr::Comma(_, b) => self.eval(b)?,
            Expr::Cast(t, x) => {
                let v = self.eval(x)?;
                return self.convert(v, t);
            }
            Expr::SizeofType(t) => {
                let (s, _) = self
                    .size_align(t)
                    .ok_or("sizeof of incomplete or unsupported type")?;
                Int(s as i128, self.size_t())
            }
            Expr::AlignofType(t) => {
                let (_, a) = self
                    .size_align(t)
                    .ok_or("alignof of incomplete or unsupported type")?;
                Int(a as i128, self.size_t())
            }
            Expr::SizeofExpr(x) => {
                let t = self.type_of(x).ok_or("cannot determine type for sizeof")?;
                let (s, _) = self
                    .size_align(&t)
                    .ok_or("sizeof of incomplete or unsupported type")?;
                Int(s as i128, self.size_t())
            }
            Expr::OffsetOf(t, steps) => Int(self.offset_of(t, steps)? as i128, self.size_t()),
            Expr::Generic(ctrl, arms) => {
                let ct = self
                    .type_of(ctrl)
                    .ok_or("cannot determine type of _Generic controlling expression")?;
                let ct = self.decay_lvalue(&ct);
                let mut default = None;
                for (t, x) in arms {
                    match t {
                        Some(t) if self.types_compatible(&ct, &self.decay_lvalue(t)) => {
                            return self.eval(x);
                        }
                        None => default = Some(x),
                        _ => {}
                    }
                }
                return self.eval(default.ok_or("no matching _Generic association")?);
            }
            Expr::Call(f, args) => return self.eval_builtin_call(f, args),
            Expr::Index(..)
            | Expr::Member(..)
            | Expr::Arrow(..)
            | Expr::Assign(..)
            | Expr::PostInc(..)
            | Expr::StmtExpr(_)
            | Expr::CompoundLit(..)
            | Expr::Unsupported(_) => {
                return err("expression is not constant");
            }
        })
    }

    fn eval_binary(&self, op: BinOp, a: &Expr, b: &Expr) -> R<ConstVal> {
        use ConstVal::*;
        if op.is_logical() {
            let l = truthy(&self.eval(a)?)?;
            if (op == BinOp::LAnd && !l) || (op == BinOp::LOr && l) {
                return Ok(Int(l as i128, IntKind::Int));
            }
            return Ok(Int(truthy(&self.eval(b)?)? as i128, IntKind::Int));
        }
        let (x, y) = (self.eval(a)?, self.eval(b)?);
        match (x, y) {
            (Int(p, pk), Int(q, qk)) => {
                if op.is_shift() {
                    let k = self.promote(pk);
                    let bits = self.abi.int_size(k) * 8;
                    if q < 0 || q as u128 >= bits as u128 {
                        return err("shift count out of range");
                    }
                    let v = if op == BinOp::Shl { p << q } else { p >> q };
                    return Ok(Int(self.wrap(v, k), k));
                }
                let k = self.common_int(pk, qk);
                let (p, q) = (self.wrap(p, k), self.wrap(q, k));
                let cmp = |b: bool| Ok(Int(b as i128, IntKind::Int));
                let arith = |v: i128| Ok(Int(self.wrap(v, k), k));
                match op {
                    BinOp::Add => arith(p.wrapping_add(q)),
                    BinOp::Sub => arith(p.wrapping_sub(q)),
                    BinOp::Mul => arith(p.wrapping_mul(q)),
                    BinOp::Div | BinOp::Rem => {
                        if q == 0 {
                            return err("division by zero");
                        }
                        arith(if op == BinOp::Div {
                            p.wrapping_div(q)
                        } else {
                            p.wrapping_rem(q)
                        })
                    }
                    BinOp::BitAnd => arith(p & q),
                    BinOp::BitOr => arith(p | q),
                    BinOp::BitXor => arith(p ^ q),
                    BinOp::Eq => cmp(p == q),
                    BinOp::Ne => cmp(p != q),
                    BinOp::Lt => cmp(p < q),
                    BinOp::Gt => cmp(p > q),
                    BinOp::Le => cmp(p <= q),
                    BinOp::Ge => cmp(p >= q),
                    _ => err(format!("operator {} is not constant", op.name())),
                }
            }
            (x @ (Float(..) | Int(..)), y @ (Float(..) | Int(..))) => {
                let kind = |v: &ConstVal| if let Float(_, k) = v { Some(*k) } else { None };
                let k = match (kind(&x), kind(&y)) {
                    (Some(a), Some(b)) => std::cmp::max_by_key(a, b, |k| self.abi.float_size(*k)),
                    (Some(a), None) | (None, Some(a)) => a,
                    _ => unreachable!(),
                };
                let f = |v: &ConstVal| match v {
                    Float(f, _) => *f,
                    Int(i, _) => *i as f64,
                    _ => 0.0,
                };
                let (p, q) = (f(&x), f(&y));
                let cmp = |b: bool| Ok(Int(b as i128, IntKind::Int));
                match op {
                    BinOp::Add => Ok(Float(p + q, k)),
                    BinOp::Sub => Ok(Float(p - q, k)),
                    BinOp::Mul => Ok(Float(p * q, k)),
                    BinOp::Div => Ok(Float(p / q, k)),
                    BinOp::Eq => cmp(p == q),
                    BinOp::Ne => cmp(p != q),
                    BinOp::Lt => cmp(p < q),
                    BinOp::Gt => cmp(p > q),
                    BinOp::Le => cmp(p <= q),
                    BinOp::Ge => cmp(p >= q),
                    _ => err(format!(
                        "operator {} on floating point is not constant",
                        op.name()
                    )),
                }
            }
            (Ptr(p), Int(q, _)) | (Int(q, _), Ptr(p)) if matches!(op, BinOp::Add | BinOp::Sub) => {
                Ok(Ptr(if op == BinOp::Add { p + q } else { p - q }))
            }
            (Ptr(p), Ptr(q)) if matches!(op, BinOp::Eq | BinOp::Ne) => {
                Ok(Int(((p == q) == (op == BinOp::Eq)) as i128, IntKind::Int))
            }
            _ => err("operands are not constant"),
        }
    }

    fn eval_builtin_call(&self, f: &Expr, args: &[Expr]) -> R<ConstVal> {
        use ConstVal::*;
        let Expr::Ident(name) = f else {
            return err("call is not constant");
        };
        let Some(b) = Builtin::lookup(name) else {
            return err(format!("call to '{name}' is not constant"));
        };
        match b {
            Builtin::Expect if !args.is_empty() => self.eval(&args[0]),
            Builtin::ConstantP => Ok(Int(
                args.first().is_some_and(|a| self.eval(a).is_ok()) as i128,
                IntKind::Int,
            )),
            Builtin::ChooseExpr if args.len() == 3 => {
                let c = truthy(&self.eval(&args[0])?)?;
                self.eval(&args[if c { 1 } else { 2 }])
            }
            Builtin::Inf(k) => Ok(Float(f64::INFINITY, k)),
            Builtin::Nan(k) => Ok(Float(f64::NAN, k)),
            _ => err(format!("call to '{name}' is not constant")),
        }
    }

    /// Converts a constant to type `t`.
    pub fn convert(&self, v: ConstVal, t: &Type) -> R<ConstVal> {
        use ConstVal::*;
        let t = self.resolve(t);
        Ok(match (&t, v) {
            (Type::Int(k), Int(i, _)) => Int(self.wrap(i, *k), *k),
            (Type::Int(k), Float(f, _)) => Int(self.wrap(f as i128, *k), *k),
            (Type::Int(k), Ptr(p)) => Int(self.wrap(p, *k), *k),
            (Type::Enum(id), v) => return self.convert(v, &self.enum_underlying(*id)),
            (Type::Bool, v) => Int(truthy(&v)? as i128, IntKind::Int),
            (Type::Float(k), Int(i, _)) => Float(i as f64, *k),
            (Type::Float(k), Float(f, _)) => Float(
                if *k == FloatKind::Float {
                    f as f32 as f64
                } else {
                    f
                },
                *k,
            ),
            (Type::Ptr(_) | Type::NullPtr, Int(i, k)) => Ptr(if self.abi.is_unsigned(k) {
                i
            } else {
                self.wrap(i, self.size_t())
            }),
            (Type::Ptr(_), Ptr(p)) => Ptr(p),
            (Type::Ptr(_), Str(s)) => Str(s),
            (Type::Void, v) => v,
            _ => return err("unsupported constant conversion"),
        })
    }

    pub fn offset_of(&self, t: &Type, steps: &[OffsetStep]) -> R<u64> {
        let mut cur = self.resolve(t);
        let mut total = 0u64;
        for s in steps {
            match s {
                OffsetStep::Field(name) => {
                    let Type::Record(id) = cur else {
                        return err("offsetof on non-record");
                    };
                    let (off, ty) = self
                        .find_field(id, name)
                        .ok_or_else(|| format!("no member '{name}'"))?;
                    total += off;
                    cur = self.resolve(&ty);
                }
                OffsetStep::Index(e) => {
                    let Type::Array(el, _) = &cur else {
                        return err("offsetof index on non-array");
                    };
                    let i = match self.eval(e)? {
                        ConstVal::Int(i, _) => i,
                        _ => return err("non-integer offsetof index"),
                    };
                    let (es, _) = self.size_align(el).ok_or("incomplete element")?;
                    total = (total as i128 + i * es as i128) as u64;
                    cur = self.resolve(el);
                }
            }
        }
        Ok(total)
    }

    pub fn find_field(&self, id: RecId, name: &str) -> Option<(u64, Type)> {
        let rec = &self.records[id];
        let layout = rec.layout.as_ref()?;
        for (f, l) in rec.fields.iter().zip(&layout.fields) {
            match &f.name {
                Some(n) if &**n == name => return Some((l.offset, f.ty.clone())),
                None => {
                    if let Type::Record(inner) = self.resolve(&f.ty) {
                        if let Some((o, t)) = self.find_field(inner, name) {
                            return Some((l.offset + o, t));
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn decay_lvalue(&self, t: &Type) -> Type {
        match self.resolve(t) {
            Type::Array(e, _) => Type::Ptr(e),
            Type::Func(f) => Type::Ptr(Box::new(Type::Func(f))),
            other => other,
        }
    }

    pub fn types_compatible(&self, a: &Type, b: &Type) -> bool {
        let (a, b) = (self.resolve(a), self.resolve(b));
        match (&a, &b) {
            (Type::Int(x), Type::Int(y)) => x == y,
            (Type::Enum(x), Type::Enum(y)) => x == y,
            (Type::Enum(id), Type::Int(k)) | (Type::Int(k), Type::Enum(id)) => {
                self.enum_underlying(*id) == Type::Int(*k)
            }
            (Type::Ptr(x), Type::Ptr(y)) => self.types_compatible(x, y),
            (Type::Array(x, lx), Type::Array(y, ly)) => {
                self.types_compatible(x, y)
                    && (lx == ly || *lx == ArrLen::Incomplete || *ly == ArrLen::Incomplete)
            }
            (Type::Func(x), Type::Func(y)) => {
                self.types_compatible(&x.ret, &y.ret)
                    && x.variadic == y.variadic
                    && x.params.len() == y.params.len()
                    && x.params
                        .iter()
                        .zip(&y.params)
                        .all(|(p, q)| self.types_compatible(p, q))
            }
            _ => a == b,
        }
    }

    fn common_type(&self, a: &Type, b: &Type) -> Type {
        let (a, b) = (self.resolve(a), self.resolve(b));
        match (&a, &b) {
            (Type::Int(x), Type::Int(y)) => Type::Int(self.common_int(*x, *y)),
            (Type::Float(x), Type::Float(y)) => {
                Type::Float(std::cmp::max_by_key(*x, *y, |k| self.abi.float_size(*k)))
            }
            (Type::Float(_), _) => a,
            (_, Type::Float(_)) => b,
            (Type::Enum(_) | Type::Bool, Type::Int(_) | Type::Enum(_) | Type::Bool) => {
                Type::Int(self.common_int(IntKind::Int, IntKind::Int))
            }
            _ => a,
        }
    }

    pub fn type_of(&self, e: &Expr) -> Option<Type> {
        Some(match e {
            Expr::Int(_, k) | Expr::Char(_, k) => Type::Int(*k),
            Expr::Bool(_) => Type::Int(IntKind::Int),
            Expr::Float(_, k) => Type::Float(*k),
            Expr::Str(s) => Type::Array(
                Box::new(Type::Int(IntKind::Char)),
                ArrLen::Fixed(s.len() as u64 + 1),
            ),
            Expr::Nullptr => Type::NullPtr,
            Expr::Typed(t, _) => t.clone(),
            Expr::Ident(n) => match self.lookup(n)? {
                Ident::Var(t) | Ident::Func(t) | Ident::Typedef(t) => t.clone(),
                Ident::EnumConst(_, id) => match self.enum_underlying(*id) {
                    Type::Int(k) if self.abi.int_size(k) > self.abi.int => Type::Int(k),
                    _ => Type::Int(IntKind::Int),
                },
            },
            Expr::Cast(t, _) | Expr::CompoundLit(t, _) => t.clone(),
            Expr::SizeofType(_)
            | Expr::SizeofExpr(_)
            | Expr::AlignofType(_)
            | Expr::OffsetOf(..) => Type::Int(self.size_t()),
            Expr::Unary(op, x) => match *op {
                UnOp::Not => Type::Int(IntKind::Int),
                UnOp::AddrOf => Type::Ptr(Box::new(self.type_of(x)?)),
                UnOp::Deref => match self.resolve(&self.type_of(x)?) {
                    Type::Ptr(t) | Type::Array(t, _) => *t,
                    Type::Func(f) => Type::Func(f),
                    _ => return None,
                },
                UnOp::PreInc | UnOp::PreDec => self.type_of(x)?,
                _ => match self.resolve(&self.type_of(x)?) {
                    Type::Int(k) => Type::Int(self.promote(k)),
                    Type::Enum(_) | Type::Bool => Type::Int(IntKind::Int),
                    other => other,
                },
            },
            Expr::PostInc(_, x) => self.type_of(x)?,
            Expr::Binary(op, a, b) => match *op {
                op if op.is_comparison() || op.is_logical() => Type::Int(IntKind::Int),
                BinOp::Shl | BinOp::Shr => match self.resolve(&self.type_of(a)?) {
                    Type::Int(k) => Type::Int(self.promote(k)),
                    _ => Type::Int(IntKind::Int),
                },
                _ => {
                    let (ta, tb) = (
                        self.decay_lvalue(&self.type_of(a)?),
                        self.decay_lvalue(&self.type_of(b)?),
                    );
                    match (&ta, &tb) {
                        (Type::Ptr(_), Type::Ptr(_)) if *op == BinOp::Sub => {
                            Type::Int(self.ptrdiff_t())
                        }
                        (Type::Ptr(_), _) => ta,
                        (_, Type::Ptr(_)) => tb,
                        _ => self.common_type(&ta, &tb),
                    }
                }
            },
            Expr::Assign(_, l, _) => self.type_of(l)?,
            Expr::Cond(_, a, b) => self.common_type(&self.type_of(a)?, &self.type_of(b)?),
            Expr::Comma(_, b) => self.type_of(b)?,
            Expr::Member(x, f) => {
                let Type::Record(id) = self.resolve(&self.type_of(x)?) else {
                    return None;
                };
                self.find_field(id, f)?.1
            }
            Expr::Arrow(x, f) => {
                let (Type::Ptr(inner) | Type::Array(inner, _)) = self.resolve(&self.type_of(x)?)
                else {
                    return None;
                };
                let Type::Record(id) = self.resolve(&inner) else {
                    return None;
                };
                self.find_field(id, f)?.1
            }
            Expr::Index(a, _) => match self.resolve(&self.type_of(a)?) {
                Type::Ptr(t) | Type::Array(t, _) => *t,
                _ => return None,
            },
            Expr::Call(f, _) => match self.resolve(&self.type_of(f)?) {
                Type::Func(ft) => ft.ret.clone(),
                Type::Ptr(p) => match self.resolve(&p) {
                    Type::Func(ft) => ft.ret.clone(),
                    _ => return None,
                },
                _ => return None,
            },
            Expr::Generic(c, arms) => {
                let ct = self.decay_lvalue(&self.type_of(c)?);
                let mut default = None;
                for (t, x) in arms {
                    match t {
                        Some(t) if self.types_compatible(&ct, &self.decay_lvalue(t)) => {
                            return self.type_of(x);
                        }
                        None => default = Some(x),
                        _ => {}
                    }
                }
                return self.type_of(default?);
            }
            Expr::StmtExpr(_) | Expr::Unsupported(_) => return None,
        })
    }
}

pub fn truthy(v: &ConstVal) -> R<bool> {
    Ok(match v {
        ConstVal::Int(i, _) => *i != 0,
        ConstVal::Float(f, _) => *f != 0.0,
        ConstVal::Ptr(p) => *p != 0,
        ConstVal::Str(_) => true,
    })
}
