use crate::{
    prelude::{Tuple, Value},
    rsgc::Trace,
    runtime::Context,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Trace)]
#[collect(no_drop)]
#[repr(u8)]
pub enum RaiseKind {
    AssertionViolation = 0,
    WrongNumberOfArguments = 1,
    NonApplicable = 2,
    Undefined = 3,
    WrongNumberOfArgumentsCar = 4,
    WrongNumberOfArgumentsCdr = 5,
    WrongNumberOfArgumentsSetCar = 6,
    WrongNumberOfArgumentsSetCdr = 7,
    WrongNumberOfArgumentsVariableRef = 8,
    WrongNumberOfArgumentsVariableSet = 9,
    WrongNumberOfArgumentsVariableBound = 10,
    CarNotPair = 11,
    CdrNotPair = 12,
}

impl RaiseKind {
    pub const fn code(self) -> usize {
        self as usize
    }

    pub fn from_code(code: usize) -> Option<Self> {
        match code {
            0 => Some(Self::AssertionViolation),
            1 => Some(Self::WrongNumberOfArguments),
            2 => Some(Self::NonApplicable),
            3 => Some(Self::Undefined),
            4 => Some(Self::WrongNumberOfArgumentsCar),
            5 => Some(Self::WrongNumberOfArgumentsCdr),
            6 => Some(Self::WrongNumberOfArgumentsSetCar),
            7 => Some(Self::WrongNumberOfArgumentsSetCdr),
            8 => Some(Self::WrongNumberOfArgumentsVariableRef),
            9 => Some(Self::WrongNumberOfArgumentsVariableSet),
            10 => Some(Self::WrongNumberOfArgumentsVariableBound),
            11 => Some(Self::CarNotPair),
            12 => Some(Self::CdrNotPair),
            _ => None,
        }
    }

    pub fn primitive_wrong_arity(self) -> Option<(&'static str, isize)> {
        match self {
            Self::WrongNumberOfArgumentsCar => Some(("car", 1)),
            Self::WrongNumberOfArgumentsCdr => Some(("cdr", 1)),
            Self::WrongNumberOfArgumentsSetCar => Some(("set-car!", 2)),
            Self::WrongNumberOfArgumentsSetCdr => Some(("set-cdr!", 2)),
            Self::WrongNumberOfArgumentsVariableRef => Some(("variable-ref", 1)),
            Self::WrongNumberOfArgumentsVariableSet => Some(("variable-set!", 2)),
            Self::WrongNumberOfArgumentsVariableBound => Some(("variable-bound?", 1)),
            _ => None,
        }
    }
}

/// A builder for condition objects.
pub struct ConditionBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub simple: Vec<Value<'gc>>,
}

impl<'gc> ConditionBuilder<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            simple: Vec::new(),
        }
    }

    pub fn serious(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&serious")
            .expect("failed to get &serious record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn message(mut self, msg: impl AsRef<str>) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&message")
            .expect("failed to get &message record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();
        let msg_value = self.ctx.str(msg.as_ref());
        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, msg_value]).into());
        self
    }

    pub fn message_value(mut self, msg: Value<'gc>) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&message")
            .expect("failed to get &message record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();
        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, msg]).into());
        self
    }

    pub fn warning(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&warning")
            .expect("failed to get &warning record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn error(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&error")
            .expect("failed to get &error record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn violation(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&violation")
            .expect("failed to get &violation record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn irritants(mut self, irritants: &[Value<'gc>]) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&irritants")
            .expect("failed to get &irritants record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();
        let irritants_value = irritants
            .iter()
            .rfold(Value::null(), |acc, item| Value::cons(self.ctx, *item, acc));
        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, irritants_value]).into());
        self
    }

    pub fn who(mut self, who: impl AsRef<str>) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&who")
            .expect("failed to get &who record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();
        let who_value = self.ctx.intern(who.as_ref());
        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, who_value]).into());
        self
    }

    pub fn who_value(mut self, who: Value<'gc>) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&who")
            .expect("failed to get &who record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();
        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, who]).into());
        self
    }

    pub fn marks(mut self) -> Self {
        let marks = self.ctx.current_continuation_marks();

        let record_type = self
            .ctx
            .public_ref("capy", "&marks")
            .expect("failed to get &marks record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();
        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, marks.into()]).into());
        self
    }

    pub fn non_continuable(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&non-continuable")
            .expect("failed to get &non-continuable record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn implementation_restriction(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&implementation-restriction")
            .expect("failed to get &implementation-restriction record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn lexical(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&lexical")
            .expect("failed to get &lexical record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn syntax(mut self, form: Value<'gc>, subform: Value<'gc>) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&syntax")
            .expect("failed to get &syntax record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd, form, subform]).into());
        self
    }

    pub fn undefined(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&undefined")
            .expect("failed to get &undefined record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn assertion(mut self) -> Self {
        let record_type = self
            .ctx
            .public_ref("capy", "&assertion")
            .expect("failed to get &assertion record type");
        let rtd = record_type.downcast::<Tuple>()[2].get();

        self.simple
            .push(Tuple::from_slice(*self.ctx, &[rtd]).into());
        self
    }

    pub fn build(self) -> Value<'gc> {
        let ls = self
            .simple
            .into_iter()
            .rfold(Value::null(), |acc, item| Value::cons(self.ctx, item, acc));
        Tuple::from_slice(*self.ctx, &[self.ctx.intern("type:condition"), ls]).into()
    }
}

pub fn make_assertion_violation<'gc>(
    ctx: Context<'gc>,
    who: Option<&str>,
    msg: &str,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let mut cc = ConditionBuilder::new(ctx).assertion();
    if let Some(who) = who {
        cc = cc.who(who);
    }
    cc.message(msg).irritants(irritants).marks().build()
}

pub fn make_assertion_violation_value<'gc>(
    ctx: Context<'gc>,
    who: Value<'gc>,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let mut cc = ConditionBuilder::new(ctx).assertion();
    if who != Value::new(false) {
        cc = cc.who_value(who);
    }
    cc.message_value(message)
        .irritants(irritants)
        .marks()
        .build()
}

pub fn make_non_applicable_violation<'gc>(ctx: Context<'gc>, subr: Value<'gc>) -> Value<'gc> {
    make_assertion_violation_value(
        ctx,
        Value::new(false),
        ctx.str("attempt to call non-procedure").into(),
        &[subr],
    )
}

pub fn make_wrong_number_of_arguments_violation<'gc>(
    ctx: Context<'gc>,
    subr: Value<'gc>,
    got: usize,
    expected: isize,
) -> Value<'gc> {
    let message = if expected < 0 {
        format!(
            "procedure expected at least {} arguments, got {}",
            -expected, got
        )
    } else {
        format!("procedure expected {} arguments, got {}", expected, got)
    };
    let meta = if subr.is::<crate::runtime::value::Closure>() {
        subr.downcast::<crate::runtime::value::Closure>().meta.get()
    } else {
        Value::new(false)
    };
    make_assertion_violation_value(
        ctx,
        Value::new(false),
        ctx.str(&message).into(),
        &[subr, meta],
    )
}

pub fn make_primitive_wrong_number_of_arguments_violation<'gc>(
    ctx: Context<'gc>,
    who: &'static str,
    got: usize,
    expected: isize,
) -> Value<'gc> {
    let message = if expected < 0 {
        format!(
            "procedure expected at least {} arguments, got {}",
            -expected, got
        )
    } else {
        format!("procedure expected {} arguments, got {}", expected, got)
    };
    ConditionBuilder::new(ctx)
        .assertion()
        .who(who)
        .message(&message)
        .irritants(&[])
        .marks()
        .build()
}

pub fn make_primitive_assertion_violation<'gc>(
    ctx: Context<'gc>,
    who: &'static str,
    message: &'static str,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    ConditionBuilder::new(ctx)
        .assertion()
        .who(who)
        .message(message)
        .irritants(irritants)
        .marks()
        .build()
}

pub fn make_raise_condition<'gc>(
    ctx: Context<'gc>,
    code: usize,
    values: &[Value<'gc>],
) -> Value<'gc> {
    match RaiseKind::from_code(code).expect("invalid raise kind code") {
        kind @ (RaiseKind::WrongNumberOfArgumentsCar
        | RaiseKind::WrongNumberOfArgumentsCdr
        | RaiseKind::WrongNumberOfArgumentsSetCar
        | RaiseKind::WrongNumberOfArgumentsSetCdr
        | RaiseKind::WrongNumberOfArgumentsVariableRef
        | RaiseKind::WrongNumberOfArgumentsVariableSet
        | RaiseKind::WrongNumberOfArgumentsVariableBound) => {
            let (who, expected) = kind
                .primitive_wrong_arity()
                .expect("primitive wrong arity kind should have static arity data");
            let got = values.first().and_then(|value| value.int32()).unwrap_or(0) as usize;
            make_primitive_wrong_number_of_arguments_violation(ctx, who, got, expected)
        }
        RaiseKind::AssertionViolation => {
            let who = values.first().copied().unwrap_or(Value::new(false));
            let message = values
                .get(1)
                .copied()
                .unwrap_or_else(|| ctx.str("assertion violation").into());
            let irritants = values.get(2..).unwrap_or(&[]);
            make_assertion_violation_value(ctx, who, message, irritants)
        }
        RaiseKind::WrongNumberOfArguments => {
            let subr = values.first().copied().unwrap_or(Value::new(false));
            let got = values.get(1).and_then(|value| value.int32()).unwrap_or(0) as usize;
            let expected = values.get(2).and_then(|value| value.int32()).unwrap_or(0) as isize;
            make_wrong_number_of_arguments_violation(ctx, subr, got, expected)
        }
        RaiseKind::NonApplicable => {
            let subr = values.first().copied().unwrap_or(Value::new(false));
            make_non_applicable_violation(ctx, subr)
        }
        RaiseKind::CarNotPair => {
            let value = values.first().copied().unwrap_or(Value::new(false));
            make_primitive_assertion_violation(ctx, "car", "not a pair", &[value])
        }
        RaiseKind::CdrNotPair => {
            let value = values.first().copied().unwrap_or(Value::new(false));
            make_primitive_assertion_violation(ctx, "cdr", "not a pair", &[value])
        }
        RaiseKind::Undefined => {
            let who = values.first().copied();
            let message = values
                .get(1)
                .copied()
                .unwrap_or_else(|| ctx.str("undefined value").into());
            let irritants = values.get(2..).unwrap_or(&[]);
            let mut cc = ConditionBuilder::new(ctx).undefined();
            if let Some(who) = who
                && who != Value::new(false)
            {
                cc = cc.who_value(who);
            }
            cc.message_value(message)
                .irritants(irritants)
                .marks()
                .build()
        }
    }
}

pub fn make_undefined_violation<'gc>(
    ctx: Context<'gc>,
    who: Option<Value<'gc>>,
    message: &str,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let mut cc = ConditionBuilder::new(ctx).undefined();
    if let Some(who) = who {
        cc = cc.who_value(who);
    }
    cc.message(message).irritants(irritants).marks().build()
}
