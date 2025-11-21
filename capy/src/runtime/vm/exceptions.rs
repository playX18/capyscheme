use crate::{
    prelude::{Tuple, Value},
    runtime::Context,
};

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
