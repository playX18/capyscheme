use crate::{
    list,
    runtime::{
        Context,
        value::{IntoValues, NativeReturn, Str, Value},
        vm::{NativeCallContext, NativeCallReturn},
    },
    static_symbols,
};

pub fn error<'gc>(
    ctx: Context<'gc>,
    key: Value<'gc>,
    subr: &str,
    message: &str,
    args: Value<'gc>,
    rest: Value<'gc>,
    retk: Value<'gc>,
    reth: Value<'gc>,
) -> NativeReturn<'gc> {
    let subr = if subr.is_empty() {
        Value::new(false)
    } else {
        Str::new(&ctx, subr, false).into()
    };
    let message = if message.is_empty() {
        Value::new(false)
    } else {
        Str::new(&ctx, message, false).into()
    };
    let ls = list!(ctx, subr, message, args, rest);

    super::throw::throw(ctx, key, ls, retk, reth)
}

static_symbols!(
    WRONG_NUMBER_OF_ARGS = "wrong-number-of-args"
    WRONG_TYPE_ARG = "wrong-type-arg"
    MISC_ERROR = "misc-error"
);

impl<'a, 'gc, R: IntoValues<'gc>> NativeCallContext<'a, 'gc, R> {
    pub fn wrong_type_arg(
        self,
        subr: &str,
        pos: usize,
        bad_value: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        let message = if pos == 0 {
            "Wrong type: ~A"
        } else {
            "Wrong type argument in position ~A: ~S"
        };

        let args = if pos == 0 {
            list!(self.ctx, bad_value)
        } else {
            list!(self.ctx, Value::new(pos as i32), bad_value)
        };
        let rest = list!(self.ctx, bad_value);

        let key = wrong_type_arg(self.ctx);

        self.error(key.into(), subr, &message, args, rest)
    }

    pub fn wrong_num_args(self, proc: Value<'gc>) -> NativeCallReturn<'gc> {
        let message = "Wrong number of arguments: ~A";
        let args = list!(self.ctx, proc);
        let rest = Value::new(false);

        let key = wrong_number_of_args(self.ctx);

        self.error(key.into(), "", &message, args, rest)
    }

    pub fn non_applicable(self, proc: Value<'gc>) -> NativeCallReturn<'gc> {
        let message = "Non-applicable type: ~S";
        let key = wrong_type_arg(self.ctx);
        let args = list!(self.ctx, proc);
        let rest = list!(self.ctx, proc);
        self.error(key.into(), "", &message, args, rest)
    }

    pub fn non_continuation(self, proc: Value<'gc>) -> NativeCallReturn<'gc> {
        let message = "Not a continuation: ~S";
        let key = wrong_type_arg(self.ctx);
        let args = list!(self.ctx, proc);
        let rest = list!(self.ctx, proc);
        self.error(key.into(), "", &message, args, rest)
    }
}
