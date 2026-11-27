use crate::prelude::*;
use crate::runtime::prelude::*;
pub(crate) fn init_eval<'gc>(ctx: MutationContext<'gc>) {
    eval_ops::register(ctx);
}

pub(crate) fn is_applicable(value: Value<'_>) -> bool {
    value.is::<Closure>()
}

/// What `list` is as the last argument of `apply`.
pub(crate) enum ListShape {
    /// A proper list of this many elements.
    Proper(usize),
    /// It ends in something other than `()`.
    Improper,
    /// It has a cycle. Printing it would not end, so the error does not show it.
    Circular,
}

pub(crate) fn list_shape(list: Value<'_>) -> ListShape {
    let mut count = 0usize;
    let mut cursor = list;
    // Floyd's cycle check: `slow` moves one pair for every two of `cursor`.
    let mut slow = list;
    while !cursor.is_null() {
        if !cursor.is_pair() {
            return ListShape::Improper;
        }
        count += 1;
        cursor = cursor.cdr();
        if count % 2 == 0 {
            slow = slow.cdr();
            if cursor.is_pair() && cursor == slow {
                return ListShape::Circular;
            }
        }
    }
    ListShape::Proper(count)
}

pub(crate) const LAST_NOT_LIST: &str = "last argument must be a list";
pub(crate) const LAST_CIRCULAR: &str = "last argument must be a list, but it is circular";

#[scheme(path=capy)]
pub mod eval_ops {
    #[scheme(name = "apply")]
    pub fn apply(rator: Value<'gc>, rands: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if !is_applicable(rator) {
            call.wrong_argument_violation(
                "apply",
                "attempt to call non-procedure",
                Some(rator),
                Some(0),
                1,
                &[rator],
            );
            return Err(Value::undefined());
        }

        if rands.is_empty() {
            call.tail(rator, &[]);
            return Err(Value::undefined());
        }
        let fixed = &rands[..rands.len() - 1];
        let rest = rands[rands.len() - 1];
        let rest_len = match list_shape(rest) {
            ListShape::Proper(count) => count,
            ListShape::Improper => {
                call.wrong_argument_violation(
                    "apply",
                    LAST_NOT_LIST,
                    Some(rest),
                    Some(rands.len() - 1),
                    rands.len(),
                    rands,
                );
                return Err(Value::undefined());
            }
            ListShape::Circular => {
                call.wrong_argument_violation("apply", LAST_CIRCULAR, None, None, rands.len(), rands);
                return Err(Value::undefined());
            }
        };
        let mut args = Vec::with_capacity(fixed.len() + rest_len);
        args.extend_from_slice(fixed);
        let mut items = rest;
        while !items.is_null() {
            args.push(items.car());
            items = items.cdr();
        }
        call.tail(rator, &args);
        Err(Value::undefined())
    }

    #[scheme(name = "procedure?")]
    pub fn procedure_p(v: Value<'gc>) -> Value<'gc> {
        Value::new(is_applicable(v))
    }
}
