pub mod inherents;
pub mod opcode;
use std::{
    ops::{Index, IndexMut},
    sync::OnceLock,
};

use inherents::Inherent;
use rsgc::{Rootable, Trace, mutator::Global};

use super::{Context, value::*};

pub struct VirtualMachine<'gc> {
    inherents: [Value<'gc>; inherents::Inherent::TOTAL],
}

unsafe impl<'gc> Trace for VirtualMachine<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        self.inherents
            .iter_mut()
            .for_each(|inherent| inherent.trace(visitor));
    }
}

impl<'gc> VirtualMachine<'gc> {
    pub(crate) fn init(ctx: Context<'gc>) -> Self {
        let inherents = [Value::new(Value::void()); inherents::Inherent::TOTAL];

        let mut this = Self { inherents };

        let mut inherent = |s, inherent| {
            let sym = Symbol::new_inherent(ctx, s, inherent);
            this[inherent] = Value::new(sym);
        };

        inherent("const", Inherent::VMOP_CONST);
        inherent("const.unspec", Inherent::VMOP_CONST_UNSPEC);
        inherent("const.undef", Inherent::VMOP_CONST_UNDEF);
        inherent("gloc.of", Inherent::VMOP_GLOC_OF);
        inherent("gloc", Inherent::VMOP_GLOC);
        inherent("iloc", Inherent::VMOP_ILOC);
        inherent("car.iloc", Inherent::VMOP_CAR_ILOC);
        inherent("cdr.iloc", Inherent::VMOP_CDR_ILOC);
        inherent("cadr.iloc", Inherent::VMOP_CADR_ILOC);
        inherent("cddr.iloc", Inherent::VMOP_CDDR_ILOC);
        inherent("close", Inherent::VMOP_CLOSE);
        inherent("ret.const", Inherent::VMOP_RET_CONST);
        inherent("ret.const.unspec", Inherent::VMOP_RET_CONST_UNSPEC);
        inherent("ret.const.undef", Inherent::VMOP_RET_CONST_UNDEF);
        inherent("ret.gloc.of", Inherent::VMOP_RET_GLOC_OF);
        inherent("ret.gloc", Inherent::VMOP_RET_GLOC);
        inherent("ret.iloc", Inherent::VMOP_RET_ILOC);
        inherent("ret.close", Inherent::VMOP_RET_CLOSE);
        inherent("push.const", Inherent::VMOP_PUSH_CONST);
        inherent("push.const.unspec", Inherent::VMOP_PUSH_CONST_UNSPEC);
        inherent("push.const.undef", Inherent::VMOP_PUSH_CONST_UNDEF);
        inherent("push.gloc.of", Inherent::VMOP_PUSH_GLOC_OF);
        inherent("push.gloc", Inherent::VMOP_PUSH_GLOC);
        inherent("push.iloc", Inherent::VMOP_PUSH_ILOC);
        inherent("push.car.iloc", Inherent::VMOP_PUSH_CAR_ILOC);
        inherent("push.cdr.iloc", Inherent::VMOP_PUSH_CDR_ILOC);
        inherent("push.cadr.iloc", Inherent::VMOP_PUSH_CADR_ILOC);
        inherent("push.cddr.iloc", Inherent::VMOP_PUSH_CDDR_ILOC);
        inherent("push.close", Inherent::VMOP_PUSH_CLOSE);
        inherent("push.close+", Inherent::VMOP_PUSH_CLOSE_LOCAL);
        inherent("push", Inherent::VMOP_PUSH);
        inherent("set.gloc.of", Inherent::VMOP_SET_GLOC_OF);
        inherent("set.gloc", Inherent::VMOP_SET_GLOC);
        inherent("set.iloc", Inherent::VMOP_SET_ILOC);
        inherent("if.true", Inherent::VMOP_IF_TRUE);
        inherent("if.false.tailcall", Inherent::VMOP_IF_FALSE_TAILCALL);
        inherent("if.true.ret", Inherent::VMOP_IF_TRUE_RET);
        inherent("if.false.ret", Inherent::VMOP_IF_FALSE_RET);
        inherent("call", Inherent::VMOP_CALL);
        inherent("apply.gloc.of", Inherent::VMOP_APPLY_GLOC_OF);
        inherent("apply.gloc", Inherent::VMOP_APPLY_GLOC);
        inherent("apply.iloc", Inherent::VMOP_APPLY_ILOC);
        inherent("apply.iloc+", Inherent::VMOP_APPLY_ILOC_LOCAL);
        inherent("apply", Inherent::VMOP_APPLY);
        inherent("extend", Inherent::VMOP_EXTEND);
        inherent("enclose", Inherent::VMOP_ENCLOSE);
        inherent("touch.gloc.of", Inherent::VMOP_TOUCH_GLOC_OF);
        inherent("touch.gloc", Inherent::VMOP_TOUCH_GLOC);
        inherent("subr.gloc.of", Inherent::VMOP_SUBR_GLOC_OF);
        inherent("subr.gloc", Inherent::VMOP_SUBR_GLOC);
        inherent("subr", Inherent::VMOP_SUBR);
        inherent("extend.unbound", Inherent::VMOP_EXTEND_UNBOUND);
        inherent("extend.enclose", Inherent::VMOP_EXTEND_ENCLOSE);
        inherent("extend.enclose+", Inherent::VMOP_EXTEND_ENCLOSE_LOCAL);
        inherent("vm.escape", Inherent::VMOP_VM_ESCAPE);
        inherent("push.iloc.0", Inherent::VMOP_PUSH_ILOC0);
        inherent("push.iloc.1", Inherent::VMOP_PUSH_ILOC1);
        inherent("iloc.0", Inherent::VMOP_ILOC0);
        inherent("iloc.1", Inherent::VMOP_ILOC1);
        inherent("ret.subr.gloc.of", Inherent::VMOP_RET_SUBR_GLOC_OF);
        inherent("ret.subr.gloc", Inherent::VMOP_RET_SUBR_GLOC);
        inherent("ret.subr", Inherent::VMOP_RET_SUBR);
        inherent("push.subr", Inherent::VMOP_PUSH_SUBR);
        inherent("push.subr.gloc.of", Inherent::VMOP_PUSH_SUBR_GLOC_OF);
        inherent("push.subr.gloc", Inherent::VMOP_PUSH_SUBR_GLOC);
        inherent("if.null?", Inherent::VMOP_IF_NULLP);
        inherent("if.null?.ret.const", Inherent::VMOP_IF_NULLP_RET_CONST);
        inherent(
            "if.not.null?.ret.const",
            Inherent::VMOP_IF_NOT_NULLP_RET_CONST,
        );
        inherent("if.pair?", Inherent::VMOP_IF_PAIRP);
        inherent("if.pair?.ret.const", Inherent::VMOP_IF_PAIRP_RET_CONST);
        inherent(
            "if.not.pair?.ret.const",
            Inherent::VMOP_IF_NOT_PAIRP_RET_CONST,
        );
        inherent("if.symbol?", Inherent::VMOP_IF_SYMBOLP);
        inherent("if.symbol?.ret.const", Inherent::VMOP_IF_SYMBOLP_RET_CONST);
        inherent(
            "if.not.symbol?.ret.const",
            Inherent::VMOP_IF_NOT_SYMBOLP_RET_CONST,
        );
        inherent("if.eq?", Inherent::VMOP_IF_EQP);
        inherent("if.eq?.ret.const", Inherent::VMOP_IF_EQP_RET_CONST);
        inherent("if.not.eq?.ret.const", Inherent::VMOP_IF_NOT_EQP_RET_CONST);
        inherent("if.true.ret.const", Inherent::VMOP_IF_TRUE_RET_CONST);
        inherent("if.false.ret.const", Inherent::VMOP_IF_FALSE_RET_CONST);
        inherent("ret.cons", Inherent::VMOP_RET_CONS);
        inherent("ret.eq?", Inherent::VMOP_RET_EQP);
        inherent("ret.null?", Inherent::VMOP_RET_NULLP);
        inherent("ret.pair?", Inherent::VMOP_RET_PAIRP);
        inherent("push.cons", Inherent::VMOP_PUSH_CONS);
        inherent("push.n+.iloc", Inherent::VMOP_PUSH_NADD_ILOC);
        inherent("n+.iloc", Inherent::VMOP_NADD_ILOC);
        inherent("=n.iloc", Inherent::VMOP_EQ_N_ILOC);
        inherent("<n.iloc", Inherent::VMOP_LT_N_ILOC);
        inherent("<=n.iloc", Inherent::VMOP_LE_N_ILOC);
        inherent(">n.iloc", Inherent::VMOP_GT_N_ILOC);
        inherent(">=n.iloc", Inherent::VMOP_GE_N_ILOC);
        inherent("=.iloc", Inherent::VMOP_EQ_ILOC);
        inherent("<.iloc", Inherent::VMOP_LT_ILOC);
        inherent("<=.iloc", Inherent::VMOP_LE_ILOC);
        inherent(">.iloc", Inherent::VMOP_GT_ILOC);
        inherent(">=.iloc", Inherent::VMOP_GE_ILOC);
        inherent("little", Inherent::S_CODE_LITTLE);
        inherent("big", Inherent::S_CODE_BIG);
        inherent("quote", Inherent::S_CODE_QUOTE);
        inherent("quasiquote", Inherent::S_CODE_QUASIQUOTE);
        inherent("unquote", Inherent::S_CODE_UNQUOTE);
        inherent("unquote-splicing", Inherent::S_CODE_UNQUOTE_SPLICING);
        inherent("syntax", Inherent::S_CODE_SYNTAX);
        inherent("quasisyntax", Inherent::S_CODE_QUASISYNTAX);
        inherent("unsyntax", Inherent::S_CODE_UNSYNTAX);
        inherent("unsyntax-splicing", Inherent::S_CODE_UNSYNTAX_SPLICING);
        inherent("(", Inherent::S_CODE_LPAREN);
        inherent(")", Inherent::S_CODE_RPAREN);
        inherent("[", Inherent::S_CODE_LBRACK);
        inherent("]", Inherent::S_CODE_RBRACK);
        inherent(".", Inherent::S_CODE_DOT);

        this
    }

    pub fn get(ctx: Context<'gc>) -> &'gc Self {
        VM.get_or_init(|| Global::new(Self::init(ctx))).fetch(&ctx)
    }
}

impl<'gc> Index<Inherent> for VirtualMachine<'gc> {
    type Output = Value<'gc>;

    fn index(&self, index: Inherent) -> &Self::Output {
        &self.inherents[index as usize]
    }
}

impl<'gc> IndexMut<Inherent> for VirtualMachine<'gc> {
    fn index_mut(&mut self, index: Inherent) -> &mut Self::Output {
        &mut self.inherents[index as usize]
    }
}

pub type GlobalVM = Global<Rootable!(VirtualMachine<'_>)>;

static VM: OnceLock<GlobalVM> = OnceLock::new();
