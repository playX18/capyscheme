//! Expansion environment (`Cenv`), lexical frame (`Frame`), and denotation
//! table (`Denotations`) types used by the Scheme macro expander.

use std::collections::HashMap;

use crate::{
    expander::term::LVarRef,
    rsgc::{Gc, Trace},
    runtime::{Context, modules::Module, value::Value},
};

pub struct Cenv<'gc> {
    pub ctx: Context<'gc>,
    pub expression_name: Value<'gc>,
    pub frames: Option<Box<Frame<'gc>>>,
    pub denotations: &'gc Denotations<'gc>,
    pub current_module: Gc<'gc, Module<'gc>>,
}

impl<'gc> Cenv<'gc> {
    pub fn toplevel(ctx: Context<'gc>) -> Self {
        Cenv {
            ctx,
            current_module: ctx.globals().root_module(),
            expression_name: Value::undefined(),
            frames: None,
            denotations: super::core::denotations(ctx),
        }
    }

    pub fn new(
        ctx: Context<'gc>,
        expression_name: Value<'gc>,
        module: Gc<'gc, Module<'gc>>,
        denotations: &'gc Denotations<'gc>,
    ) -> Self {
        Cenv {
            current_module: module,
            ctx,
            expression_name,
            frames: None,
            denotations,
        }
    }

    pub fn get(&self, name: Value<'gc>) -> Option<LVarRef<'gc>> {
        self.frames.as_ref().and_then(|frame| frame.get(name))
    }

    pub fn extend(&mut self, name: Value<'gc>, lvar_ref: LVarRef<'gc>) {
        self.frames.as_mut().unwrap().extend(name, lvar_ref);
    }

    pub fn new_frame(&mut self) {
        let mut new_frame = Box::new(Frame::new(None));
        new_frame.up = self.frames.take();
        self.frames = Some(new_frame);
    }

    pub fn pop_frame(&mut self) {
        if let Some(frame) = self.frames.take() {
            self.frames = frame.up;
        } else {
            panic!("Cannot pop frame from empty stack");
        }
    }
}

pub struct Frame<'gc> {
    pub up: Option<Box<Self>>,
    pub env: HashMap<Value<'gc>, LVarRef<'gc>>,
}

impl<'gc> Frame<'gc> {
    pub fn new(up: Option<Box<Self>>) -> Self {
        Frame {
            up,
            env: HashMap::new(),
        }
    }

    pub fn get(&self, name: Value<'gc>) -> Option<LVarRef<'gc>> {
        match self.env.get(&name) {
            Some(lvar_ref) => Some(*lvar_ref),
            None => self.up.as_ref().and_then(|up| up.get(name)),
        }
    }

    pub fn extend(&mut self, name: Value<'gc>, lvar_ref: LVarRef<'gc>) {
        self.env.insert(name, lvar_ref);
    }
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct Denotations<'gc> {
    pub denotation_of_define: Value<'gc>,
    pub denotation_of_let: Value<'gc>,
    pub denotation_of_let_star: Value<'gc>,
    pub denotation_of_let_rec: Value<'gc>,
    pub denotation_of_let_rec_star: Value<'gc>,
    pub denotation_of_lambda: Value<'gc>,
    pub denotation_of_case_lambda: Value<'gc>,
    pub denotation_of_begin: Value<'gc>,
    pub denotation_of_cond: Value<'gc>,
    pub denotation_of_case: Value<'gc>,
    pub denotation_of_do: Value<'gc>,
    pub denotation_of_if: Value<'gc>,
    pub denotation_of_quote: Value<'gc>,
    pub denotation_of_set: Value<'gc>,
    pub denotation_of_values: Value<'gc>,
    pub denotation_of_receive: Value<'gc>,
    pub denotation_of_and: Value<'gc>,
    pub denotation_of_or: Value<'gc>,
    pub denotation_of_when: Value<'gc>,
    pub denotation_of_unless: Value<'gc>,
    pub denotation_of_wcm: Value<'gc>,
    pub denotation_of_define_struct: Value<'gc>,
    pub denotation_of_public_ref: Value<'gc>,
    pub denotation_of_private_ref: Value<'gc>,
}
