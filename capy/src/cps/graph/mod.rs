use crate::{Gc, Trace};

pub type TermRef<'gc> = Gc<'gc, Term<'gc>>;

#[derive(Trace)]
pub struct Term<'gc> {
    /// Previous term in the linked-list
    pub prev: Option<TermRef<'gc>>,
    /// Next term in the linked-list, also is the next
    /// term in evaluation order.
    pub next: Option<TermRef<'gc>>,
}
