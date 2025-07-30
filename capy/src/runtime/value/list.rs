use rsgc::{Trace, barrier};

use crate::runtime::Context;

use super::*;
#[derive(Trace)]
#[collect(no_drop)]
#[repr(C, align(8))]
pub struct Pair<'gc> {
    pub header: ScmHeader,
    pub car: Value<'gc>,
    pub cdr: Value<'gc>,
}

impl<'gc> Pair<'gc> {
    pub fn new(mc: Context<'gc>, car: Value<'gc>, cdr: Value<'gc>) -> Gc<'gc, Self> {
        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::PAIR.bits() as _);
        let pair = Gc::new(
            &mc,
            Self {
                header: hdr,
                car,
                cdr,
            },
        );
        pair
    }

    pub fn car(&self) -> Value<'gc> {
        self.car
    }

    pub fn cdr(&self) -> Value<'gc> {
        self.cdr
    }

    pub fn set_car(self: Gc<'gc, Self>, mc: Context<'gc>, value: Value<'gc>) {
        barrier::field!(Gc::write(&mc, self), Pair, car).write(value);
    }

    pub fn set_cdr(self: Gc<'gc, Self>, mc: Context<'gc>, value: Value<'gc>) {
        barrier::field!(Gc::write(&mc, self), Pair, cdr).write(value);
    }

    pub fn caar(&self) -> Value<'gc> {
        self.car().car()
    }

    pub fn cadr(&self) -> Value<'gc> {
        self.cdr().car()
    }

    pub fn cdar(&self) -> Value<'gc> {
        self.car().cdr()
    }

    pub fn cddr(&self) -> Value<'gc> {
        self.cdr().cdr()
    }

    pub fn caaar(&self) -> Value<'gc> {
        self.car().car().car()
    }

    pub fn caadr(&self) -> Value<'gc> {
        self.cdr().car().car()
    }

    pub fn caddr(&self) -> Value<'gc> {
        self.cdr().cdr().car()
    }

    pub fn cdaar(&self) -> Value<'gc> {
        self.car().car().cdr()
    }

    pub fn cdadr(&self) -> Value<'gc> {
        self.cdr().car().cdr()
    }

    pub fn cddar(&self) -> Value<'gc> {
        self.car().cdr().cdr()
    }

    pub fn cdddr(&self) -> Value<'gc> {
        self.cdr().cdr().cdr()
    }

    pub fn caaaar(&self) -> Value<'gc> {
        self.car().car().car().car()
    }

    pub fn caaadr(&self) -> Value<'gc> {
        self.cdr().car().car().car()
    }

    pub fn caadar(&self) -> Value<'gc> {
        self.car().cdr().car().car()
    }

    pub fn caaddr(&self) -> Value<'gc> {
        self.cdr().cdr().car().car()
    }

    pub fn cadaar(&self) -> Value<'gc> {
        self.car().car().cdr().car()
    }

    pub fn cadadr(&self) -> Value<'gc> {
        self.cdr().car().cdr().car()
    }

    pub fn caddar(&self) -> Value<'gc> {
        self.car().cdr().cdr().car()
    }

    pub fn cadddr(&self) -> Value<'gc> {
        self.cdr().cdr().cdr().car()
    }

    pub fn cdaaar(&self) -> Value<'gc> {
        self.car().car().car().cdr()
    }

    pub fn cdaadr(&self) -> Value<'gc> {
        self.cdr().car().car().cdr()
    }

    pub fn cdadar(&self) -> Value<'gc> {
        self.car().cdr().car().cdr()
    }

    pub fn cdaddr(&self) -> Value<'gc> {
        self.cdr().cdr().car().cdr()
    }

    pub fn cddaar(&self) -> Value<'gc> {
        self.car().car().cdr().cdr()
    }

    pub fn cddadr(&self) -> Value<'gc> {
        self.cdr().car().cdr().cdr()
    }

    pub fn cdddar(&self) -> Value<'gc> {
        self.car().cdr().cdr().cdr()
    }

    pub fn cddddr(&self) -> Value<'gc> {
        self.cdr().cdr().cdr().cdr()
    }
}

unsafe impl<'gc> Tagged for Pair<'gc> {
    const TC8: TypeCode8 = TypeCode8::PAIR;
}

impl<'gc> Value<'gc> {
    pub fn is_pair(&self) -> bool {
        self.is::<Pair>()
    }

    pub fn car(&self) -> Value<'gc> {
        self.downcast::<Pair>().car
    }

    pub fn cdr(&self) -> Value<'gc> {
        self.downcast::<Pair>().cdr
    }

    pub fn set_car(self, mc: Context<'gc>, value: Value<'gc>) {
        let pair = self.downcast::<Pair>();

        barrier::field!(Gc::write(&mc, pair), Pair, car).write(value);
    }

    pub fn set_cdr(self, mc: Context<'gc>, value: Value<'gc>) {
        let pair = self.downcast::<Pair>();

        barrier::field!(Gc::write(&mc, pair), Pair, cdr).write(value);
    }

    pub fn caar(&self) -> Value<'gc> {
        self.car().car()
    }

    pub fn cadr(&self) -> Value<'gc> {
        self.cdr().car()
    }

    pub fn cdar(&self) -> Value<'gc> {
        self.car().cdr()
    }

    pub fn cddr(&self) -> Value<'gc> {
        self.cdr().cdr()
    }

    pub fn caaar(&self) -> Value<'gc> {
        self.car().car().car()
    }

    pub fn caadr(&self) -> Value<'gc> {
        self.cdr().car().car()
    }

    pub fn caddr(&self) -> Value<'gc> {
        self.cdr().cdr().car()
    }

    pub fn cdaar(&self) -> Value<'gc> {
        self.car().car().cdr()
    }

    pub fn cdadr(&self) -> Value<'gc> {
        self.cdr().car().cdr()
    }

    pub fn cddar(&self) -> Value<'gc> {
        self.car().cdr().cdr()
    }

    pub fn cdddr(&self) -> Value<'gc> {
        self.cdr().cdr().cdr()
    }

    pub fn caaaar(&self) -> Value<'gc> {
        self.car().car().car().car()
    }

    pub fn caaadr(&self) -> Value<'gc> {
        self.cdr().car().car().car()
    }

    pub fn caadar(&self) -> Value<'gc> {
        self.car().cdr().car().car()
    }

    pub fn caaddr(&self) -> Value<'gc> {
        self.cdr().cdr().car().car()
    }

    pub fn cadaar(&self) -> Value<'gc> {
        self.car().car().cdr().car()
    }

    pub fn cadadr(&self) -> Value<'gc> {
        self.cdr().car().cdr().car()
    }

    pub fn caddar(&self) -> Value<'gc> {
        self.car().cdr().cdr().car()
    }

    pub fn cadddr(&self) -> Value<'gc> {
        self.cdr().cdr().cdr().car()
    }

    pub fn cdaaar(&self) -> Value<'gc> {
        self.car().car().car().cdr()
    }

    pub fn cdaadr(&self) -> Value<'gc> {
        self.cdr().car().car().cdr()
    }

    pub fn cdadar(&self) -> Value<'gc> {
        self.car().cdr().car().cdr()
    }

    pub fn cdaddr(&self) -> Value<'gc> {
        self.cdr().cdr().car().cdr()
    }

    pub fn cddaar(&self) -> Value<'gc> {
        self.car().car().cdr().cdr()
    }

    pub fn cddadr(&self) -> Value<'gc> {
        self.cdr().car().cdr().cdr()
    }

    pub fn cdddar(&self) -> Value<'gc> {
        self.car().cdr().cdr().cdr()
    }

    pub fn cddddr(&self) -> Value<'gc> {
        self.cdr().cdr().cdr().cdr()
    }

    pub fn list_ref(self, index: usize) -> Option<Value<'gc>> {
        let mut current = self;
        let mut i = 0;

        while i < index {
            if current.is_null() || !current.is_pair() {
                return None;
            }
            current = current.cdr();
            i += 1;
        }

        if current.is_null() || !current.is_pair() {
            None
        } else {
            Some(current.car())
        }
    }

    pub fn list_length(self) -> usize {
        let mut slow = self;
        let mut fast = self;
        let mut len = 0;

        loop {
            if !fast.is_pair() {
                return len;
            }
            fast = fast.cdr();
            len += 1;

            if !fast.is_pair() {
                return len;
            }
            fast = fast.cdr();
            slow = slow.cdr();
            len += 1;

            if fast == slow {
                // Cycle detected
                return usize::MAX;
            }
        }
    }

    pub fn is_list(self) -> bool {
        let mut slow = self;
        let mut fast = self;

        loop {
            if fast.is_null() {
                return true;
            }
            if !fast.is_pair() {
                return false;
            }
            fast = fast.cdr();

            if fast.is_null() {
                return true;
            }
            if !fast.is_pair() {
                return false;
            }
            fast = fast.cdr();
            slow = slow.cdr();

            if fast == slow {
                // Cycle detected
                return false;
            }
        }
    }

    pub fn list_to_vector(self, mc: Context<'gc>) -> Gc<'gc, Vector<'gc>> {
        if !self.is_list() {
            unreachable!("not a list")
        }

        let vector = Vector::new::<false>(&mc, self.list_length(), Value::null());
        let mut current = self;
        let mut index = 0;

        while current.is_pair() {
            Gc::write(&mc, vector)[index].write(current.car());
            index += 1;
            current = current.cdr();
        }

        vector
    }

    pub fn list_reverse(self, mc: Context<'gc>) -> Value<'gc> {
        if !self.is_list() {
            // Or handle error appropriately
            return self;
        }

        let mut reversed_list = Value::null();
        let mut current = self;

        while current.is_pair() {
            reversed_list = Value::cons(mc, current.car(), reversed_list);
            current = current.cdr();
        }
        reversed_list
    }

    pub fn append(self, mc: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
        let x = self;

        fn lp<'gc>(ctx: Context<'gc>, x: Value<'gc>, obj: Value<'gc>) -> Value<'gc> {
            if x.is_null() {
                obj
            } else {
                Value::cons(ctx, x.car(), lp(ctx, x.cdr(), obj))
            }
        }

        lp(mc, x, obj)
    }

    pub fn list_append<T>(self, mc: Context<'gc>, iter: impl IntoIterator<Item = T>) -> Value<'gc>
    where
        T: IntoValue<'gc>,
    {
        let mut new_head = Value::null();
        let mut new_tail = Value::null();

        // Helper to append a value to the new list
        let mut append_to_new_list = |val: Value<'gc>| {
            let new_pair = Pair::new(mc, val, Value::null());
            if new_head.is_null() {
                new_head = Value::from(new_pair);
                new_tail = new_head;
            } else {
                new_tail.set_cdr(mc, Value::from(new_pair));
                new_tail = Value::from(new_pair);
            }
        };

        // Copy elements from the original list (self)
        let mut current_original = self;
        while current_original.is_pair() {
            append_to_new_list(current_original.car());
            current_original = current_original.cdr();
        }

        if !self.is_list() && !self.is_null() {
            return new_head;
        }

        // Append elements from the iterator
        for val in iter {
            append_to_new_list(val.into_value(mc));
        }

        new_head
    }

    pub fn cons(mc: Context<'gc>, car: Value<'gc>, cdr: Value<'gc>) -> Value<'gc> {
        let pair = Pair::new(mc, car, cdr);
        Value::from(pair)
    }

    pub fn cons_star(mc: Context<'gc>, args: impl AsRef<[Value<'gc>]>) -> Value<'gc> {
        let args = args.as_ref();

        if args.len() == 0 {
            return Value::null();
        } else if args.len() == 1 {
            return args[0];
        } else {
            let obj = Self::cons(mc, args[0], Value::null());
            let mut tail = obj;

            for i in 1..args.len() - 1 {
                let e = Value::cons(mc, args[i], Value::null());
                tail.set_cdr(mc, e);
                tail = e;
            }

            tail.set_cdr(mc, args[args.len() - 1]);
            obj
        }
    }

    pub fn acons(
        mc: Context<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
        cdr: Value<'gc>,
    ) -> Value<'gc> {
        let pair = Pair::new(mc, key, value);
        let new_pair = Pair::new(mc, Value::from(pair), cdr);
        Value::from(new_pair)
    }

    pub fn map<F>(self, mc: Context<'gc>, mut f: F) -> Value<'gc>
    where
        F: FnMut(Value<'gc>) -> Value<'gc>,
    {
        let mut new_list = Value::null();
        let mut current = self;

        while current.is_pair() {
            let new_value = f(current.car());
            new_list = Value::cons(mc, new_value, new_list);
            current = current.cdr();
        }

        new_list
    }

    pub fn try_map<F, E>(self, mc: Context<'gc>, mut f: F) -> Result<Value<'gc>, E>
    where
        F: FnMut(Value<'gc>) -> Result<Value<'gc>, E>,
    {
        let mut new_list = Value::null();
        let mut current = self;

        while current.is_pair() {
            let new_value = f(current.car())?;
            new_list = Value::cons(mc, new_value, new_list);
            current = current.cdr();
        }

        Ok(new_list)
    }

    pub fn for_each<F>(self, mut f: F)
    where
        F: FnMut(Value<'gc>),
    {
        let mut current = self;

        while current.is_pair() {
            f(current.car());
            current = current.cdr();
        }
    }

    pub fn for_all<F>(self, mut f: F) -> bool
    where
        F: FnMut(Value<'gc>) -> bool,
    {
        let mut current = self;

        while current.is_pair() {
            if !f(current.car()) {
                return false;
            }
            current = current.cdr();
        }

        true
    }

    pub fn zip_with<F>(self, other: Value<'gc>, mc: Context<'gc>, mut f: F) -> Value<'gc>
    where
        F: FnMut(Value<'gc>, Value<'gc>) -> Value<'gc>,
    {
        let mut new_list = Value::null();
        let mut current_self = self;
        let mut current_other = other;

        while current_self.is_pair() && current_other.is_pair() {
            let new_value = f(current_self.car(), current_other.car());
            new_list = Value::cons(mc, new_value, new_list);
            current_self = current_self.cdr();
            current_other = current_other.cdr();
        }

        new_list
    }

    pub fn memv(self, value: Value<'gc>) -> bool {
        let mut current = self;

        while current.is_pair() {
            if current.car() == value {
                return true;
            }
            current = current.cdr();
        }

        false
    }

    pub fn memq(self, value: Value<'gc>) -> bool {
        let mut current = self;

        while current.is_pair() {
            if current.car() == value {
                return true;
            }
            current = current.cdr();
        }

        false
    }

    pub fn assp<F>(self, mut f: F) -> Option<Value<'gc>>
    where
        F: FnMut(Value<'gc>) -> bool,
    {
        let mut current = self;

        while current.is_pair() {
            if f(current.car()) {
                return Some(current.car());
            }
            current = current.cdr();
        }

        None
    }

    pub fn assq(self, id: Value<'gc>) -> Option<Value<'gc>> {
        self.assp(|x| x.car() == id)
    }
}

#[macro_export]
macro_rules! list {
    ($mc: expr, $($value: expr),*) => {

        {
            use $crate::runtime::value::IntoValue;
            let mut ls = $crate::runtime::value::Value::null();
            let mc = $mc;
            let mut _last =  $crate::runtime::value::Value::null();

            $(
                let value = $value;
                let pair =  $crate::runtime::value::Pair::new(mc,value.into_value(mc),  $crate::runtime::value::Value::null());
                if _last.is_null() {
                    ls =  $crate::runtime::value::Value::from(pair);
                } else {
                    _last.set_cdr(mc,  $crate::runtime::value::Value::from(pair));
                }
                _last =  $crate::runtime::value::Value::from(pair);
            )*

            ls
        }
    };

    ($mc: expr, $value: expr; $count: expr) => {
        {
            use $crate::runtime::value::IntoValue;
            let value = $value;
            let count = $count;
            let mut ls = Value::null();
            let mut last = Value::null();

            for _ in 0..count {
                let pair = Pair::new($mc, value.into_value($mc), Value::null());
                if last.is_null() {
                    ls = Value::from(pair);
                } else {
                    last.set_cdr($mc, Value::from(pair));
                }
                last = Value::from(pair);
            }

            ls
        }
    };

    ($mc: expr) => {
        Value::null()
    };
}

pub struct ListIterator<'gc> {
    current: Value<'gc>,
    index: usize,
}

impl<'gc> ListIterator<'gc> {
    pub fn new(list: Value<'gc>) -> Self {
        Self {
            current: list,
            index: 0,
        }
    }
}

impl<'gc> Iterator for ListIterator<'gc> {
    type Item = Value<'gc>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.is_null() || !self.current.is_pair() {
            return None;
        }

        let value = self.current.car();
        self.current = self.current.cdr();
        self.index += 1;

        Some(value)
    }
}

#[macro_export]
macro_rules! for_each {
    ($p: ident, $list: ident $b: block) => {
        $p = $list;
        while $p.is_pair() {
            $b;
            $p = $p.cdr();
        }
    };
}
