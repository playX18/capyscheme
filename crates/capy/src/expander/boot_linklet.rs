//! Boot files expanded as linklets.
//!
//! A file whose leading forms are `(|#%require| spec ...)` and/or
//! `(|#%provide| all-defined | sym ...)` is expanded as one linklet named
//! `(<dir> <file>)`, for example `lib/boot/prim.scm` is `(boot prim)`.
//! Free identifiers resolve to the file's own exports, then to its imports.
//! Anything else is a compile error.

use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use crate::{
    expander::{
        cenv::Cenv,
        core::{CompileError, Error, LVarRef, TermRef, constant, expand, fresh_lvar, linklet_term},
        datum_sourcev,
    },
    heap::alloc::array::Array,
    list,
    runtime::{
        MutationContext,
        linklet::{registry_lookup, root_registry},
        value::{Symbol, Value},
    },
};

/// What a linklet file exports.
pub enum Exports<'gc> {
    /// Every top-level definition that is not an imported name.
    AllDefined,
    /// Exactly these symbols.
    Explicit(Vec<Value<'gc>>),
}

/// The leading `#%require` and `#%provide` forms of a file.
pub struct BootHeader<'gc> {
    /// Import specs: `key`, `(only key sym ...)` or `(late key sym ...)`.
    pub requires: Vec<Value<'gc>>,
    pub exports: Exports<'gc>,
}

/// Linklet state of the file being expanded, stored in [`Cenv`].
pub struct BootLinklet<'gc> {
    key: Value<'gc>,
    /// Import groups as `(key sym ...)`, in order.
    groups: Vec<(Value<'gc>, Vec<Value<'gc>>)>,
    /// One slot per import symbol, in group order.
    import_slots: Vec<LVarRef<'gc>>,
    /// The visible import of each symbol. The last group importing it wins.
    imported: HashMap<Value<'gc>, (Value<'gc>, LVarRef<'gc>)>,
    /// The slots of the first import group, `(#%capy)`.
    capy_imported: HashMap<Value<'gc>, (Value<'gc>, LVarRef<'gc>)>,
    exports: Exports<'gc>,
    /// Top-level names defined by this file.
    defined: HashSet<Value<'gc>>,
    export_order: Vec<Value<'gc>>,
    export_slots: HashMap<Value<'gc>, LVarRef<'gc>>,
    /// Keys of the `(late key sym ...)` imports, see [`BootLinklet::finish`].
    late_keys: Vec<Value<'gc>>,
}

fn list_to_vec<'gc>(mut list: Value<'gc>) -> Vec<Value<'gc>> {
    let mut out = Vec::new();
    while list.is_pair() {
        out.push(list.car());
        list = list.cdr();
    }
    out
}

fn boot_error<'gc>(ctx: MutationContext<'gc>, form: Value<'gc>, message: String) -> Error<'gc> {
    Box::new(CompileError {
        message,
        irritants: vec![form],
        sourcev: datum_sourcev(ctx, form),
    })
}

/// Splits the leading `#%require` / `#%provide` forms off `forms`. Returns the
/// header and the number of forms it used, or `None` when the file has no header.
pub fn parse_header<'gc>(
    ctx: MutationContext<'gc>,
    forms: &[Value<'gc>],
) -> Result<Option<(BootHeader<'gc>, usize)>, Error<'gc>> {
    let require_sym: Value<'gc> = Symbol::from_str(ctx, "#%require").into();
    let provide_sym: Value<'gc> = Symbol::from_str(ctx, "#%provide").into();
    let all_sym: Value<'gc> = Symbol::from_str(ctx, "all-defined").into();

    let mut requires: Option<Vec<Value<'gc>>> = None;
    let mut exports: Option<Exports<'gc>> = None;
    let mut index = 0;

    while let Some(&form) = forms.get(index) {
        if !form.is_pair() {
            break;
        }
        let head = form.car();
        if head == require_sym {
            if requires.is_some() {
                return Err(boot_error(ctx, form, "duplicate #%require".to_string()));
            }
            requires = Some(list_to_vec(form.cdr()));
        } else if head == provide_sym {
            if exports.is_some() {
                return Err(boot_error(ctx, form, "duplicate #%provide".to_string()));
            }
            let items = list_to_vec(form.cdr());
            exports = Some(if items.len() == 1 && items[0] == all_sym {
                Exports::AllDefined
            } else {
                for item in &items {
                    if !item.is::<Symbol>() {
                        return Err(boot_error(
                            ctx,
                            form,
                            "#%provide takes all-defined or symbols".to_string(),
                        ));
                    }
                }
                Exports::Explicit(items)
            });
        } else {
            break;
        }
        index += 1;
    }

    if requires.is_none() && exports.is_none() {
        return Ok(None);
    }

    let header = BootHeader {
        requires: requires.unwrap_or_default(),
        exports: exports.unwrap_or(Exports::Explicit(Vec::new())),
    };
    Ok(Some((header, index)))
}

fn import_group<'gc>(
    ctx: MutationContext<'gc>,
    key: Value<'gc>,
    only: Option<Vec<Value<'gc>>>,
    form: Value<'gc>,
) -> Result<(Value<'gc>, Vec<Value<'gc>>), Error<'gc>> {
    let Some(inst) = registry_lookup(ctx, root_registry(ctx), key) else {
        return Err(boot_error(
            ctx,
            form,
            format!("boot linklet: missing library instance {}", key),
        ));
    };
    match only {
        None => {
            let mut syms: Vec<Value<'gc>> = inst.vars.keys().collect();
            syms.sort_by_cached_key(|sym| sym.to_string());
            Ok((key, syms))
        }
        Some(syms) => {
            for sym in &syms {
                if inst.variable(ctx, *sym).is_none() {
                    return Err(boot_error(
                        ctx,
                        form,
                        format!("boot linklet: {} does not export {}", key, sym),
                    ));
                }
            }
            Ok((key, syms))
        }
    }
}

/// Top-level names defined by `forms`, looking through `begin`.
fn collect_defined<'gc>(cenv: &Cenv<'gc>, forms: &[Value<'gc>], out: &mut Vec<Value<'gc>>) {
    for &form in forms {
        if !form.is_pair() {
            continue;
        }
        let head = form.car();
        if head == cenv.denotations.denotation_of_define {
            if !form.cdr().is_pair() {
                continue;
            }
            let mut target = form.cdr().car();
            while target.is_pair() {
                target = target.car();
            }
            if target.is::<Symbol>() {
                out.push(target);
            }
        } else if head == cenv.denotations.denotation_of_begin {
            collect_defined(cenv, &list_to_vec(form.cdr()), out);
        }
    }
}

impl<'gc> BootLinklet<'gc> {
    fn new(
        ctx: MutationContext<'gc>,
        key: Value<'gc>,
        groups: Vec<(Value<'gc>, Vec<Value<'gc>>)>,
        exports: Exports<'gc>,
        defined: Vec<Value<'gc>>,
    ) -> Self {
        let mut import_slots = Vec::new();
        let mut imported = HashMap::new();
        let mut capy_imported = HashMap::new();
        for (index, (group_key, syms)) in groups.iter().enumerate() {
            for sym in syms {
                let slot = fresh_lvar(ctx, *sym);
                import_slots.push(slot);
                imported.insert(*sym, (*group_key, slot));
                if index == 0 {
                    capy_imported.insert(*sym, (*group_key, slot));
                }
            }
        }

        let explicit = match &exports {
            Exports::Explicit(list) => list.clone(),
            Exports::AllDefined => Vec::new(),
        };

        let mut linklet = BootLinklet {
            key,
            groups,
            import_slots,
            imported,
            capy_imported,
            exports,
            defined: defined.into_iter().collect(),
            export_order: Vec::new(),
            export_slots: HashMap::new(),
            late_keys: Vec::new(),
        };
        for sym in explicit {
            linklet.export_slot(ctx, sym);
        }
        linklet
    }

    fn export_slot(&mut self, ctx: MutationContext<'gc>, sym: Value<'gc>) -> LVarRef<'gc> {
        if let Some(slot) = self.export_slots.get(&sym) {
            return *slot;
        }
        let slot = fresh_lvar(ctx, sym);
        self.export_slots.insert(sym, slot);
        self.export_order.push(sym);
        slot
    }

    fn is_exported(&self, sym: Value<'gc>) -> bool {
        match &self.exports {
            Exports::AllDefined => !self.imported.contains_key(&sym),
            Exports::Explicit(list) => list.contains(&sym),
        }
    }

    /// The `(key, slot)` a free top-level reference to `sym` refers to, if
    /// this file defines and exports it or imports it.
    pub fn resolve(
        &mut self,
        ctx: MutationContext<'gc>,
        sym: Value<'gc>,
    ) -> Option<(Value<'gc>, LVarRef<'gc>)> {
        if self.defined.contains(&sym) && self.is_exported(sym) {
            return Some((self.key, self.export_slot(ctx, sym)));
        }
        self.imported.get(&sym).copied()
    }

    /// The `(key, slot)` of the `(#%capy)` import `sym`, whatever else this file imports or
    /// defines under that name.
    pub fn capy_slot(&self, sym: Value<'gc>) -> Option<(Value<'gc>, LVarRef<'gc>)> {
        self.capy_imported.get(&sym).copied()
    }

    pub fn define_target(
        &mut self,
        ctx: MutationContext<'gc>,
        sym: Value<'gc>,
        form: Value<'gc>,
    ) -> Result<(Value<'gc>, LVarRef<'gc>), Error<'gc>> {
        self.defined.insert(sym);
        if self.is_exported(sym) {
            return Ok((self.key, self.export_slot(ctx, sym)));
        }
        if let Some(target) = self.imported.get(&sym).copied() {
            return Ok(target);
        }
        Err(boot_error(
            ctx,
            form,
            format!("boot linklet {}: define of unexported {}", self.key, sym),
        ))
    }

    fn finish(&self, ctx: MutationContext<'gc>, body: TermRef<'gc>) -> TermRef<'gc> {
        let groups: Vec<Value<'gc>> = self
            .groups
            .iter()
            .map(|(key, syms)| Value::cons(ctx, *key, Value::list_from_slice(ctx, syms)))
            .collect();
        let imports = Value::list_from_slice(ctx, groups);
        let exports = Value::list_from_slice(ctx, &self.export_order);

        let mut meta = Value::list_from_slice(ctx, &self.late_keys);
        meta = Value::cons(ctx, Value::new(false), meta);
        meta = Value::cons(ctx, Symbol::from_str(ctx, "#%boot").into(), meta);

        let mut slots = self.import_slots.clone();
        slots.extend(self.export_order.iter().map(|sym| self.export_slots[sym]));

        linklet_term(
            ctx,
            self.key,
            imports,
            exports,
            Array::from_slice(ctx, slots),
            body,
            meta,
            Value::new(false),
        )
    }
}

/// Expands the body of a boot file with `header` as one linklet.
pub fn expand_linklet<'gc>(
    cenv: &mut Cenv<'gc>,
    file: &Path,
    header: BootHeader<'gc>,
    forms: &[Value<'gc>],
) -> Result<TermRef<'gc>, Error<'gc>> {
    let ctx = cenv.ctx;

    let stem = file.file_stem().and_then(|s| s.to_str());
    let dir = file
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|d| d.to_str());
    let (Some(dir), Some(stem)) = (dir, stem) else {
        return Err(Box::new(CompileError {
            message: format!("cannot name boot linklet for {}", file.display()),
            irritants: vec![],
            sourcev: Value::new(false),
        }));
    };
    let dir_sym: Value<'gc> = Symbol::from_str(ctx, dir).into();
    let stem_sym: Value<'gc> = Symbol::from_str(ctx, stem).into();
    let key: Value<'gc> = list!(ctx, dir_sym, stem_sym);

    let capy_sym: Value<'gc> = Symbol::from_str(ctx, "#%capy").into();
    let capy_key: Value<'gc> = list!(ctx, capy_sym);
    let mut groups = vec![import_group(ctx, capy_key, None, Value::new(false))?];

    let only_sym: Value<'gc> = Symbol::from_str(ctx, "only").into();
    let late_sym: Value<'gc> = Symbol::from_str(ctx, "late").into();
    for spec in header.requires.iter().copied() {
        if spec.is_pair() && spec.car() == late_sym {
            // `(late key sym ...)`: the names come from a key that may not be
            // instantiated yet, so the registry is not consulted. The loader
            // hands out placeholder instances for such keys.
            let items = list_to_vec(spec.cdr());
            let Some((&group_key, syms)) = items.split_first() else {
                return Err(boot_error(ctx, spec, "empty (late ...) import".to_string()));
            };
            groups.push((group_key, syms.to_vec()));
        } else if spec.is_pair() && spec.car() == only_sym {
            let items = list_to_vec(spec.cdr());
            let Some((&group_key, syms)) = items.split_first() else {
                return Err(boot_error(ctx, spec, "empty (only ...) import".to_string()));
            };
            groups.push(import_group(ctx, group_key, Some(syms.to_vec()), spec)?);
        } else {
            groups.push(import_group(ctx, spec, None, spec)?);
        }
    }

    let mut late_keys: Vec<Value<'gc>> = Vec::new();
    for spec in header.requires.iter().copied() {
        if spec.is_pair() && spec.car() == late_sym {
            let key = spec.cdr().car();
            if !late_keys.iter().any(|k| k.r5rs_equal(key)) {
                late_keys.push(key);
            }
        }
    }

    let mut defined = Vec::new();
    collect_defined(cenv, forms, &mut defined);

    cenv.boot = Some(Box::new(BootLinklet::new(
        ctx,
        key,
        groups,
        header.exports,
        defined,
    )));

    cenv.boot.as_mut().expect("set above").late_keys = late_keys;

    let body = if forms.is_empty() {
        constant(ctx, Value::undefined())
    } else {
        let mut ls = Value::null();
        for form in forms.iter().rev() {
            ls = Value::cons(ctx, *form, ls);
        }
        ls = Value::cons(ctx, cenv.denotations.denotation_of_begin, ls);
        expand(cenv, ls)?
    };

    let linklet = cenv.boot.take().expect("boot linklet state is set above");
    Ok(linklet.finish(ctx, body))
}
