use crate::{
    cps::term::{Atom, ContRef, Expression, FuncRef, Term, TermRef, Throw},
    expander::core::LVarRef,
};
use hashlink::LinkedHashMap as HashMap;
use hashlink::LinkedHashSet as HashSet;

type Vars<'gc> = HashSet<LVarRef<'gc>>;

pub fn get_fvt<'gc>(term: TermRef<'gc>, fv: &mut FreeVars<'gc>) -> HashSet<LVarRef<'gc>> {
    match *term {
        Term::Let(bind, expr, body) => match expr {
            Expression::PrimCall(_, args, h, _) => {
                let mut map: Vars = args.iter().copied().flat_map(get_fva).collect();
                map.insert(h);
                map.union(&get_fvt(body, fv))
                    .filter(|v| **v != bind)
                    .copied()
                    .collect()
            }

            _ => HashSet::new(),
        },

        Term::Letk(conts, body) => {
            let map = conts.iter().fold(HashSet::new(), |mut acc, cont| {
                let free = get_fvc(*cont, fv);
                fv.cvars.insert(*cont, free.clone());
                fv.conts.insert(cont.binding(), *cont);
                acc.extend(free);
                acc
            });
            map.union(&get_fvt(body, fv))
                .filter(|v| conts.iter().all(|c| c.binding() != **v))
                .copied()
                .collect()
        }

        Term::Fix(funcs, body) => funcs
            .iter()
            .fold(HashSet::new(), |mut acc, func| {
                let free = get_fvf(*func, fv);
                fv.fvars.insert(*func, free.clone());
                fv.funcs.insert(func.binding, *func);
                acc.extend(free);
                acc
            })
            .union(&get_fvt(body, fv))
            .filter(|v| funcs.iter().all(|f| f.binding != **v))
            .copied()
            .collect(),

        Term::Continue(k, args, _) => args
            .iter()
            .copied()
            .flat_map(get_fva)
            .chain(std::iter::once(k))
            .collect(),

        Term::App(func, k, h, args, _) => {
            fv.cvals.insert(k);
            fv.cvals.insert(h);
            args.iter()
                .copied()
                .flat_map(get_fva)
                .chain(std::iter::once(func).flat_map(get_fva))
                .chain(std::iter::once(k))
                .chain(std::iter::once(h))
                .collect()
        }
        Term::Throw(throw, _) => match throw {
            Throw::Throw(key, args) | Throw::Value(key, args) | Throw::ValueAndData(key, args) => {
                get_fva(key)
                    .into_iter()
                    .chain(get_fva(args).into_iter())
                    .collect()
            }
        },

        Term::If {
            test,
            consequent,
            consequent_args,
            alternative,
            alternative_args,
            ..
        } => get_fva(test)
            .into_iter()
            .chain(Some(consequent).into_iter())
            .chain(Some(alternative).into_iter())
            .chain(
                consequent_args
                    .iter()
                    .flat_map(|args| args.iter().copied().flat_map(get_fva)),
            )
            .chain(
                alternative_args
                    .iter()
                    .flat_map(|args| args.iter().copied().flat_map(get_fva)),
            )
            .collect(),
    }
}

fn get_fva<'gc>(atom: Atom<'gc>) -> Vec<LVarRef<'gc>> {
    match atom {
        Atom::Local(lvar) => vec![lvar],
        Atom::Values(atoms) => atoms.iter().copied().flat_map(get_fva).collect(),
        _ => Vec::new(),
    }
}

fn get_fvc<'gc>(cont: ContRef<'gc>, fv: &mut FreeVars<'gc>) -> Vars<'gc> {
    let mut map = get_fvt(cont.body, fv);
    map.insert(cont.handler.get());

    for arg in cont.args.iter().chain(cont.variadic.iter()) {
        map.remove(arg);
    }
    map.remove(&cont.binding);
    assert!(map.contains(&cont.handler.get()));
    map
}

pub fn get_fvf<'gc>(func: FuncRef<'gc>, fv: &mut FreeVars<'gc>) -> Vars<'gc> {
    fv.funcs.insert(func.binding, func);
    let mut map = get_fvt(func.body, fv);
    for arg in func.args.iter().chain(func.variadic.iter()) {
        map.remove(arg);
    }
    map.remove(&func.return_cont);
    map.remove(&func.handler_cont);

    map
}

pub struct FreeVars<'gc> {
    pub fvars: HashMap<FuncRef<'gc>, Vars<'gc>>,
    pub cvars: HashMap<ContRef<'gc>, Vars<'gc>>,

    pub funcs: HashMap<LVarRef<'gc>, FuncRef<'gc>>,
    pub conts: HashMap<LVarRef<'gc>, ContRef<'gc>>,
    /// Set of continuations that are used as values.
    pub cvals: HashSet<LVarRef<'gc>>,
}

impl<'gc> FreeVars<'gc> {
    pub fn new() -> Self {
        FreeVars {
            fvars: HashMap::new(),
            cvars: HashMap::new(),
            funcs: HashMap::new(),
            conts: HashMap::new(),
            cvals: HashSet::new(),
        }
    }
}
