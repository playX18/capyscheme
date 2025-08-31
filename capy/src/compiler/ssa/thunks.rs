use cranelift_codegen::{
    ir::{self, FuncRef, Function},
    isa::CallConv,
};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use rsgc::object::{VTable, VTableOf};

use crate::runtime::value::{Boxed, Pair, Vector};
pub struct Thunks {
    pub wrong_number_of_args: FuncId,
    pub cons_rest: FuncId,
    pub non_applicable: FuncId,
    pub lookup: FuncId,
    pub lookup_bound: FuncId,
    pub current_module: FuncId,
    pub define: FuncId,
    pub make_box: FuncId,
    pub cons: FuncId,
    pub make_closure: FuncId,
    pub fasl_read_nofail: FuncId,
    pub yieldpoint: FuncId,
    pub alloc_tc8: FuncId,
    pub alloc_tc16: FuncId,
    pub reverse: FuncId,
    pub append: FuncId,
    pub eqv: FuncId,
    pub equal: FuncId,
}

impl Thunks {
    pub fn new<'gc, M: Module>(module: &mut M) -> Self {
        let sig = call_signature!(SystemV (I64, I64) -> I64);

        let wrong_number_of_args = module
            .declare_function("capy_wrong_number_of_args", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64, I64) -> I64);

        let cons_rest = module
            .declare_function("capy_cons_rest", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64) -> I64);

        let non_applicable = module
            .declare_function("capy_non_applicable", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64) -> I64);

        let cons = module
            .declare_function("capy_cons", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64) -> (I64, I64));

        let lookup_bound = module
            .declare_function("capy_lookup_bound", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64) -> (I64, I64));

        let lookup = module
            .declare_function("capy_lookup", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV ( I64) -> I64);

        let current_module = module
            .declare_function("capy_current_module", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64) -> I64);

        let define = module
            .declare_function("capy_define", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64) -> I64);

        let make_box = module
            .declare_function("capy_make_box", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64, I64) -> I64);

        let make_closure = module
            .declare_function("capy_make_closure", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64) -> I64);
        let fasl_read_nofail = module
            .declare_function("capy_fasl_read_nofail", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64, I64) -> I64);
        let yieldpoint = module
            .declare_function("capy_yieldpoint", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I8, I64) -> I64);
        let alloc_tc8 = module
            .declare_function("capy_alloc_tc8", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I16, I64) -> I64);
        let alloc_tc16 = module
            .declare_function("capy_alloc_tc16", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64) -> I64);
        let reverse = module
            .declare_function("capy_reverse", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64, I64) -> I64);
        let append = module
            .declare_function("capy_append", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64) -> I64);
        let eqv = module
            .declare_function("capy_eqv", Linkage::Import, &sig)
            .unwrap();

        let sig = call_signature!(SystemV (I64, I64) -> I64);
        let equal = module
            .declare_function("capy_equal", Linkage::Import, &sig)
            .unwrap();

        Self {
            eqv,
            equal,
            append,
            reverse,
            yieldpoint,
            wrong_number_of_args,
            cons_rest,
            non_applicable,
            lookup,
            lookup_bound,
            fasl_read_nofail,
            current_module,
            define,
            make_box,
            cons,
            make_closure,
            alloc_tc16,
            alloc_tc8,
        }
    }
}

pub struct ImportedThunks {
    pub wrong_number_of_args: FuncRef,
    pub cons_rest: FuncRef,
    pub non_applicable: FuncRef,
    pub lookup: FuncRef,
    pub lookup_bound: FuncRef,
    pub current_module: FuncRef,
    pub define: FuncRef,
    pub make_box: FuncRef,
    pub cons: FuncRef,
    pub make_closure: FuncRef,
    pub yieldpoint: FuncRef,
    pub alloc_tc8: FuncRef,
    pub alloc_tc16: FuncRef,
    pub reverse: FuncRef,
    pub append: FuncRef,
    pub eqv: FuncRef,
    pub equal: FuncRef,
}

impl ImportedThunks {
    pub fn new<'gc>(thunks: &Thunks, function: &mut Function, module: &mut ObjectModule) -> Self {
        let wrong_number_of_args =
            module.declare_func_in_func(thunks.wrong_number_of_args, function);
        let cons_rest = module.declare_func_in_func(thunks.cons_rest, function);
        let non_applicable = module.declare_func_in_func(thunks.non_applicable, function);
        let lookup = module.declare_func_in_func(thunks.lookup, function);
        let lookup_bound = module.declare_func_in_func(thunks.lookup_bound, function);
        let current_module = module.declare_func_in_func(thunks.current_module, function);
        let define = module.declare_func_in_func(thunks.define, function);
        let make_box = module.declare_func_in_func(thunks.make_box, function);
        let cons = module.declare_func_in_func(thunks.cons, function);
        let make_closure = module.declare_func_in_func(thunks.make_closure, function);
        let yieldpoint = module.declare_func_in_func(thunks.yieldpoint, function);
        let alloc_tc8 = module.declare_func_in_func(thunks.alloc_tc8, function);
        let alloc_tc16 = module.declare_func_in_func(thunks.alloc_tc16, function);
        let reverse = module.declare_func_in_func(thunks.reverse, function);
        let append = module.declare_func_in_func(thunks.append, function);
        let eqv = module.declare_func_in_func(thunks.eqv, function);
        let equal = module.declare_func_in_func(thunks.equal, function);

        Self {
            eqv,
            equal,
            append,
            reverse,
            alloc_tc16,
            alloc_tc8,
            cons_rest,
            wrong_number_of_args,
            non_applicable,
            lookup,
            lookup_bound,
            current_module,
            define,
            make_box,
            cons,
            make_closure,
            yieldpoint,
        }
    }
}

#[unsafe(no_mangle)]
pub static BOX_VTABLE: &'static VTable = &VTableOf::<Boxed>::VT;
#[unsafe(no_mangle)]
pub static PAIR_VTABLE: &'static VTable = &VTableOf::<Pair>::VT;
#[unsafe(no_mangle)]
pub static VECTOR_VTABLE: &'static VTable = &Vector::VT;
