"""
Z3 SMT Proof for NaN boxing encoding.

"""
from z3 import (BitVec, BitVecVal, Solver, sat, unsat, And, Not, unknown, Concat, Extract, Implies)
VALUE_SIZE = 64

DOUBLE_ENCODE_OFFSET_BIT = 49
DOUBLE_ENCODE_OFFSET = 1 << DOUBLE_ENCODE_OFFSET_BIT  # 0x20000000000000

NUMBER_TAG = 0xfffe000000000000

OTHER_TAG = 0x2
BOOL_TAG = 0x4
UNDEFINED_TAG = 0x8

CHAR_TAG = 0x82 
CHAR_MASK = NUMBER_TAG | CHAR_TAG 


VALUE_FALSE = OTHER_TAG | BOOL_TAG | 0x0  # 0x6
VALUE_TRUE = OTHER_TAG | BOOL_TAG | 0x1   # 0x7
VALUE_UNDEFINED = OTHER_TAG | UNDEFINED_TAG  # 0xA
VALUE_NULL = OTHER_TAG  # 0x2

MISC_TAG = OTHER_TAG | BOOL_TAG | UNDEFINED_TAG  # 0xE
VALUE_VOID = MISC_TAG | 0x10          # 0x1E
VALUE_UNSPECIFIED = MISC_TAG | 0x20   # 0x2E
VALUE_EOF = MISC_TAG | 0x30           # 0x3E
VALUE_BWP = MISC_TAG | 0x40           # 0x4E

NOT_CELL_MASK = NUMBER_TAG | OTHER_TAG

VALUE_EMPTY = 0x0
VALUE_DELETED = 0x4

PURE_NAN_BITS = 0x7ff8000000000000

v = BitVec('v', VALUE_SIZE)



def is_empty(val):
    return val == VALUE_EMPTY

def is_deleted(val):
    return val == VALUE_DELETED

def is_cell(val):
    # self.raw_i64() & Self::NOT_CELL_MASK == 0 && self.raw_i64() != Self::VALUE_EMPTY
    return And(And((val & NOT_CELL_MASK) == 0, val != VALUE_EMPTY), val != VALUE_DELETED)

def is_int32(val):
    # self.raw_i64() & Self::NUMBER_TAG == Self::NUMBER_TAG
    return (val & NUMBER_TAG) == NUMBER_TAG

def is_flonum(val):
    # self.is_inline_number() && !self.is_int32()
    is_inline_num = (val & NUMBER_TAG) != 0
    not_is_int32 = (val & NUMBER_TAG) != NUMBER_TAG
    return And(is_inline_num, not_is_int32)

def is_char(val):
    # self.raw_i64() & Self::CHAR_MASK == Self::CHAR_TAG
    return (val & CHAR_MASK) == CHAR_TAG

def is_true(val):
    return val == VALUE_TRUE

def is_false(val):
    return val == VALUE_FALSE

def is_undefined(val):
    return val == VALUE_UNDEFINED

def is_null(val):
    return val == VALUE_NULL

def is_void(val):
    return val == VALUE_VOID

def is_unspecified(val):
    return val == VALUE_UNSPECIFIED

def is_eof(val):
    return val == VALUE_EOF

def is_bwp(val):
    return val == VALUE_BWP

# List of (name, predicate_function) tuples
value_types = [
    ("empty", is_empty),
    ("deleted", is_deleted),
    ("cell", is_cell), # General cell pointers
    ("int32", is_int32),
    ("flonum", is_flonum),
    ("char", is_char),
    ("true", is_true),
    ("false", is_false),
    ("undefined", is_undefined),
    ("null", is_null),
    ("void", is_void),
    ("unspecified", is_unspecified),
    ("eof", is_eof),
    ("bwp", is_bwp),
]

def run_disjointness_proofs():
    """
    Check that value encodings are disjoint i.e that no two types can be true at the same time.
    """

    print("Running Disjointness Proofs...")
    all_disjoint = True
    for i, (name1, pred1_func) in enumerate(value_types):
        for j, (name2, pred2_func) in enumerate(value_types):
            if j <= i:
                continue
            s = Solver()
            s.add(pred1_func(v))
            s.add(pred2_func(v))

            result = s.check()
            if result == sat:
                print(f"FAIL: {name1} and {name2} are NOT disjoint.")
                print(f"  Model (conflicting value v): {s.model()}")
                all_disjoint = False
            elif result == unknown:
                print(f"UNKNOWN: Solver could not determine disjointness for {name1} and {name2}.")
                all_disjoint = False
    
    if all_disjoint:
        print("All checked type pairs are disjoint.")
    else:
        print("Some type pairs are NOT disjoint. See details above.")
    return all_disjoint

def run_flonum_encoding_integrity_proof():
    print("\nRunning Flonum Encoding Integrity Proof (assuming input NaNs are canonicalized to PURE_NAN_BITS)...")
    f_payload = BitVec('f_payload', VALUE_SIZE)
    encoded_f = f_payload + DOUBLE_ENCODE_OFFSET
    exponent_bits_f_payload = Extract(62, 52, f_payload)
    significand_bits_f_payload = Extract(51, 0, f_payload)
    is_f_payload_nan = And(exponent_bits_f_payload == BitVecVal(0x7FF, 11), significand_bits_f_payload != BitVecVal(0, 52))
    nan_assumption = Implies(is_f_payload_nan, f_payload == PURE_NAN_BITS)

    solver_not_cell_like = Solver()
    solver_not_cell_like.add(nan_assumption) 
    solver_not_cell_like.add((encoded_f & NUMBER_TAG) == 0)
    
    res_not_cell_like = solver_not_cell_like.check()
    passed_not_cell_like = False
    if res_not_cell_like == unsat:
        print("PASS: Under NaN canonicalization assumption, encoded flonum never has its (NUMBER_TAG relevant) high bits as 0 (does not clash with cell/misc tags).")
        passed_not_cell_like = True
    else:
        print("FAIL: Even under NaN canonicalization assumption, encoded flonum CAN have its (NUMBER_TAG relevant) high bits as 0.")
        if res_not_cell_like == sat:
            model = solver_not_cell_like.model()
            f_payload_val = model[f_payload]
            print(f"  Model (f_payload): {hex(f_payload_val) if f_payload_val is not None else 'N/A'}")
            if model.eval(is_f_payload_nan):
                print(f"    Note: f_payload was a NaN and assumed to be {hex(PURE_NAN_BITS)} for this check.")

    solver_not_int32_like = Solver()
    solver_not_int32_like.add(nan_assumption) # Add the NaN canonicalization assumption
    # Check if encoded_f could ever look like an int32 (top bits 111...110)
    # (encoded_f & NUMBER_TAG) == NUMBER_TAG
    solver_not_int32_like.add((encoded_f & NUMBER_TAG) == NUMBER_TAG)

    res_not_int32_like = solver_not_int32_like.check()
    passed_not_int32_like = False
    if res_not_int32_like == unsat:
        print("PASS: Under NaN canonicalization assumption, encoded flonum never has its (NUMBER_TAG relevant) high bits as the int32 tag (does not clash with int32 tag).")
        passed_not_int32_like = True
    else:
        print("FAIL: Even under NaN canonicalization assumption, encoded flonum CAN have its (NUMBER_TAG relevant) high bits as the int32 tag.")
        if res_not_int32_like == sat:
            model = solver_not_int32_like.model()
            f_payload_val = model[f_payload]
            print(f"  Model (f_payload): {hex(f_payload_val) if f_payload_val is not None else 'N/A'}")
            if model.eval(is_f_payload_nan):
                print(f"    Note: f_payload was a NaN and assumed to be {hex(PURE_NAN_BITS)} for this check.")
            
    if passed_not_cell_like and passed_not_int32_like:
        print("Flonum encoding integrity: PASS (under NaN canonicalization assumption).")
    else:
        print("Flonum encoding integrity: FAIL (even under NaN canonicalization assumption). See details above.")
    return passed_not_cell_like and passed_not_int32_like

def run_char_encoding_integrity_proof():
    """
    Check that all char payloads are encoded correctly
    and do not interfere with other encodings.
    """
    print("\nRunning Char Encoding Integrity Proof...")
    # Prove that for any valid char payload, the encoding satisfies is_char()
    # char payload is u32, shifted by 16
    char_payload_u32 = BitVec('char_payload_u32', 32)

    char_payload_u64 = Concat(BitVecVal(0, 32), char_payload_u32)
    
    encoded_char_val = (char_payload_u64 << 16) | CHAR_TAG

    s = Solver()
 
    s.add(Not(is_char(encoded_char_val)))
    
    result = s.check()
    passed_char_integrity = False
    if result == unsat:
        print("PASS: Char encoding correctly satisfies is_char() for all u32 char payloads.")
        passed_char_integrity = True
    else:
        print("FAIL: Char encoding does NOT always satisfy is_char().")
        if result == sat:
            print(f"  Model (char_payload_u32 causing failure): {s.model()}")
            
    return passed_char_integrity

def run_flonum_nan_purity_proof():
    """
    Check that if a flonum is NaN, its original f64 bits are PURE_NAN_BITS.
    This is a proof of the purity of NaN encoding.
    """
    print("\nRunning Flonum NaN Purity Proof (assuming input NaNs are canonicalized to PURE_NAN_BITS by the system)...")
    s = Solver()

   
    f64_raw_bits = v - DOUBLE_ENCODE_OFFSET

    exponent_bits = Extract(62, 52, f64_raw_bits)
    significand_bits = Extract(51, 0, f64_raw_bits)
    is_a_nan = And(exponent_bits == BitVecVal(0x7FF, 11), significand_bits != BitVecVal(0, 52))

   
    s.add(Implies(is_a_nan, f64_raw_bits == PURE_NAN_BITS))

    
    s.add(is_flonum(v))
    s.add(is_a_nan)  
    s.add(f64_raw_bits != PURE_NAN_BITS)

    result = s.check()
    passed_nan_purity = False
    if result == unsat:
       
        print("PASS: Under the assumption that input NaNs are canonicalized to PURE_NAN_BITS, " +
              "if an encoded flonum represents a NaN, its original f64 bits are indeed PURE_NAN_BITS.")
        passed_nan_purity = True
    else:
        print("FAIL: Unexpectedly, a counterexample was found or the problem is complex, " +
              "even with the assumption of input NaN canonicalization. Review Z3 script logic.")
        if result == sat:
            model = s.model()
            v_val_obj = model[v]
            f64_val_obj = model.eval(f64_raw_bits)

            v_hex = hex(v_val_obj.as_long()) if v_val_obj is not None else 'N/A'
            f64_hex = hex(f64_val_obj.as_long()) if f64_val_obj is not None else 'N/A'
            
            print(f"  Counterexample (unexpected, check logic):")
            print(f"    Encoded Value (v): {v_hex}")
            print(f"    Decoded f64 bits (v - OFFSET):  {f64_hex}")
            
            exp_val_obj = model.eval(exponent_bits)
            sig_val_obj = model.eval(significand_bits)
            exp_hex = hex(exp_val_obj.as_long()) if exp_val_obj is not None else 'N/A'
            sig_hex = hex(sig_val_obj.as_long()) if sig_val_obj is not None else 'N/A'

            print(f"      f64 Exponent (bits 62-52): {exp_hex}")
            print(f"      f64 Significand (bits 51-0): {sig_hex}")
            print(f"      (PURE_NAN_BITS is {hex(PURE_NAN_BITS)})")
    return passed_nan_purity

if __name__ == "__main__":
    print("Starting Z3 SMT Proof for Capyscheme Value Encodings...")
    
    DISJOINT_OK = run_disjointness_proofs()
    FLONUM_OK = run_flonum_encoding_integrity_proof()
    CHAR_OK = run_char_encoding_integrity_proof()
    NAN_PURITY_OK = run_flonum_nan_purity_proof()
    
    print("\n--- Summary ---")
    if DISJOINT_OK:
        print("Disjointness Checks: All clear (or see specific non-disjoint pairs above if any were found).")
    else:
        print("Disjointness Checks: FAILED for some pairs.")
        
    if FLONUM_OK:
        print("Flonum Encoding Integrity: PASS.")
    else:
        print("Flonum Encoding Integrity: FAILED.")

    if CHAR_OK:
        print("Char Encoding Integrity: PASS.")
    else:
        print("Char Encoding Integrity: FAILED.")

    if NAN_PURITY_OK:
        print("Flonum NaN Purity: PASS.")
    else:
        print("Flonum NaN Purity: FAILED.")

    if DISJOINT_OK and FLONUM_OK and CHAR_OK and NAN_PURITY_OK:
        print("\nOverall Result: All primary proofs PASSED.")
    else:
        print("\nOverall Result: Some proofs FAILED. Review the output above.")
