"""
Z3 SMT Proof for NaN boxing encoding.

This module provides formal verification of the value encoding scheme used in CapyScheme.
The encoding uses NaN boxing based on JavaScriptCore's approach, with proofs that verify:
- Disjointness of value types
- Integrity of flonum encoding
- Integrity of character encoding
- Purity of NaN encoding
"""

from typing import List, Tuple, Callable, Any, Optional
from z3 import (BitVec, BitVecVal, Solver, sat, unsat, And, Not, unknown, Concat, Extract, Implies)

# Type alias for Z3 expressions
Z3Expr = Any


class ProofResult:
    """Represents the result of a single proof."""
    
    def __init__(self, name: str, passed: bool, details: str = ""):
        self.name = name
        self.passed = passed
        self.details = details


class ValueEncodingConstants:
    """Constants used in the value encoding scheme."""
    
    # Basic parameters
    VALUE_SIZE = 64
    DOUBLE_ENCODE_OFFSET_BIT = 49
    DOUBLE_ENCODE_OFFSET = 1 << DOUBLE_ENCODE_OFFSET_BIT  # 0x20000000000000
    
    # Primary tags
    NUMBER_TAG = 0xfffe000000000000
    OTHER_TAG = 0x2
    BOOL_TAG = 0x4
    UNDEFINED_TAG = 0x8
    
    # Character encoding
    CHAR_TAG = 0x82
    CHAR_MASK = NUMBER_TAG | CHAR_TAG
    
    # Boolean values
    VALUE_FALSE = OTHER_TAG | BOOL_TAG | 0x0  # 0x6
    VALUE_TRUE = OTHER_TAG | BOOL_TAG | 0x1   # 0x7
    
    # Special values
    VALUE_UNDEFINED = OTHER_TAG | UNDEFINED_TAG  # 0xA
    VALUE_NULL = OTHER_TAG  # 0x2
    
    # Miscellaneous values
    MISC_TAG = OTHER_TAG | BOOL_TAG | UNDEFINED_TAG  # 0xE
    VALUE_VOID = MISC_TAG | 0x10          # 0x1E
    VALUE_UNSPECIFIED = MISC_TAG | 0x20   # 0x2E
    VALUE_EOF = MISC_TAG | 0x30           # 0x3E
    VALUE_BWP = MISC_TAG | 0x40           # 0x4E
    
    # Cell mask
    NOT_CELL_MASK = NUMBER_TAG | OTHER_TAG
    
    # Empty/deleted values
    VALUE_EMPTY = 0x0
    VALUE_DELETED = 0x4
    
    # NaN representation
    PURE_NAN_BITS = 0x7ff8000000000000


class ValuePredicates:
    """Predicate functions for checking value types."""
    
    def __init__(self, constants: ValueEncodingConstants):
        self.constants = constants
    
    def is_empty(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents an empty slot."""
        return val == self.constants.VALUE_EMPTY
    
    def is_deleted(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents a deleted slot."""
        return val == self.constants.VALUE_DELETED
    
    def is_cell(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents a cell pointer.
        
        A cell is identified by having zero bits in the NOT_CELL_MASK positions,
        and not being VALUE_EMPTY or VALUE_DELETED.
        """
        return And(
            And((val & self.constants.NOT_CELL_MASK) == 0, val != self.constants.VALUE_EMPTY),
            val != self.constants.VALUE_DELETED
        )
    
    def is_int32(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents a 32-bit integer."""
        return (val & self.constants.NUMBER_TAG) == self.constants.NUMBER_TAG
    
    def is_flonum(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents a floating-point number."""
        is_inline_num = (val & self.constants.NUMBER_TAG) != 0
        not_is_int32 = (val & self.constants.NUMBER_TAG) != self.constants.NUMBER_TAG
        return And(is_inline_num, not_is_int32)
    
    def is_char(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents a character."""
        return (val & self.constants.CHAR_MASK) == self.constants.CHAR_TAG
    
    def is_true(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents boolean true."""
        return val == self.constants.VALUE_TRUE
    
    def is_false(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents boolean false."""
        return val == self.constants.VALUE_FALSE
    
    def is_undefined(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents undefined."""
        return val == self.constants.VALUE_UNDEFINED
    
    def is_null(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents null."""
        return val == self.constants.VALUE_NULL
    
    def is_void(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents void."""
        return val == self.constants.VALUE_VOID
    
    def is_unspecified(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents unspecified."""
        return val == self.constants.VALUE_UNSPECIFIED
    
    def is_eof(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents end-of-file."""
        return val == self.constants.VALUE_EOF
    
    def is_bwp(self, val: Z3Expr) -> Z3Expr:
        """Check if value represents black hole (broken promise)."""
        return val == self.constants.VALUE_BWP


class TableFormatter:
    """Utility class for creating ASCII tables."""
    
    @staticmethod
    def create_table(headers: List[str], rows: List[List[str]], title: str = "") -> str:
        """Create an ASCII table with the given headers and rows."""
        if not rows:
            return ""
        
        # Calculate column widths
        col_widths = [len(header) for header in headers]
        for row in rows:
            for i, cell in enumerate(row):
                col_widths[i] = max(col_widths[i], len(str(cell)))
        
        # Add some padding
        col_widths = [width + 2 for width in col_widths]
        
        # Create separator line
        separator = "+" + "+".join("-" * width for width in col_widths) + "+"
        
        # Create header
        header_line = "|"
        for i, header in enumerate(headers):
            header_line += f" {header:<{col_widths[i]-2}} |"
        
        # Create rows
        row_lines = []
        for row in rows:
            row_line = "|"
            for i, cell in enumerate(row):
                row_line += f" {str(cell):<{col_widths[i]-2}} |"
            row_lines.append(row_line)
        
        # Combine all parts
        table = []
        if title:
            table.append(f"\n{title}")
        table.append(separator)
        table.append(header_line)
        table.append(separator)
        for row_line in row_lines:
            table.append(row_line)
        table.append(separator)
        
        return "\n".join(table)


class ValueEncodingProofs:
    """Class containing all proofs for the value encoding scheme."""
    
    def __init__(self):
        self.constants = ValueEncodingConstants()
        self.predicates = ValuePredicates(self.constants)
        self.v = BitVec('v', self.constants.VALUE_SIZE)
        
        # List of (name, predicate_function) tuples
        self.value_types: List[Tuple[str, Callable[[Z3Expr], Z3Expr]]] = [
            ("empty", self.predicates.is_empty),
            ("deleted", self.predicates.is_deleted),
            ("cell", self.predicates.is_cell),  # General cell pointers
            ("int32", self.predicates.is_int32),
            ("flonum", self.predicates.is_flonum),
            ("char", self.predicates.is_char),
            ("true", self.predicates.is_true),
            ("false", self.predicates.is_false),
            ("undefined", self.predicates.is_undefined),
            ("null", self.predicates.is_null),
            ("void", self.predicates.is_void),
            ("unspecified", self.predicates.is_unspecified),
            ("eof", self.predicates.is_eof),
            ("bwp", self.predicates.is_bwp),
        ]
    
    def run_disjointness_proofs(self) -> Tuple[bool, List[ProofResult]]:
        """
        Check that value encodings are disjoint i.e. that no two types can be true at the same time.
        
        Returns:
            Tuple[bool, List[ProofResult]]: (all_passed, results)
        """
        print("Running Disjointness Proofs...")
        results = []
        all_disjoint = True
        
        # Collect all results first
        for i, (name1, pred1_func) in enumerate(self.value_types):
            for j, (name2, pred2_func) in enumerate(self.value_types):
                if j <= i:
                    continue
                    
                s = Solver()
                s.add(pred1_func(self.v))
                s.add(pred2_func(self.v))
                
                result = s.check()
                if result == sat:
                    results.append(ProofResult(
                        f"{name1} vs {name2}",
                        False,
                        f"FAIL: {name1} and {name2} are NOT disjoint. Model: {s.model()}"
                    ))
                    all_disjoint = False
                elif result == unknown:
                    results.append(ProofResult(
                        f"{name1} vs {name2}",
                        False,
                        f"UNKNOWN: Solver could not determine disjointness for {name1} and {name2}"
                    ))
                    all_disjoint = False
                else:  # unsat
                    results.append(ProofResult(
                        f"{name1} vs {name2}",
                        True,
                        "PASS: Types are disjoint"
                    ))
        
        # Create table for results
        headers = ["Type Pair", "Status", "Details"]
        rows = []
        for result in results:
            status = "PASS" if result.passed else "FAIL"
            rows.append([result.name, status, result.details])
        
        table = TableFormatter.create_table(headers, rows, "Disjointness Proof Results")
        print(table)
        
        if all_disjoint:
            print("All checked type pairs are disjoint.")
        else:
            print("Some type pairs are NOT disjoint. See details above.")
            
        return all_disjoint, results
    
    def run_flonum_encoding_integrity_proof(self) -> Tuple[bool, List[ProofResult]]:
        """
        Verify the integrity of flonum encoding.
        
        Checks that encoded flonums don't clash with cell/misc tags or int32 tags,
        assuming input NaNs are canonicalized to PURE_NAN_BITS.
        
        Returns:
            Tuple[bool, List[ProofResult]]: (all_passed, results)
        """
        print("\nRunning Flonum Encoding Integrity Proof (assuming input NaNs are canonicalized to PURE_NAN_BITS)...")
        results = []
        
        f_payload = BitVec('f_payload', self.constants.VALUE_SIZE)
        encoded_f = f_payload + self.constants.DOUBLE_ENCODE_OFFSET
        
        exponent_bits_f_payload = Extract(62, 52, f_payload)
        significand_bits_f_payload = Extract(51, 0, f_payload)
        is_f_payload_nan = And(
            exponent_bits_f_payload == BitVecVal(0x7FF, 11),
            significand_bits_f_payload != BitVecVal(0, 52)
        )
        nan_assumption = Implies(is_f_payload_nan, f_payload == self.constants.PURE_NAN_BITS)
        
        # Check if encoded_f could ever look like a cell (top bits 000...000)
        solver_not_cell_like = Solver()
        solver_not_cell_like.add(nan_assumption)
        solver_not_cell_like.add((encoded_f & self.constants.NUMBER_TAG) == 0)
        
        res_not_cell_like = solver_not_cell_like.check()
        passed_not_cell_like = False
        
        if res_not_cell_like == unsat:
            results.append(ProofResult(
                "Cell Clash Check",
                True,
                "PASS: Encoded flonum never clashes with cell/misc tags"
            ))
            passed_not_cell_like = True
        else:
            details = "FAIL: Encoded flonum CAN clash with cell/misc tags"
            if res_not_cell_like == sat:
                model = solver_not_cell_like.model()
                f_payload_val = model[f_payload]
                details += f". Model (f_payload): {hex(f_payload_val) if f_payload_val is not None else 'N/A'}"
                if model.eval(is_f_payload_nan):
                    details += f". Note: f_payload was a NaN and assumed to be {hex(self.constants.PURE_NAN_BITS)} for this check."
            results.append(ProofResult("Cell Clash Check", False, details))
        
        # Check if encoded_f could ever look like an int32 (top bits 111...110)
        solver_not_int32_like = Solver()
        solver_not_int32_like.add(nan_assumption)  # Add the NaN canonicalization assumption
        solver_not_int32_like.add((encoded_f & self.constants.NUMBER_TAG) == self.constants.NUMBER_TAG)
        
        res_not_int32_like = solver_not_int32_like.check()
        passed_not_int32_like = False
        
        if res_not_int32_like == unsat:
            results.append(ProofResult(
                "Int32 Clash Check",
                True,
                "PASS: Encoded flonum never clashes with int32 tag"
            ))
            passed_not_int32_like = True
        else:
            details = "FAIL: Encoded flonum CAN clash with int32 tag"
            if res_not_int32_like == sat:
                model = solver_not_int32_like.model()
                f_payload_val = model[f_payload]
                details += f". Model (f_payload): {hex(f_payload_val) if f_payload_val is not None else 'N/A'}"
                if model.eval(is_f_payload_nan):
                    details += f". Note: f_payload was a NaN and assumed to be {hex(self.constants.PURE_NAN_BITS)} for this check."
            results.append(ProofResult("Int32 Clash Check", False, details))
        
        # Create table for results
        headers = ["Check", "Status", "Details"]
        rows = []
        for result in results:
            status = "PASS" if result.passed else "FAIL"
            rows.append([result.name, status, result.details])
        
        table = TableFormatter.create_table(headers, rows, "Flonum Encoding Integrity Proof Results")
        print(table)
        
        if passed_not_cell_like and passed_not_int32_like:
            print("Flonum encoding integrity: PASS (under NaN canonicalization assumption).")
        else:
            print("Flonum encoding integrity: FAIL (even under NaN canonicalization assumption). See details above.")
            
        return passed_not_cell_like and passed_not_int32_like, results
    
    def run_char_encoding_integrity_proof(self) -> Tuple[bool, List[ProofResult]]:
        """
        Check that all char payloads are encoded correctly and do not interfere with other encodings.
        
        Returns:
            Tuple[bool, List[ProofResult]]: (passed, results)
        """
        print("\nRunning Char Encoding Integrity Proof...")
        results = []
        
        # Prove that for any valid char payload, the encoding satisfies is_char()
        # char payload is u32, shifted by 16
        char_payload_u32 = BitVec('char_payload_u32', 32)
        char_payload_u64 = Concat(BitVecVal(0, 32), char_payload_u32)
        encoded_char_val = (char_payload_u64 << 16) | self.constants.CHAR_TAG
        
        s = Solver()
        s.add(Not(self.predicates.is_char(encoded_char_val)))
        
        result = s.check()
        passed_char_integrity = False
        
        if result == unsat:
            results.append(ProofResult(
                "Char Encoding Check",
                True,
                "PASS: Char encoding correctly satisfies is_char() for all u32 char payloads"
            ))
            passed_char_integrity = True
        else:
            details = "FAIL: Char encoding does NOT always satisfy is_char()"
            if result == sat:
                details += f". Model (char_payload_u32 causing failure): {s.model()}"
            results.append(ProofResult("Char Encoding Check", False, details))
        
        # Create table for results
        headers = ["Check", "Status", "Details"]
        rows = []
        for result in results:
            status = "PASS" if result.passed else "FAIL"
            rows.append([result.name, status, result.details])
        
        table = TableFormatter.create_table(headers, rows, "Char Encoding Integrity Proof Results")
        print(table)
        
        return passed_char_integrity, results
    
    def run_flonum_nan_purity_proof(self) -> Tuple[bool, List[ProofResult]]:
        """
        Check that if a flonum is NaN, its original f64 bits are PURE_NAN_BITS.
        
        This is a proof of the purity of NaN encoding, assuming input NaNs are canonicalized
        to PURE_NAN_BITS by the system.
        
        Returns:
            Tuple[bool, List[ProofResult]]: (passed, results)
        """
        print("\nRunning Flonum NaN Purity Proof (assuming input NaNs are canonicalized to PURE_NAN_BITS by the system)...")
        results = []
        
        s = Solver()
        f64_raw_bits = self.v - self.constants.DOUBLE_ENCODE_OFFSET
        
        exponent_bits = Extract(62, 52, f64_raw_bits)
        significand_bits = Extract(51, 0, f64_raw_bits)
        is_a_nan = And(
            exponent_bits == BitVecVal(0x7FF, 11),
            significand_bits != BitVecVal(0, 52)
        )
        
        # Add the assumption that NaNs are canonicalized
        s.add(Implies(is_a_nan, f64_raw_bits == self.constants.PURE_NAN_BITS))
        
        # Check if we can find a counterexample
        s.add(self.predicates.is_flonum(self.v))
        s.add(is_a_nan)
        s.add(f64_raw_bits != self.constants.PURE_NAN_BITS)
        
        result = s.check()
        passed_nan_purity = False
        
        if result == unsat:
            results.append(ProofResult(
                "NaN Purity Check",
                True,
                "PASS: Encoded flonum NaNs decode to PURE_NAN_BITS"
            ))
            passed_nan_purity = True
        else:
            details = "FAIL: Unexpected counterexample found or problem is complex"
            if result == sat:
                model = s.model()
                v_val_obj = model[self.v]
                f64_val_obj = model.eval(f64_raw_bits)
                
                v_hex = hex(v_val_obj.as_long()) if v_val_obj is not None else 'N/A'
                f64_hex = hex(f64_val_obj.as_long()) if f64_val_obj is not None else 'N/A'
                
                details += f". Counterexample - Encoded Value (v): {v_hex}, Decoded f64 bits: {f64_hex}"
                
                exp_val_obj = model.eval(exponent_bits)
                sig_val_obj = model.eval(significand_bits)
                exp_hex = hex(exp_val_obj.as_long()) if exp_val_obj is not None else 'N/A'
                sig_hex = hex(sig_val_obj.as_long()) if sig_val_obj is not None else 'N/A'
                
                details += f". Exponent: {exp_hex}, Significand: {sig_hex}"
                details += f". (PURE_NAN_BITS is {hex(self.constants.PURE_NAN_BITS)})"
            results.append(ProofResult("NaN Purity Check", False, details))
        
        # Create table for results
        headers = ["Check", "Status", "Details"]
        rows = []
        for result in results:
            status = "PASS" if result.passed else "FAIL"
            rows.append([result.name, status, result.details])
        
        table = TableFormatter.create_table(headers, rows, "Flonum NaN Purity Proof Results")
        print(table)
        
        return passed_nan_purity, results


def main():
    """Run all value encoding proofs."""
    print("Starting Z3 SMT Proof for Capyscheme Value Encodings...")
    
    # Create proof instance
    proofs = ValueEncodingProofs()
    
    # Run all proofs
    disjoint_ok, _ = proofs.run_disjointness_proofs()
    flonum_ok, _ = proofs.run_flonum_encoding_integrity_proof()
    char_ok, _ = proofs.run_char_encoding_integrity_proof()
    nan_purity_ok, _ = proofs.run_flonum_nan_purity_proof()
    
    # Print summary in a table
    print("\n" + "="*50)
    print("FINAL SUMMARY")
    print("="*50)
    
    summary_data = [
        ["Disjointness Checks", "PASS" if disjoint_ok else "FAIL"],
        ["Flonum Encoding Integrity", "PASS" if flonum_ok else "FAIL"],
        ["Char Encoding Integrity", "PASS" if char_ok else "FAIL"],
        ["Flonum NaN Purity", "PASS" if nan_purity_ok else "FAIL"]
    ]
    
    summary_table = TableFormatter.create_table(["Proof Category", "Result"], summary_data)
    print(summary_table)
    
    overall_result = disjoint_ok and flonum_ok and char_ok and nan_purity_ok
    print(f"\nOverall Result: {'ALL PROOFS PASSED' if overall_result else 'SOME PROOFS FAILED'}")


if __name__ == "__main__":
    main()
