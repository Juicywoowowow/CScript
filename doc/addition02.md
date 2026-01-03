# Addition 02: Type System Enforcement

## Status
**COMPLETED** - 2026-01-03

## Overview
Improve CScript's type system to provide stronger type safety with warnings and errors for potentially dangerous operations.

---

## Features Implemented

### ✅ 1. Function Argument Count/Type Checking
Validates function calls have correct number and types of arguments:

```csr
void foo(int a, int b) {}
foo(1);        // Error: expected 2 arguments, got 1
foo(1, 2, 3);  // Error: expected 2 arguments, got 3
foo("hi", 1);  // Error: expected 'int', found 'char*'
```

### ✅ 2. Narrowing Conversion Warnings
Warns when assigning larger types to smaller types that may lose data:

```csr
int64 big = 100;
int32 small = big;      // Warning: narrowing from int64 to int32
int16 smaller = small;  // Warning: narrowing from int32 to int16
int8 tiny = smaller;    // Warning: narrowing from int16 to int8
```

### ✅ 3. Signed/Unsigned Comparison Warnings
Warns when comparing signed and unsigned integers:

```csr
int x = -1;
uint32 y = 5;
if (x < y) { ... }  // Warning: comparison between signed and unsigned
```

---

## Implementation Details

### Type Size Detection
Added `numeric_type_size()` helper to determine bit width of numeric types:
- int8/uint8/char: 8 bits
- int16/uint16/short: 16 bits
- int32/uint32/int: 32 bits
- int64/uint64/long: 64 bits
- float/float32: 32 bits
- double/float64: 64 bits

### Signed Type Detection
Added `is_signed_type()` helper to determine signedness:
- Signed: int8, int16, int32, int64, int, long, short, char
- Unsigned: uint8, uint16, uint32, uint64, unsigned int/long/short/char

### Type Compatibility
Updated `base_types_compatible()` to allow all integer types to be compatible (C-like behavior), with narrowing warnings issued separately.

---

## Diagnostic Codes Added

- `W005` - NARROWING_CONVERSION: Narrowing conversion warning
- `W006` - SIGNED_UNSIGNED_COMPARE: Signed/unsigned comparison warning

---

## Files Modified

1. `src/semantic/type_checker.rs` - Added narrowing/signed-unsigned checks
2. `src/diagnostics/mod.rs` - Added new warning codes

---

## Testing

Test file: `examples/type_strictness.csr`

```
$ csc examples/type_strictness.csr

error[E200]: function 'takes_two' expects 2 arguments, got 1
error[E200]: function 'takes_two' expects 2 arguments, got 3
error[E200]: type mismatch for parameter 'msg'
   = help: expected 'char*', found 'int'

warning[W005]: narrowing conversion in initialization of 'small'
   = help: converting from 'int64_t' to 'int32_t' may lose data

warning[W005]: narrowing conversion in initialization of 'smaller'
   = help: converting from 'int32_t' to 'int16_t' may lose data

warning[W005]: narrowing conversion in initialization of 'tiny'
   = help: converting from 'int16_t' to 'int8_t' may lose data

warning[W006]: comparison between signed and unsigned integers
   = help: comparing 'int' with 'uint32_t'

3 errors, 6 warnings generated
```
