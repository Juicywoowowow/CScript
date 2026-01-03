# Addition 01: Complete Option Type Support

## Status
**COMPLETED** - 2026-01-03

## Overview
Implement full support for `Option<T>` types in CScript, providing null-safety similar to Rust.

---

## Current State

### What Works
- ✅ Parser recognizes `Option<T>` syntax
- ✅ AST has `BaseType::Option(Box<TypeSpec>)` 
- ✅ Code generator emits Option struct types
- ✅ `none` literal is parsed
- ✅ `match` statement syntax is parsed
- ✅ Implicit wrapping when returning values from Option functions
- ✅ Proper type inference for `some(x)` bindings in match
- ✅ `.is_some()` / `.is_none()` method support  
- ✅ Improved match codegen with proper types

---

## Implementation Plan

### 1. Implicit Option Wrapping in Returns

When a function returns `Option<T>` and the return expression is of type `T`, 
automatically wrap it:

```csr
Option<int> get_value() {
    return 42;  // Implicitly becomes Some(42)
}

Option<int> maybe_get() {
    if (condition) {
        return 42;   // Implicitly Some(42)
    }
    return none;     // Explicit none
}
```

**Implementation:**
- In type checker `Stmt::Return`, check if function returns `Option<T>`
- If return expr type is `T` (not Option), mark it for wrapping
- In code generator, emit wrapped version

### 2. Type Inference for `some(x)` Binding

In match expressions, the bound variable should have the inner type:

```csr
Option<struct User*> result = find_user(1);

match (result) {
    some(user) => {
        // 'user' should be 'struct User*', not 'void*'
        printf("Found: %s\n", user->name);
    }
    none => {
        printf("Not found\n");
    }
}
```

**Implementation:**
- When type-checking match, extract inner type from Option<T>
- Bind the variable with the correct inner type
- Update code generator to use proper type casts

### 3. `.is_some()` / `.is_none()` Methods

Allow checking Option values without match:

```csr
Option<int> val = get_value();

if (val.is_some()) {
    int x = val.unwrap();
}

if (val.is_none()) {
    printf("No value\n");
}
```

**Implementation:**
- Extend expression parser to recognize `.is_some()` / `.is_none()`
- Add AST nodes for these method calls
- Generate `opt.is_some` field access in codegen

### 4. Improved Match Codegen

Current match generates `void*`. Need proper types:

**Before:**
```c
void* _match_val = (void*)(...);
if (_match_val != NULL) {
    void* user = _match_val;  // Wrong type!
}
```

**After:**
```c
Option_User_ptr _match_val = result;
if (_match_val.is_some) {
    struct User* user = (struct User*)_match_val.value;
}
```

---

## Testing

Create `examples/option_complete.csr`:
```csr
#include <stdio.h>
#include <stdlib.h>

Option<int> divide(int a, int b) {
    if (b == 0) {
        return none;
    }
    return a / b;  // Implicit wrap
}

int main() {
    Option<int> result = divide(10, 2);
    
    if (result.is_some()) {
        printf("Result: %d\n", result.unwrap());
    }
    
    match (result) {
        some(val) => {
            printf("Got: %d\n", val);
        }
        none => {
            printf("Division failed\n");
        }
    }
    
    return 0;
}
```

---

## Files to Modify

1. `src/semantic/type_checker.rs` - Return type checking, match binding types
2. `src/codegen/c_emitter.rs` - Match codegen, Option wrapping
3. `src/parser/expression.rs` - `.is_some()` / `.is_none()` method parsing
4. `src/parser/ast.rs` - Add IsSome/IsNone expr variants if needed
