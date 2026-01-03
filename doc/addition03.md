# Addition 03: Result<T, E> Type and Error Propagation

## Status
**COMPLETED** - 2026-01-03

## Overview
Implement Rust-style `Result<T, E>` type with the `?` operator for ergonomic error propagation. This makes CScript the first C-transpiler with proper error handling semantics.

---

## Syntax

### Result Type Declaration
```csr
Result<int, char*> parse_int(char* s);
Result<FILE*, int> open_file(char* path);
Result<void, Error> do_something();
```

### Creating Results
```csr
// Success
return ok(42);
return ok(ptr);

// Error  
return err("failed to parse");
return err(errno);
```

### Using Results
```csr
Result<int, char*> result = parse_int("42");

// Check and unwrap
if (result.is_ok()) {
    int val = result.unwrap();
}

if (result.is_err()) {
    char* error = result.unwrap_err();
}

// With default
int val = result.unwrap_or(0);

// Match
match (result) {
    ok(val) => printf("Got: %d\n", val),
    err(e) => printf("Error: %s\n", e)
}
```

### Error Propagation with `?`
```csr
Result<int, char*> calculate() {
    int a = parse_int("42")?;   // Returns early if err
    int b = parse_int("10")?;
    return ok(a + b);
}
```

The `?` operator:
1. If Result is `ok(v)`, extracts `v` and continues
2. If Result is `err(e)`, returns `err(e)` immediately

---

## C Code Generation

### Result Struct
```c
typedef struct {
    bool is_ok;
    union {
        int ok_value;      // T
        char* err_value;   // E
    };
} Result_int_charptr;

#define Result_int_charptr_OK(v) ((Result_int_charptr){ .is_ok = true, .ok_value = (v) })
#define Result_int_charptr_ERR(e) ((Result_int_charptr){ .is_ok = false, .err_value = (e) })
```

### `?` Operator Expansion
```csr
int a = parse_int("42")?;
```
Becomes:
```c
Result_int_charptr _tmp_0 = parse_int("42");
if (!_tmp_0.is_ok) {
    return Result_int_charptr_ERR(_tmp_0.err_value);
}
int a = _tmp_0.ok_value;
```

---

## Implementation Plan

### Phase 1: Parser
- [ ] Add `Result<T, E>` to type parser
- [ ] Add `ok(expr)` and `err(expr)` expressions
- [ ] Add `?` postfix operator
- [ ] Extend match patterns for `ok(name)` and `err(name)`

### Phase 2: AST
- [ ] Add `BaseType::Result(Box<TypeSpec>, Box<TypeSpec>)`
- [ ] Add `Expr::Ok(Box<Expr>)` and `Expr::Err(Box<Expr>)`
- [ ] Add `Expr::Try(Box<Expr>)` for `?` operator
- [ ] Add `MatchPattern::Ok(String)` and `MatchPattern::Err(String)`

### Phase 3: Type Checker
- [ ] Validate Result type usage
- [ ] Check `?` operator is only used in Result-returning functions
- [ ] Ensure error types are compatible for propagation
- [ ] Type inference for ok/err expressions

### Phase 4: Code Generator
- [ ] Generate Result struct definitions
- [ ] Generate OK/ERR macros
- [ ] Emit `?` operator as if-return pattern
- [ ] Handle match on Result types

---

## Files to Modify/Create

1. `src/parser/ast.rs` - Add Result types and expressions
2. `src/parser/mod.rs` - Parse Result<T,E> type
3. `src/parser/expression.rs` - Parse ok(), err(), and ?
4. `src/semantic/type_checker.rs` - Validate Result usage
5. `src/codegen/c_emitter.rs` - Generate C code

---

## Testing

Create `examples/result_test.csr`:
```csr
#include <stdio.h>
#include <stdlib.h>

Result<int, char*> parse_positive(char* s) {
    int val = atoi(s);
    if (val <= 0) {
        return err("must be positive");
    }
    return ok(val);
}

Result<int, char*> add_positives(char* a, char* b) {
    int x = parse_positive(a)?;
    int y = parse_positive(b)?;
    return ok(x + y);
}

int main() {
    Result<int, char*> r1 = add_positives("10", "20");
    match (r1) {
        ok(val) => printf("Sum: %d\n", val),
        err(e) => printf("Error: %s\n", e)
    }
    
    Result<int, char*> r2 = add_positives("10", "-5");
    if (r2.is_err()) {
        printf("Failed: %s\n", r2.unwrap_err());
    }
    
    return 0;
}
```

Expected output:
```
Sum: 30
Failed: must be positive
```
