# Issue 02: Enhanced Error Messages & Option Types

## Status
**In Progress** - Started 2026-01-03

## Overview
This issue tracks improvements to:
1. Rich error/warning/help messages with source code context
2. Complete Option<T> type support
3. Better type system enforcement (narrowing warnings, etc.)

---

## Part 1: Rich Error Messages with Source Context

### Current Problem
The type checker uses `reporter.add(diagnostic)` which produces errors without source location:
```
error[E200]: type mismatch in initialization of 'value'
```

### Desired Output
Errors should show source context like Rust/GCC:
```
error[E200]: type mismatch in initialization of 'value'
   --> examples/test.csr:20:5
    |
 20 |     int value = p.x;
    |                 ^^^ expected 'int', found 'struct Point'
    |
    = help: struct fields access their field types, not the struct type
```

### Implementation

#### Step 1: Add Span to AST Nodes
Modify `src/parser/ast.rs` to add source positions:

```rust
/// Source span for error reporting
#[derive(Debug, Clone, Default)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
}

// Add span to expressions
pub enum Expr {
    IntLiteral(i64, Span),
    // ... etc
}
```

#### Step 2: Parser Updates
Modify parser to record spans when building AST nodes.

#### Step 3: Type Checker Updates  
Modify type checker to use `reporter.report()` with offsets instead of `reporter.add()`.

---

## Part 2: Complete Option Type Support

### Current State
- Parser: Can parse `Option<T>` syntax ✓
- AST: `BaseType::Option(Box<TypeSpec>)` exists ✓
- CodeGen: Generates Option struct types ✓
- Type Checker: Partial support

### Missing Features

#### 2.1 Implicit Option Wrapping
When returning a value from an Option-returning function, implicitly wrap:
```csr
Option<int> get_value() {
    return 42;  // Should become Option<int>{.is_some = true, .value = 42}
}
```

#### 2.2 Type Inference for `some(x)` Binding
In match arms, bind the inner type correctly:
```csr
match (result) {
    some(user) => {  // 'user' should have type T, not void*
        // ...
    }
}
```

#### 2.3 `.is_some()` / `.is_none()` Methods
```csr
if (result.is_some()) { ... }
if (result.is_none()) { ... }
```

#### 2.4 Complete `match` Codegen
Generate proper C switch/if statements for match expressions.

---

## Part 3: Type System Enforcement

### 3.1 Narrowing Conversion Warnings
Warn when assigning larger types to smaller:
```csr
int64 big = 9999999999;
int32 small = big;  // Warning: narrowing conversion from int64 to int32
```

### 3.2 Integer/Float Mixing
Require explicit casts:
```csr
int x = 10;
float f = x;  // Error: implicit conversion from int to float
float f = (float)x;  // OK
```

### 3.3 Function Argument Count Checking
```csr
void foo(int a, int b) {}
foo(1);  // Error: expected 2 arguments, got 1
```

### 3.4 Pointer Type Strictness  
```csr
int* ip = malloc(sizeof(int));  // Warn: void* to int* without cast
int* ip = (int*)malloc(sizeof(int));  // OK
```

---

## Implementation Order

### Phase 1: Rich Error Messages (High Priority) ✅ COMPLETED
1. [x] Add `Span` struct to AST
2. [x] Add `name_span` to VariableDecl
3. [x] Update parser to track spans for variable declarations
4. [x] Update type checker to use spans in error reporting
5. [x] Support local struct declarations inside functions

### Phase 2: Option Types
1. [ ] Implement implicit Option wrapping in returns
2. [ ] Type inference for match `some(x)` bindings
3. [ ] Add `.is_some()` / `.is_none()` method support
4. [ ] Complete match expression codegen

### Phase 3: Type System (Lower Priority)
1. [ ] Implement narrowing conversion warnings
2. [ ] Function argument count validation
3. [ ] Stricter integer/float conversion rules

---

## Testing

Create test files:
- `examples/error_messages_test.csr` - Intentional errors to verify output
- `examples/option_complete.csr` - Full Option type usage
- `examples/type_strictness.csr` - Type conversion edge cases
