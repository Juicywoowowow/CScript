# Issue 01: Struct Member Access Type Resolution

## Status
**Open** - Low Priority

## Summary
The type checker does not correctly resolve the type of struct member access expressions (`.` operator). Currently, it returns the struct type instead of the field's type.

## Example
```csr
struct Point {
    int x;
    int y;
};

int main() {
    struct Point p = { .x = 10, .y = 20 };
    
    // ERROR: type checker sees p.x as 'struct Point' instead of 'int'
    int value = p.x;  // Reports: expected 'int', found 'struct Point'
    
    return 0;
}
```

## Root Cause
In `src/semantic/type_checker.rs`, the `Expr::Member` case simply returns the object's type without looking up the field's type from the struct definition:

```rust
Expr::Member { object, member: _ } => {
    self.check_expr(object)
    // TODO: Check struct field exists and return field type
}
```

## Fix Required
1. Look up the struct type from the symbol table
2. Find the field definition by name
3. Return the field's type instead of the struct's type
4. Report an error if the field doesn't exist

## Workaround
For now, avoid using struct member access in variable initializations or use explicit pointer access with proper typing.

## Affected Files
- `src/semantic/type_checker.rs` - `check_expr()` function, `Expr::Member` case
- `src/semantic/symbol_table.rs` - Need to query `StructDef` fields

## Priority
Low - The code generator still produces correct C code. This is a type checker limitation that doesn't affect the output.
