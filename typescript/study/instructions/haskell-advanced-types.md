# Advanced Haskell Types and Type Classes

## Objective
Create a Haskell module demonstrating algebraic data types, type classes, and pattern matching.

## Requirements

### Module Definition
- Module name: `Calculator`
- Export list: `Expr(..)`, `eval`, `prettyPrint`, `simplify`

### Data Types

1. **Expression ADT**
   ```haskell
   data Expr = 
       Num Double
     | Add Expr Expr  
     | Sub Expr Expr
     | Mul Expr Expr
     | Div Expr Expr
   ```

### Type Class Instances

1. **Show Instance**
   - Implement `Show` for `Expr`
   - Format: `Add (Num 1.0) (Num 2.0)` style

2. **Eq Instance** 
   - Implement structural equality for expressions
   - `Num 1.0 == Num 1.0` should be `True`

### Functions to Implement

1. **eval :: Expr -> Maybe Double**
   - Evaluate expression to a numeric result
   - Return `Nothing` for division by zero
   - Handle all arithmetic operations

2. **prettyPrint :: Expr -> String**
   - Convert expression to human-readable string
   - Format: `(1.0 + 2.0)`, `(3.0 * (4.0 - 1.0))`
   - Use proper parentheses for precedence

3. **simplify :: Expr -> Expr**
   - Basic algebraic simplification
   - `Add (Num 0) x` → `x`
   - `Mul (Num 1) x` → `x`  
   - `Mul (Num 0) x` → `Num 0`

### Code Style Requirements
- Use explicit type signatures
- Comprehensive pattern matching
- Guard clauses where appropriate
- Clear documentation for each function

### Example Usage
```haskell
import Calculator

expr = Add (Num 1.0) (Mul (Num 2.0) (Num 3.0))

main = do
  print $ eval expr           -- Just 7.0
  putStrLn $ prettyPrint expr -- "(1.0 + (2.0 * 3.0))"
  print $ simplify expr       -- Same or simplified form
```

## Expected Behavior
- Proper handling of division by zero in evaluation
- Correct precedence in pretty printing
- Basic algebraic simplifications applied
- All pattern matches should be exhaustive 