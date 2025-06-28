# Basic Haskell Module Implementation

## Objective
Create a simple Haskell module that implements basic list operations with proper error handling.

## Requirements

### Module Definition
- Module name: `ListUtils`
- Export list: `safeHead`, `safeTail`, `safeIndex`, `safeReverse`

### Functions to Implement

1. **safeHead :: [a] -> Maybe a**
   - Return the first element of a list, or Nothing if empty
   - Handle empty list case gracefully

2. **safeTail :: [a] -> Maybe [a]** 
   - Return the tail of a list, or Nothing if empty
   - Handle empty list case gracefully

3. **safeIndex :: [a] -> Int -> Maybe a**
   - Return element at given index, or Nothing if out of bounds
   - Handle negative indices and out-of-bounds cases

4. **safeReverse :: [a] -> [a]**
   - Reverse a list safely (can never fail)
   - Should work for empty lists

### Code Style Requirements
- Use explicit type signatures for all functions
- Add basic documentation comments for each function  
- Use standard Haskell naming conventions
- No external dependencies beyond Prelude

### Example Usage
The module should support usage like:
```haskell
import ListUtils

main = do
  print $ safeHead [1,2,3]     -- Just 1
  print $ safeHead []          -- Nothing
  print $ safeIndex [1,2,3] 1  -- Just 2
  print $ safeReverse [1,2,3]  -- [3,2,1]
```

## Expected Behavior
- All functions should be total (no runtime errors)
- Proper handling of edge cases (empty lists, invalid indices)
- Clean, readable Haskell code following standard conventions 