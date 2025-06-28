{- | QiCore Base Module

This module provides the foundational types and operations for the QiCore library:
- QiError: Standardized error representation with rich context
- Result: Type-safe success/failure handling without exceptions

These types form the basis for all other QiCore components and enable
functional composition and error handling throughout the library.
-}
module QiCore.Base
  ( -- * Error Types and Operations
    module QiCore.Base.Error

    -- * Result Types and Operations
  , module QiCore.Base.Result

    -- * Re-exports for convenience
  , QiError (..)
  , ErrorCategory (..)
  , Result (..)
  ) where

import QiCore.Base.Error
import QiCore.Base.Result
