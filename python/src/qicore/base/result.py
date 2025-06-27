# src/qicore/base/result.py
from collections.abc import Callable
from typing import Generic, TypeVar, Union

from returns.result import Failure, Success
from returns.result import Result as ReturnsResult

from .error import QiError

T = TypeVar('T')
E = TypeVar('E')
U = TypeVar('U')

class Result(Generic[T]):
    """Result monad implementation wrapping returns.Result"""
    
    def __init__(self, value: Union[ReturnsResult[T, 'QiError'], T, 'QiError']):
        if isinstance(value, ReturnsResult):
            self._inner = value
        elif hasattr(value, 'category'):  # Duck typing for QiError
            self._inner = Failure(value)
        else:
            self._inner = Success(value)
    
    # Operation 1: Create success
    @classmethod
    def success(cls, value: T) -> 'Result[T]':
        """Create successful result"""
        return cls(Success(value))
    
    # Operation 2: Create failure
    @classmethod
    def failure(cls, error: 'QiError') -> 'Result[T]':
        """Create failed result"""
        return cls(Failure(error))
    
    # Operation 3: Map
    def map(self, fn: Callable[[T], U]) -> 'Result[U]':
        """Map function over success value"""
        return Result(self._inner.map(fn))
    
    # Operation 4: FlatMap (bind)
    def flat_map(self, fn: Callable[[T], 'Result[U]']) -> 'Result[U]':
        """Monadic bind operation"""
        return Result(self._inner.bind(lambda x: fn(x)._inner))
    
    # Operation 5: Map error
    def map_error(self, fn: Callable[['QiError'], 'QiError']) -> 'Result[T]':
        """Transform error value"""
        return Result(self._inner.alt(lambda e: Failure(fn(e))))
    
    # Operation 6: Recover
    def recover(self, fn: Callable[['QiError'], T]) -> 'Result[T]':
        """Recover from error with fallback"""
        if isinstance(self._inner, Success):
            return self  # Already successful, return as-is
        # For failures, apply recovery function
        error = self._inner.failure()
        return Result.success(fn(error))
    
    # Operation 7: Unwrap with default
    def unwrap_or(self, default: T) -> T:
        """Get value or default"""
        return self._inner.value_or(default)
    
    # Operation 8: Check success
    def is_success(self) -> bool:
        """Check if successful"""
        return isinstance(self._inner, Success)
    
    def unwrap(self) -> T:
        """Unwrap value (raises if failure)"""
        if self.is_success():
            return self._inner.unwrap()
        raise ValueError(f"Cannot unwrap failed Result: {self._inner.failure()}")