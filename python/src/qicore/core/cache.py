# src/qicore/core/cache.py
from typing import TypeVar, Generic, Optional, Dict, Any, Callable, List, Tuple
from cachetools import TTLCache, LRUCache
import asyncio
import time
import pickle
from ..base.result import Result
from ..base.error import QiError

K = TypeVar('K')
V = TypeVar('V')

class Cache(Generic[K, V]):
    """Thread-safe cache with TTL and LRU eviction"""
    
    def __init__(
        self,
        max_size: int = 1000,
        ttl: Optional[float] = None,
        eviction_callback: Optional[Callable[[K, V], None]] = None
    ):
        self.max_size = max_size
        self.ttl = ttl
        self.eviction_callback = eviction_callback
        
        if ttl:
            self._cache: Dict[K, V] = TTLCache(maxsize=max_size, ttl=ttl)
        else:
            self._cache = LRUCache(maxsize=max_size)
        
        self._lock = asyncio.Lock()
        self._stats = {
            'hits': 0,
            'misses': 0,
            'evictions': 0,
            'sets': 0
        }
    
    # Operation 1: Get
    async def get(self, key: K) -> Result[Optional[V]]:
        """Get value from cache"""
        async with self._lock:
            try:
                if key in self._cache:
                    self._stats['hits'] += 1
                    return Result.success(self._cache[key])
                else:
                    self._stats['misses'] += 1
                    return Result.success(None)
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache get failed: {e}",
                        "get",
                        str(key)
                    )
                )
    
    # Operation 2: Set
    async def set(self, key: K, value: V) -> Result[None]:
        """Set value in cache"""
        async with self._lock:
            try:
                # Check if we're about to evict
                if len(self._cache) >= self.max_size and key not in self._cache:
                    self._stats['evictions'] += 1
                    if self.eviction_callback:
                        # Get the item that will be evicted
                        evicted_key = next(iter(self._cache))
                        evicted_value = self._cache[evicted_key]
                        self.eviction_callback(evicted_key, evicted_value)
                
                self._cache[key] = value
                self._stats['sets'] += 1
                return Result.success(None)
                
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache set failed: {e}",
                        "set",
                        str(key)
                    )
                )
    
    # Operation 3: Delete
    async def delete(self, key: K) -> Result[bool]:
        """Delete value from cache"""
        async with self._lock:
            try:
                if key in self._cache:
                    del self._cache[key]
                    return Result.success(True)
                return Result.success(False)
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache delete failed: {e}",
                        "delete",
                        str(key)
                    )
                )
    
    # Operation 4: Clear
    async def clear(self) -> Result[None]:
        """Clear all cache entries"""
        async with self._lock:
            try:
                cache_size = len(self._cache)
                self._cache.clear()
                self._stats['evictions'] += cache_size
                return Result.success(None)
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache clear failed: {e}",
                        "clear",
                        "all"
                    )
                )
    
    # Operation 5: Get many
    async def get_many(self, keys: List[K]) -> Result[Dict[K, Optional[V]]]:
        """Get multiple values at once"""
        async with self._lock:
            try:
                results = {}
                for key in keys:
                    if key in self._cache:
                        results[key] = self._cache[key]
                        self._stats['hits'] += 1
                    else:
                        results[key] = None
                        self._stats['misses'] += 1
                return Result.success(results)
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache get_many failed: {e}",
                        "get_many",
                        str(len(keys))
                    )
                )
    
    # Operation 6: Set many
    async def set_many(self, items: Dict[K, V]) -> Result[None]:
        """Set multiple values at once"""
        async with self._lock:
            try:
                for key, value in items.items():
                    if len(self._cache) >= self.max_size and key not in self._cache:
                        self._stats['evictions'] += 1
                    self._cache[key] = value
                    self._stats['sets'] += 1
                return Result.success(None)
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache set_many failed: {e}",
                        "set_many",
                        str(len(items))
                    )
                )
    
    # Operation 7: Get stats
    def get_stats(self) -> Dict[str, Any]:
        """Get cache statistics"""
        return {
            **self._stats,
            'size': len(self._cache),
            'max_size': self.max_size,
            'hit_rate': self._stats['hits'] / (self._stats['hits'] + self._stats['misses']) 
                        if (self._stats['hits'] + self._stats['misses']) > 0 else 0
        }
    
    # Operation 8: Memoize
    async def memoize(self, fn: Callable, *args, **kwargs) -> Result[Any]:
        """Memoize function results"""
        # Create cache key from function and arguments
        key = (fn.__name__, pickle.dumps((args, kwargs)))
        
        # Try to get from cache
        cached = await self.get(key)
        if cached.is_success() and cached.unwrap() is not None:
            return Result.success(cached.unwrap())
        
        # Compute and cache result
        try:
            result = await fn(*args, **kwargs) if asyncio.iscoroutinefunction(fn) else fn(*args, **kwargs)
            await self.set(key, result)
            return Result.success(result)
        except Exception as e:
            return Result.failure(
                QiError.state_error(
                    f"Memoization failed: {e}",
                    "function",
                    fn.__name__
                )
            )
    
    # Operation 9: Has key
    async def has_key(self, key: K) -> Result[bool]:
        """Check if key exists in cache"""
        async with self._lock:
            try:
                return Result.success(key in self._cache)
            except Exception as e:
                return Result.failure(
                    QiError.state_error(
                        f"Cache has_key failed: {e}",
                        "has_key",
                        str(key)
                    )
                )