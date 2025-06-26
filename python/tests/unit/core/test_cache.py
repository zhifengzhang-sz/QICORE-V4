# tests/unit/core/test_cache.py
import pytest
import asyncio
from qicore.core import Cache
from qicore.base import Result


class TestCache:
    """Test all 9 Cache operations"""
    
    @pytest.fixture
    def cache(self):
        """Create a cache instance for testing"""
        return Cache[str, str](max_size=10)
    
    @pytest.mark.asyncio
    async def test_get_operation(self, cache):
        """Test Operation 1: Get"""
        # Test cache miss
        result = await cache.get("missing_key")
        assert result.is_success()
        assert result.unwrap() is None
        
        # Set a value and test cache hit
        await cache.set("test_key", "test_value")
        result = await cache.get("test_key")
        assert result.is_success()
        assert result.unwrap() == "test_value"
    
    @pytest.mark.asyncio
    async def test_set_operation(self, cache):
        """Test Operation 2: Set"""
        result = await cache.set("key1", "value1")
        assert result.is_success()
        
        # Verify the value was set
        get_result = await cache.get("key1")
        assert get_result.unwrap() == "value1"
    
    @pytest.mark.asyncio
    async def test_delete_operation(self, cache):
        """Test Operation 3: Delete"""
        # Set a value first
        await cache.set("key_to_delete", "value")
        
        # Delete existing key
        result = await cache.delete("key_to_delete")
        assert result.is_success()
        assert result.unwrap() == True
        
        # Verify it's gone
        get_result = await cache.get("key_to_delete")
        assert get_result.unwrap() is None
        
        # Delete non-existing key
        result = await cache.delete("non_existing")
        assert result.is_success()
        assert result.unwrap() == False
    
    @pytest.mark.asyncio
    async def test_clear_operation(self, cache):
        """Test Operation 4: Clear"""
        # Add multiple values
        await cache.set("key1", "value1")
        await cache.set("key2", "value2")
        await cache.set("key3", "value3")
        
        # Clear cache
        result = await cache.clear()
        assert result.is_success()
        
        # Verify all values are gone
        for key in ["key1", "key2", "key3"]:
            get_result = await cache.get(key)
            assert get_result.unwrap() is None
    
    @pytest.mark.asyncio
    async def test_get_many_operation(self, cache):
        """Test Operation 5: Get many"""
        # Set some values
        await cache.set("key1", "value1")
        await cache.set("key2", "value2")
        
        # Get multiple keys
        result = await cache.get_many(["key1", "key2", "key3"])
        assert result.is_success()
        
        results = result.unwrap()
        assert results["key1"] == "value1"
        assert results["key2"] == "value2"
        assert results["key3"] is None
    
    @pytest.mark.asyncio
    async def test_set_many_operation(self, cache):
        """Test Operation 6: Set many"""
        items = {
            "multi1": "value1",
            "multi2": "value2",
            "multi3": "value3"
        }
        
        result = await cache.set_many(items)
        assert result.is_success()
        
        # Verify all values were set
        for key, expected_value in items.items():
            get_result = await cache.get(key)
            assert get_result.unwrap() == expected_value
    
    def test_get_stats_operation(self, cache):
        """Test Operation 7: Get stats"""
        stats = cache.get_stats()
        
        assert "hits" in stats
        assert "misses" in stats
        assert "evictions" in stats
        assert "sets" in stats
        assert "size" in stats
        assert "max_size" in stats
        assert "hit_rate" in stats
        
        assert stats["max_size"] == 10
        assert stats["size"] == 0
        assert stats["hit_rate"] == 0  # No operations yet
    
    @pytest.mark.asyncio
    async def test_memoize_operation(self, cache):
        """Test Operation 8: Memoize"""
        call_count = 0
        
        def expensive_function(x):
            nonlocal call_count
            call_count += 1
            return x * 2
        
        # First call should execute function
        result1 = await cache.memoize(expensive_function, 5)
        assert result1.is_success()
        assert result1.unwrap() == 10
        assert call_count == 1
        
        # Second call should use cache
        result2 = await cache.memoize(expensive_function, 5)
        assert result2.is_success()
        assert result2.unwrap() == 10
        assert call_count == 1  # Function not called again
    
    @pytest.mark.asyncio
    async def test_has_key_operation(self, cache):
        """Test Operation 9: Has key"""
        # Test key that doesn't exist
        result = await cache.has_key("non_existing")
        assert result.is_success()
        assert result.unwrap() == False
        
        # Set a key and test
        await cache.set("existing_key", "value")
        result = await cache.has_key("existing_key")
        assert result.is_success()
        assert result.unwrap() == True
    
    @pytest.mark.asyncio
    async def test_cache_eviction(self):
        """Test LRU eviction when cache is full"""
        small_cache = Cache[str, str](max_size=2)
        
        # Fill cache to capacity
        await small_cache.set("key1", "value1")
        await small_cache.set("key2", "value2")
        
        # Add third item (should evict first)
        await small_cache.set("key3", "value3")
        
        # Check eviction occurred
        result1 = await small_cache.get("key1")
        assert result1.unwrap() is None  # Should be evicted
        
        result2 = await small_cache.get("key2")
        assert result2.unwrap() == "value2"  # Should still exist
        
        result3 = await small_cache.get("key3")
        assert result3.unwrap() == "value3"  # Should exist
    
    @pytest.mark.asyncio
    async def test_ttl_cache(self):
        """Test TTL (Time To Live) functionality"""
        ttl_cache = Cache[str, str](max_size=10, ttl=0.1)  # 100ms TTL
        
        # Set a value
        await ttl_cache.set("ttl_key", "ttl_value")
        
        # Should exist immediately
        result = await ttl_cache.get("ttl_key")
        assert result.unwrap() == "ttl_value"
        
        # Wait for TTL to expire
        await asyncio.sleep(0.15)
        
        # Should be expired now
        result = await ttl_cache.get("ttl_key")
        assert result.unwrap() is None
    
    @pytest.mark.asyncio
    async def test_hit_rate_calculation(self, cache):
        """Test hit rate statistics"""
        # Initially no hits or misses
        stats = cache.get_stats()
        assert stats["hit_rate"] == 0
        
        # Add some cache misses
        await cache.get("miss1")
        await cache.get("miss2")
        
        stats = cache.get_stats()
        assert stats["hit_rate"] == 0  # 0 hits, 2 misses
        assert stats["misses"] == 2
        
        # Add a value and get it (hit)
        await cache.set("hit_key", "value")
        await cache.get("hit_key")
        
        stats = cache.get_stats()
        assert stats["hits"] == 1
        assert stats["misses"] == 2
        assert abs(stats["hit_rate"] - (1/3)) < 0.001  # 1 hit out of 3 total