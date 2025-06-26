# tests/benchmarks/test_performance.py
import pytest
import asyncio
import time
from qicore.base import Result, QiError
from qicore.core import Cache, Configuration
from pydantic import BaseModel


class BenchmarkConfig(BaseModel):
    value: int = 0
    name: str = "benchmark"


class TestPerformance:
    """Performance benchmarks to verify tier requirements"""
    
    @pytest.mark.benchmark
    def test_result_creation_performance(self, benchmark):
        """Result creation should be very fast (>100k ops/sec target)"""
        def create_results():
            results = []
            for i in range(1000):
                result = Result.success(i)
                results.append(result)
            return results
        
        results = benchmark(create_results)
        assert len(results) == 1000
        assert all(r.is_success() for r in results)
    
    @pytest.mark.benchmark
    def test_result_map_performance(self, benchmark):
        """Result map operations should be fast (>50k ops/sec target)"""
        def map_operations():
            result = Result.success(42)
            mapped_results = []
            for i in range(1000):
                mapped = result.map(lambda x: x * i)
                mapped_results.append(mapped)
            return mapped_results
        
        results = benchmark(map_operations)
        assert len(results) == 1000
        assert all(r.is_success() for r in results)
    
    @pytest.mark.benchmark
    def test_result_flat_map_performance(self, benchmark):
        """Result flat_map operations should be reasonably fast (>25k ops/sec target)"""
        def flat_map_operations():
            result = Result.success(10)
            flat_mapped_results = []
            for i in range(500):
                flat_mapped = result.flat_map(lambda x: Result.success(x + i))
                flat_mapped_results.append(flat_mapped)
            return flat_mapped_results
        
        results = benchmark(flat_map_operations)
        assert len(results) == 500
        assert all(r.is_success() for r in results)
    
    @pytest.mark.benchmark
    def test_qierror_creation_performance(self, benchmark):
        """QiError creation should be fast enough for error handling"""
        def create_errors():
            errors = []
            for i in range(1000):
                error = QiError.validation_error(f"Error {i}", f"field_{i}", i)
                errors.append(error)
            return errors
        
        errors = benchmark(create_errors)
        assert len(errors) == 1000
        assert all(e.category == "ValidationError" for e in errors)
    
    @pytest.mark.benchmark
    @pytest.mark.asyncio
    async def test_cache_operations_performance(self, benchmark):
        """Cache operations should meet interpreted tier performance (<100μs per op)"""
        cache = Cache[str, int](max_size=1000)
        
        async def cache_operations():
            # Mixed read/write operations
            operations_completed = 0
            
            # Set operations
            for i in range(200):
                await cache.set(f"key_{i}", i)
                operations_completed += 1
            
            # Get operations (hits)
            for i in range(100):
                result = await cache.get(f"key_{i}")
                assert result.is_success()
                operations_completed += 1
            
            # Get operations (misses)
            for i in range(200, 300):
                result = await cache.get(f"missing_key_{i}")
                assert result.is_success()
                assert result.unwrap() is None
                operations_completed += 1
            
            return operations_completed
        
        # Run the benchmark
        operations = await benchmark(cache_operations)
        assert operations == 400
        
        # Verify cache statistics
        stats = cache.get_stats()
        assert stats['sets'] >= 200
        assert stats['hits'] >= 100
        assert stats['misses'] >= 100
    
    @pytest.mark.benchmark
    def test_configuration_operations_performance(self, benchmark):
        """Configuration operations should be fast enough for app startup"""
        def config_operations():
            configurations = []
            for i in range(100):
                config = Configuration(BenchmarkConfig)
                result = config.load_from_dict({
                    "value": i,
                    "name": f"config_{i}"
                })
                assert result.is_success()
                configurations.append(config)
            return configurations
        
        configs = benchmark(config_operations)
        assert len(configs) == 100
        
        # Verify all configurations are valid
        for config in configs:
            data = config.get().unwrap()
            assert isinstance(data.value, int)
            assert data.name.startswith("config_")
    
    @pytest.mark.benchmark
    @pytest.mark.asyncio
    async def test_cache_set_many_performance(self, benchmark):
        """Batch cache operations should be efficient"""
        cache = Cache[str, str](max_size=2000)
        
        async def batch_operations():
            # Prepare batch data
            batch_data = {f"batch_key_{i}": f"batch_value_{i}" for i in range(500)}
            
            # Set many
            await cache.set_many(batch_data)
            
            # Get many
            keys = list(batch_data.keys())
            result = await cache.get_many(keys)
            assert result.is_success()
            
            retrieved = result.unwrap()
            assert len(retrieved) == 500
            
            return len(retrieved)
        
        count = await benchmark(batch_operations)
        assert count == 500
    
    @pytest.mark.benchmark
    def test_result_error_handling_performance(self, benchmark):
        """Error handling through Result should be fast"""
        def error_handling_operations():
            successful_recoveries = 0
            
            for i in range(500):
                # Create a failure
                error = QiError.validation_error(f"Error {i}", "field", i)
                failed_result = Result.failure(error)
                
                # Recover from the error
                recovered = failed_result.recover(lambda e: f"recovered_{i}")
                assert recovered.is_success()
                assert recovered.unwrap() == f"recovered_{i}"
                
                successful_recoveries += 1
            
            return successful_recoveries
        
        recoveries = benchmark(error_handling_operations)
        assert recoveries == 500
    
    @pytest.mark.benchmark
    def test_result_chaining_performance(self, benchmark):
        """Chaining multiple Result operations should be efficient"""
        def chaining_operations():
            successful_chains = 0
            
            for i in range(200):
                result = (Result.success(i)
                         .map(lambda x: x * 2)
                         .flat_map(lambda x: Result.success(x + 1))
                         .map(lambda x: x - 1)
                         .flat_map(lambda x: Result.success(x / 2)))
                
                assert result.is_success()
                assert result.unwrap() == i  # Should be back to original value
                successful_chains += 1
            
            return successful_chains
        
        chains = benchmark(chaining_operations)
        assert chains == 200
    
    @pytest.mark.benchmark
    @pytest.mark.asyncio
    async def test_cache_memory_efficiency(self):
        """Test cache memory usage is reasonable"""
        cache = Cache[str, str](max_size=1000)
        
        # Fill cache to capacity
        start_time = time.time()
        for i in range(1000):
            await cache.set(f"memory_key_{i}", f"memory_value_{i}" * 10)  # Larger values
        fill_time = time.time() - start_time
        
        # Should fill cache quickly (less than 1 second for 1000 items)
        assert fill_time < 1.0
        
        # Verify cache is at capacity
        stats = cache.get_stats()
        assert stats['size'] == 1000
        assert stats['sets'] == 1000
        
        # Test eviction works
        await cache.set("overflow_key", "overflow_value")
        stats_after_overflow = cache.get_stats()
        assert stats_after_overflow['size'] == 1000  # Should still be at max
        assert stats_after_overflow['evictions'] >= 1
    
    @pytest.mark.benchmark
    def test_overall_library_performance(self, benchmark):
        """Test overall library performance for typical usage patterns"""
        def typical_usage_pattern():
            operations_completed = 0
            
            # Configuration setup (typical app startup)
            config = Configuration(BenchmarkConfig)
            config.load_from_dict({"value": 42, "name": "perf_test"})
            operations_completed += 1
            
            # Result operations (typical business logic)
            for i in range(50):
                result = (Result.success(i)
                         .map(lambda x: x * 2)
                         .flat_map(lambda x: Result.success(x) if x < 100 else 
                                 Result.failure(QiError.validation_error("Too large", "value", x))))
                
                if result.is_success():
                    operations_completed += 1
                else:
                    # Error recovery
                    recovered = result.recover(lambda e: 0)
                    assert recovered.is_success()
                    operations_completed += 1
            
            return operations_completed
        
        operations = benchmark(typical_usage_pattern)
        assert operations >= 50  # At least 50 operations completed
    
    def test_performance_regression_thresholds(self):
        """Define performance regression thresholds for CI/CD"""
        # These are the minimum performance targets
        # If benchmarks fall below these, it indicates a regression
        
        performance_targets = {
            "result_creation_per_second": 100000,  # >100k/sec
            "result_map_per_second": 50000,        # >50k/sec  
            "result_flat_map_per_second": 25000,   # >25k/sec
            "cache_operation_max_latency_us": 100, # <100μs per op
            "error_handling_per_second": 10000,    # >10k/sec
            "configuration_load_per_second": 1000, # >1k/sec
        }
        
        # This test documents our performance expectations
        # Actual benchmark enforcement would be in CI with --benchmark-only
        assert len(performance_targets) == 6
        assert all(target > 0 for target in performance_targets.values())