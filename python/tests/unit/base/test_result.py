# tests/unit/base/test_result.py
import pytest
from qicore.base import Result, QiError


class TestResult:
    """Test all 8 Result operations"""
    
    def test_success_creation(self):
        """Test Operation 1: Create success"""
        result = Result.success(42)
        assert result.is_success()
        assert result.unwrap() == 42
    
    def test_failure_creation(self):
        """Test Operation 2: Create failure"""
        error = QiError.validation_error("test error", "field", "value")
        result = Result.failure(error)
        assert not result.is_success()
    
    def test_map_operation(self):
        """Test Operation 3: Map"""
        result = Result.success(10)
        mapped = result.map(lambda x: x * 2)
        assert mapped.is_success()
        assert mapped.unwrap() == 20
        
        # Test map on failure
        error = QiError.validation_error("test", "field", "value")
        failed_result = Result.failure(error)
        mapped_failed = failed_result.map(lambda x: x * 2)
        assert not mapped_failed.is_success()
    
    def test_flat_map_operation(self):
        """Test Operation 4: FlatMap (bind)"""
        result = Result.success(5)
        flat_mapped = result.flat_map(lambda x: Result.success(x * 3))
        assert flat_mapped.is_success()
        assert flat_mapped.unwrap() == 15
        
        # Test flat_map returning failure
        flat_mapped_fail = result.flat_map(
            lambda x: Result.failure(QiError.validation_error("inner", "field", x))
        )
        assert not flat_mapped_fail.is_success()
    
    def test_map_error_operation(self):
        """Test Operation 5: Map error"""
        original_error = QiError.validation_error("original", "field", "value")
        result = Result.failure(original_error)
        
        mapped_error = result.map_error(
            lambda e: QiError.network_error("mapped error", "url")
        )
        assert not mapped_error.is_success()
    
    def test_recover_operation(self):
        """Test Operation 6: Recover"""
        error = QiError.validation_error("test", "field", "value")
        result = Result.failure(error)
        
        recovered = result.recover(lambda e: "recovered_value")
        assert recovered.is_success()
        assert recovered.unwrap() == "recovered_value"
        
        # Test recover on success (should pass through)
        success_result = Result.success("original")
        recovered_success = success_result.recover(lambda e: "fallback")
        assert recovered_success.unwrap() == "original"
    
    def test_unwrap_or_operation(self):
        """Test Operation 7: Unwrap with default"""
        success_result = Result.success("value")
        assert success_result.unwrap_or("default") == "value"
        
        error = QiError.validation_error("test", "field", "value")
        failed_result = Result.failure(error)
        assert failed_result.unwrap_or("default") == "default"
    
    def test_is_success_operation(self):
        """Test Operation 8: Check success"""
        success_result = Result.success(42)
        assert success_result.is_success() == True
        
        error = QiError.validation_error("test", "field", "value")
        failed_result = Result.failure(error)
        assert failed_result.is_success() == False
    
    def test_monad_laws(self):
        """Test that Result satisfies monad laws"""
        # Left identity: return a >>= f === f a
        value = 42
        f = lambda x: Result.success(x * 2)
        
        left = Result.success(value).flat_map(f)
        right = f(value)
        assert left.unwrap() == right.unwrap()
        
        # Right identity: m >>= return === m
        m = Result.success(value)
        identity_result = m.flat_map(Result.success)
        assert identity_result.unwrap() == m.unwrap()
        
        # Associativity: (m >>= f) >>= g === m >>= (\x -> f x >>= g)
        g = lambda x: Result.success(x + 1)
        
        left_assoc = m.flat_map(f).flat_map(g)
        right_assoc = m.flat_map(lambda x: f(x).flat_map(g))
        assert left_assoc.unwrap() == right_assoc.unwrap()