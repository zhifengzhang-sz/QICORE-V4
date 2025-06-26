# tests/unit/base/test_error.py
import pytest
import time
from qicore.base import QiError


class TestQiError:
    """Test all 6 + 8 QiError operations"""
    
    def test_validation_error_creation(self):
        """Test Category 1: Validation error"""
        error = QiError.validation_error("Invalid field", "username", "")
        assert error.category == "ValidationError"
        assert error.message == "Invalid field"
        assert error.context["field"] == "username"
        assert error.context["value"] == ""
        assert error.timestamp > 0
        assert error.stack_trace is not None
    
    def test_network_error_creation(self):
        """Test Category 2: Network error"""
        error = QiError.network_error("Connection failed", "https://api.example.com", 404)
        assert error.category == "NetworkError"
        assert error.context["url"] == "https://api.example.com"
        assert error.context["status_code"] == 404
    
    def test_timeout_error_creation(self):
        """Test Category 3: Timeout error"""
        error = QiError.timeout_error("Request timed out", "api_call", 30.0)
        assert error.category == "TimeoutError"
        assert error.context["operation"] == "api_call"
        assert error.context["timeout_seconds"] == 30.0
    
    def test_permission_error_creation(self):
        """Test Category 4: Permission error"""
        error = QiError.permission_error("Access denied", "/admin", "admin_role")
        assert error.category == "PermissionError"
        assert error.context["resource"] == "/admin"
        assert error.context["required_permission"] == "admin_role"
    
    def test_configuration_error_creation(self):
        """Test Category 5: Configuration error"""
        error = QiError.configuration_error("Invalid config", "database_url", "string")
        assert error.category == "ConfigurationError"
        assert error.context["key"] == "database_url"
        assert error.context["expected_type"] == "string"
    
    def test_state_error_creation(self):
        """Test Category 6: State error"""
        error = QiError.state_error("Invalid state", "closed", "open")
        assert error.category == "StateError"
        assert error.context["current_state"] == "closed"
        assert error.context["expected_state"] == "open"
    
    def test_resource_error_creation(self):
        """Test Category 7: Resource error"""
        error = QiError.resource_error("Resource not found", "user", "123")
        assert error.category == "ResourceError"
        assert error.context["resource_type"] == "user"
        assert error.context["resource_id"] == "123"
    
    def test_integration_error_creation(self):
        """Test Category 8: Integration error"""
        error = QiError.integration_error("Service unavailable", "payment_service", "charge")
        assert error.category == "IntegrationError"
        assert error.context["service"] == "payment_service"
        assert error.context["operation"] == "charge"
    
    def test_error_chaining(self):
        """Test Operation 6: Chain errors"""
        root_error = QiError.validation_error("Root cause", "field", "value")
        chained_error = QiError.network_error("Network issue", "url")
        
        final_error = chained_error.chain(root_error)
        assert final_error.cause == root_error
        assert final_error.category == "NetworkError"
        assert final_error.message == "Network issue"
    
    def test_error_immutability(self):
        """Test that QiError is immutable (frozen dataclass)"""
        error = QiError.validation_error("test", "field", "value")
        
        with pytest.raises(Exception):  # Should raise FrozenInstanceError
            error.message = "modified"
    
    def test_timestamp_accuracy(self):
        """Test that timestamp is set correctly"""
        before = time.time()
        error = QiError.validation_error("test", "field", "value")
        after = time.time()
        
        assert before <= error.timestamp <= after
    
    def test_all_error_categories_count(self):
        """Verify we have all 8 error categories"""
        categories = [
            "ValidationError",
            "NetworkError", 
            "TimeoutError",
            "PermissionError",
            "ConfigurationError",
            "StateError",
            "ResourceError",
            "IntegrationError"
        ]
        
        # Test we can create all 8 categories
        errors = [
            QiError.validation_error("test", "field", "value"),
            QiError.network_error("test", "url"),
            QiError.timeout_error("test", "op", 1.0),
            QiError.permission_error("test", "resource", "perm"),
            QiError.configuration_error("test", "key", "type"),
            QiError.state_error("test", "current", "expected"),
            QiError.resource_error("test", "type", "id"),
            QiError.integration_error("test", "service", "op")
        ]
        
        actual_categories = [error.category for error in errors]
        assert actual_categories == categories