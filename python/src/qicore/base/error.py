# src/qicore/base/error.py
from dataclasses import dataclass
from typing import Any, Optional
import traceback
import time

@dataclass(frozen=True)
class QiError:
    """Structured error with context and chaining support"""
    category: str
    message: str
    context: dict[str, Any]
    timestamp: float
    cause: Optional['QiError'] = None
    stack_trace: Optional[str] = None
    
    # Operation 1: Create validation error (Category 1)
    @classmethod
    def validation_error(cls, message: str, field: str, value: Any) -> 'QiError':
        return cls(
            category="ValidationError",
            message=message,
            context={"field": field, "value": str(value)},
            timestamp=time.time(),
            stack_trace="".join(traceback.format_stack())
        )
    
    # Operation 2: Create network error (Category 2)
    @classmethod
    def network_error(cls, message: str, url: str, status_code: Optional[int] = None) -> 'QiError':
        return cls(
            category="NetworkError",
            message=message,
            context={"url": url, "status_code": status_code},
            timestamp=time.time()
        )
    
    # Operation 3: Create timeout error (Category 3)
    @classmethod
    def timeout_error(cls, message: str, operation: str, timeout_seconds: float) -> 'QiError':
        return cls(
            category="TimeoutError",
            message=message,
            context={"operation": operation, "timeout_seconds": timeout_seconds},
            timestamp=time.time()
        )
    
    # Operation 4: Create permission error (Category 4)
    @classmethod
    def permission_error(cls, message: str, resource: str, required_permission: str) -> 'QiError':
        return cls(
            category="PermissionError",
            message=message,
            context={"resource": resource, "required_permission": required_permission},
            timestamp=time.time()
        )
    
    # Operation 5: Create configuration error (Category 5)
    @classmethod
    def configuration_error(cls, message: str, key: str, expected_type: str) -> 'QiError':
        return cls(
            category="ConfigurationError",
            message=message,
            context={"key": key, "expected_type": expected_type},
            timestamp=time.time()
        )
    
    # Category 6-8: State, Resource, Integration errors
    @classmethod
    def state_error(cls, message: str, current_state: str, expected_state: str) -> 'QiError':
        return cls(
            category="StateError",
            message=message,
            context={"current_state": current_state, "expected_state": expected_state},
            timestamp=time.time()
        )
    
    @classmethod
    def resource_error(cls, message: str, resource_type: str, resource_id: str) -> 'QiError':
        return cls(
            category="ResourceError",
            message=message,
            context={"resource_type": resource_type, "resource_id": resource_id},
            timestamp=time.time()
        )
    
    @classmethod
    def integration_error(cls, message: str, service: str, operation: str) -> 'QiError':
        return cls(
            category="IntegrationError",
            message=message,
            context={"service": service, "operation": operation},
            timestamp=time.time()
        )
    
    # Operation 6: Chain errors
    def chain(self, cause: 'QiError') -> 'QiError':
        """Chain errors for better debugging"""
        return QiError(
            category=self.category,
            message=self.message,
            context=self.context,
            timestamp=self.timestamp,
            cause=cause,
            stack_trace=self.stack_trace
        )