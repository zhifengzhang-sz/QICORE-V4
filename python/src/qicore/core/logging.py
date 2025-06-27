# src/qicore/core/logging.py
import logging
from contextvars import ContextVar
from typing import Any

import structlog

from ..base.error import QiError
from ..base.result import Result

# Context variable for request correlation
correlation_id: ContextVar[str | None] = ContextVar('correlation_id', default=None)

class StructuredLogger:
    """High-performance structured logging with context propagation"""
    
    def __init__(self, name: str, context: dict[str, Any] | None = None):
        self.name = name
        self.context = context or {}
        self._logger = structlog.get_logger(name).bind(**self.context)
    
    # Operation 1: Debug log
    def debug(self, message: str, **kwargs: Any) -> None:
        """Log debug message"""
        self._add_correlation_id(kwargs)
        self._logger.debug(message, **kwargs)
    
    # Operation 2: Info log
    def info(self, message: str, **kwargs: Any) -> None:
        """Log info message"""
        self._add_correlation_id(kwargs)
        self._logger.info(message, **kwargs)
    
    # Operation 3: Warning log
    def warning(self, message: str, **kwargs: Any) -> None:
        """Log warning message"""
        self._add_correlation_id(kwargs)
        self._logger.warning(message, **kwargs)
    
    # Operation 4: Error log
    def error(self, message: str, **kwargs: Any) -> None:
        """Log error message"""
        self._add_correlation_id(kwargs)
        self._logger.error(message, **kwargs)
    
    # Operation 5: With context
    def with_context(self, **kwargs: Any) -> 'StructuredLogger':
        """Create new logger with additional context"""
        new_context = {**self.context, **kwargs}
        return StructuredLogger(self.name, new_context)
    
    # Operation 6: Log with level
    def log(self, level: str, message: str, **kwargs: Any) -> None:
        """Log with specified level"""
        self._add_correlation_id(kwargs)
        log_method = getattr(self._logger, level.lower(), self._logger.info)
        log_method(message, **kwargs)
    
    # Operation 7: Set correlation ID
    def set_correlation_id(self, correlation_id_value: str) -> None:
        """Set correlation ID for request tracking"""
        correlation_id.set(correlation_id_value)
    
    def _add_correlation_id(self, kwargs: dict[str, Any]) -> None:
        """Add correlation ID from context var"""
        if cid := correlation_id.get():
            kwargs['correlation_id'] = cid

# Global configuration function
async def configure_logging(
    level: str = "INFO",
    log_format: str = "json",
    add_timestamp: bool = True,
    processors: list[Any] | None = None
) -> Result[None]:
    """Configure global logging settings"""
    try:
        default_processors = [
            structlog.stdlib.filter_by_level,
            structlog.stdlib.add_logger_name,
            structlog.stdlib.add_log_level,
            structlog.stdlib.PositionalArgumentsFormatter(),
            structlog.processors.StackInfoRenderer(),
            structlog.processors.format_exc_info,
        ]
        
        if add_timestamp:
            default_processors.append(structlog.processors.TimeStamper(fmt="iso"))
        
        if processors:
            default_processors.extend(processors)
        
        if log_format == "json":
            default_processors.append(structlog.processors.JSONRenderer())
        else:
            default_processors.append(structlog.dev.ConsoleRenderer())
        
        structlog.configure(
            processors=default_processors,
            context_class=dict,
            logger_factory=structlog.stdlib.LoggerFactory(),
            wrapper_class=structlog.stdlib.BoundLogger,
            cache_logger_on_first_use=True,
        )
        
        # Set Python logging level
        logging.basicConfig(level=getattr(logging, level.upper()))
        
        return Result.success(None)
        
    except Exception as e:
        return Result.failure(
            QiError.configuration_error(
                f"Failed to configure logging: {e}",
                "logging",
                str(e)
            )
        )