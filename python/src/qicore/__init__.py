"""
QiCore v4.0 - Mathematical Contract-Based Python Library

A comprehensive library providing 13 components with 99 operations based on 
mathematical contracts and functional programming principles.
"""

from .base import Result, QiError
from .core import Configuration, StructuredLogger, configure_logging, Cache
# Note: Application components require additional dependencies (httpx, fastapi, etc.)
# from .application import HTTPClient, WebApplication, CLIApplication, Database, AIClient

__version__ = "4.0.1"
__author__ = "QiCore Team"
__email__ = "team@qicore.dev"

__all__ = [
    # Base components (2 components, 14 operations)
    "Result",
    "QiError",
    
    # Core components (3 components, 25 operations) 
    "Configuration",
    "StructuredLogger", 
    "configure_logging",
    "Cache",
    
    # Application components (available with additional dependencies)
    # "HTTPClient",
    # "WebApplication", 
    # "CLIApplication",
    # "Database",
    # "AIClient",
    
    # Metadata
    "__version__",
    "__author__",
    "__email__",
]

def hello() -> str:
    """Legacy hello function - kept for backwards compatibility"""
    return "Hello from qicore!"