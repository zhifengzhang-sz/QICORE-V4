"""Example demonstrating QiCore v4.0 basic usage"""
import asyncio
from pathlib import Path
from qicore.base import Result, QiError
from qicore.core import Configuration, StructuredLogger, Cache, configure_logging
from pydantic import BaseModel

class AppConfig(BaseModel):
    app_name: str = "QiCore Demo"
    cache_size: int = 100
    debug: bool = False

async def demo_basic_usage():
    """Demonstrate basic QiCore functionality"""
    
    # 1. Configure logging
    await configure_logging(level="INFO", format="json")
    logger = StructuredLogger("demo")
    
    # 2. Result monad demo
    result = Result.success(42)
    doubled = result.map(lambda x: x * 2)
    logger.info(f"Result demo: {doubled.unwrap()}")
    
    # 3. Configuration demo
    config = Configuration(AppConfig)
    config_result = config.load_from_dict({
        "app_name": "QiCore v4.0 Demo",
        "cache_size": 200,
        "debug": True
    })
    
    if config_result.is_success():
        app_config = config_result.unwrap()
        logger.info(f"Config loaded: {app_config.app_name}")
    
    # 4. Cache demo
    cache = Cache[str, str](max_size=100)
    await cache.set("demo_key", "demo_value")
    cached_value = await cache.get("demo_key")
    
    if cached_value.is_success():
        logger.info(f"Cache demo: {cached_value.unwrap()}")
    
    # 5. Error handling demo
    error = QiError.validation_error(
        "Demo validation error",
        "demo_field",
        "invalid_value"
    )
    
    failed_result = Result.failure(error)
    logger.info(f"Error handling demo: {failed_result.is_success()}")
    
    logger.info("QiCore v4.0 demo completed successfully!")

if __name__ == "__main__":
    asyncio.run(demo_basic_usage())