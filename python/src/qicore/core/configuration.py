# src/qicore/core/configuration.py
import json
from pathlib import Path
from typing import Any, Generic, TypeVar

import yaml
from cytoolz import merge
from pydantic import BaseModel, ValidationError

from ..base.error import QiError
from ..base.result import Result

T = TypeVar('T', bound=BaseModel)

class Configuration(Generic[T]):
    """Type-safe configuration with monoid merge semantics"""
    
    def __init__(self, schema: type[T]):
        self.schema = schema
        self._data: T | None = None
    
    # Operation 1: Load from environment
    def load_from_env(self) -> Result[T]:
        """Load from environment variables"""
        try:
            config = self.schema()  # BaseSettings auto-loads from env
            self._data = config
            return Result.success(config)
        except ValidationError as e:
            return Result.failure(
                QiError.validation_error(
                    f"Environment validation failed: {e}",
                    "env",
                    str(e.errors())
                )
            )
    
    # Operation 2: Load from file
    def load_from_file(self, path: Path) -> Result[T]:
        """Load from YAML/JSON file"""
        try:
            if not path.exists():
                return Result.failure(
                    QiError.validation_error(
                        f"Config file not found: {path}",
                        "path",
                        str(path)
                    )
                )
            
            with open(path) as f:
                if path.suffix in ['.yaml', '.yml']:
                    data = yaml.safe_load(f)
                elif path.suffix == '.json':
                    data = json.load(f)
                else:
                    return Result.failure(
                        QiError.validation_error(
                            f"Unsupported file type: {path.suffix}",
                            "path",
                            str(path)
                        )
                    )
            
            config = self.schema(**data)
            self._data = config
            return Result.success(config)
            
        except Exception as e:
            return Result.failure(
                QiError.validation_error(
                    f"Failed to load config: {e}",
                    "path",
                    str(path)
                )
            )
    
    # Operation 3: Load from dict
    def load_from_dict(self, data: dict[str, Any]) -> Result[T]:
        """Load from dictionary"""
        try:
            config = self.schema(**data)
            self._data = config
            return Result.success(config)
        except ValidationError as e:
            return Result.failure(
                QiError.validation_error(
                    f"Dict validation failed: {e}",
                    "data",
                    str(e.errors())
                )
            )
    
    # Operation 4: Merge (monoid operation)
    def merge(self, other: 'Configuration[T]') -> Result['Configuration[T]']:
        """Monoid merge operation"""
        if not self._data or not other._data:
            return Result.failure(
                QiError.validation_error(
                    "Cannot merge uninitialized configurations",
                    "config",
                    "uninitialized"
                )
            )
        
        try:
            # Deep merge using cytoolz
            merged_dict = merge(
                self._data.model_dump(),
                other._data.model_dump()
            )
            merged_config = self.schema(**merged_dict)
            
            result = Configuration(self.schema)
            result._data = merged_config
            return Result.success(result)
            
        except Exception as e:
            return Result.failure(
                QiError.validation_error(
                    f"Merge failed: {e}",
                    "merge",
                    str(e)
                )
            )
    
    # Operation 5: Get current configuration
    def get(self) -> Result[T]:
        """Get current configuration"""
        if self._data:
            return Result.success(self._data)
        return Result.failure(
            QiError.validation_error(
                "Configuration not loaded",
                "config",
                "uninitialized"
            )
        )
    
    # Operation 6: Set value
    def set_value(self, key: str, value: Any) -> Result[None]:
        """Set a configuration value"""
        if not self._data:
            return Result.failure(
                QiError.configuration_error(
                    "Cannot set value on uninitialized config",
                    key,
                    "any"
                )
            )
        
        try:
            setattr(self._data, key, value)
            return Result.success(None)
        except Exception as e:
            return Result.failure(
                QiError.configuration_error(
                    f"Failed to set {key}: {e}",
                    key,
                    str(type(value))
                )
            )
    
    # Operation 7: Get value
    def get_value(self, key: str) -> Result[Any]:
        """Get a configuration value"""
        if not self._data:
            return Result.failure(
                QiError.configuration_error(
                    "Cannot get value from uninitialized config",
                    key,
                    "any"
                )
            )
        
        try:
            value = getattr(self._data, key)
            return Result.success(value)
        except AttributeError:
            return Result.failure(
                QiError.configuration_error(
                    f"Key not found: {key}",
                    key,
                    "unknown"
                )
            )
    
    # Operation 8: Validate
    def validate(self) -> Result[bool]:
        """Validate current configuration"""
        if not self._data:
            return Result.failure(
                QiError.configuration_error(
                    "No configuration to validate",
                    "config",
                    "loaded"
                )
            )
        
        try:
            # Re-validate using Pydantic
            self.schema(**self._data.model_dump())
            return Result.success(True)
        except ValidationError as e:
            return Result.failure(
                QiError.validation_error(
                    f"Validation failed: {e}",
                    "config",
                    str(e.errors())
                )
            )
    
    # Operation 9: Export
    def export_to_dict(self) -> Result[dict[str, Any]]:
        """Export configuration to dictionary"""
        if not self._data:
            return Result.failure(
                QiError.configuration_error(
                    "No configuration to export",
                    "config",
                    "loaded"
                )
            )
        
        return Result.success(self._data.model_dump())