# src/qicore/application/database/db.py
import aiosqlite
from typing import List, Dict, Any, Optional, TypeVar, Generic
from contextlib import asynccontextmanager
from ...base.result import Result
from ...base.error import QiError
from ...core.logging import StructuredLogger

T = TypeVar('T')

class Database(Generic[T]):
    """Async database operations with transaction support"""
    
    def __init__(
        self,
        db_path: str,
        logger: Optional[StructuredLogger] = None
    ):
        self.db_path = db_path
        self.logger = logger or StructuredLogger("database")
        self._connection: Optional[aiosqlite.Connection] = None
    
    # Operation 1: Connect
    async def connect(self) -> Result[None]:
        """Connect to database"""
        try:
            self._connection = await aiosqlite.connect(self.db_path)
            await self._connection.execute("PRAGMA foreign_keys = ON")
            return Result.success(None)
        except Exception as e:
            return Result.failure(
                QiError.resource_error(
                    f"Database connection failed: {e}",
                    "database",
                    self.db_path
                )
            )
    
    # Operation 2: Execute query
    async def execute(
        self,
        query: str,
        params: Optional[tuple] = None
    ) -> Result[aiosqlite.Cursor]:
        """Execute a query"""
        if not self._connection:
            return Result.failure(
                QiError.state_error(
                    "Database not connected",
                    "disconnected",
                    "connected"
                )
            )
        
        try:
            cursor = await self._connection.execute(query, params or ())
            await self._connection.commit()
            return Result.success(cursor)
        except Exception as e:
            try:
                await self._connection.rollback()
            except Exception:
                pass  # Ignore if no transaction is active
            return Result.failure(
                QiError.resource_error(
                    f"Query execution failed: {e}",
                    "query",
                    query
                )
            )
    
    # Transaction-aware execute (doesn't auto-commit)
    async def execute_in_transaction(
        self,
        query: str,
        params: Optional[tuple] = None
    ) -> Result[aiosqlite.Cursor]:
        """Execute a query within a transaction (no auto-commit)"""
        if not self._connection:
            return Result.failure(
                QiError.state_error(
                    "Database not connected",
                    "disconnected",
                    "connected"
                )
            )
        
        try:
            cursor = await self._connection.execute(query, params or ())
            return Result.success(cursor)
        except Exception as e:
            return Result.failure(
                QiError.resource_error(
                    f"Query execution failed: {e}",
                    "query",
                    query
                )
            )
    
    # Operation 3: Fetch results
    async def fetch_all(
        self,
        query: str,
        params: Optional[tuple] = None
    ) -> Result[List[Dict[str, Any]]]:
        """Fetch all results from query"""
        if not self._connection:
            return Result.failure(
                QiError.state_error(
                    "Database not connected",
                    "disconnected",
                    "connected"
                )
            )
        
        try:
            cursor = await self._connection.execute(query, params or ())
            rows = await cursor.fetchall()
            columns = [desc[0] for desc in cursor.description]
            
            results = [
                dict(zip(columns, row))
                for row in rows
            ]
            
            return Result.success(results)
        except Exception as e:
            return Result.failure(
                QiError.resource_error(
                    f"Fetch failed: {e}",
                    "query",
                    query
                )
            )
    
    # Operation 4: Transaction context
    @asynccontextmanager
    async def transaction(self):
        """Transaction context manager"""
        if not self._connection:
            raise Exception("Database not connected")
        
        # Set autocommit to False for transaction management
        await self._connection.execute("BEGIN")
        try:
            yield self
            await self._connection.commit()
        except Exception as e:
            try:
                await self._connection.rollback()
            except Exception:
                pass  # Ignore rollback errors if transaction wasn't active
            raise e
    
    # Operation 5: Close connection
    async def close(self) -> Result[None]:
        """Close database connection"""
        if self._connection:
            try:
                await self._connection.close()
                self._connection = None
                return Result.success(None)
            except Exception as e:
                return Result.failure(
                    QiError.resource_error(
                        f"Close failed: {e}",
                        "database",
                        self.db_path
                    )
                )
        return Result.success(None)