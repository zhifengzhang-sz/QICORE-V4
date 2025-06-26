# src/qicore/application/http/client.py
import httpx
import asyncio
from typing import Dict, Any, Optional, List
from circuitbreaker import circuit
from tenacity import retry, stop_after_attempt, wait_exponential
from ...base.result import Result
from ...base.error import QiError
from ...core.logging import StructuredLogger

class HTTPClient:
    """Async HTTP client with circuit breaker and retry logic"""
    
    def __init__(
        self,
        base_url: Optional[str] = None,
        timeout: float = 30.0,
        max_retries: int = 3,
        circuit_failure_threshold: int = 5,
        circuit_recovery_timeout: int = 60,
        logger: Optional[StructuredLogger] = None
    ):
        self.base_url = base_url
        self.timeout = timeout
        self.max_retries = max_retries
        self.logger = logger or StructuredLogger("http_client")
        
        # Configure circuit breaker
        self._circuit_failure_threshold = circuit_failure_threshold
        self._circuit_recovery_timeout = circuit_recovery_timeout
        
        # HTTP client with connection pooling
        self._client = httpx.AsyncClient(
            base_url=base_url,
            timeout=timeout,
            limits=httpx.Limits(max_keepalive_connections=10, max_connections=100)
        )
    
    # Operation 1: GET request
    @circuit(failure_threshold=5, recovery_timeout=60)
    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=4, max=10)
    )
    async def get(
        self,
        path: str,
        params: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None
    ) -> Result[httpx.Response]:
        """GET request with circuit breaker"""
        try:
            self.logger.info(f"GET {path}", params=params)
            response = await self._client.get(path, params=params, headers=headers)
            response.raise_for_status()
            return Result.success(response)
            
        except httpx.HTTPStatusError as e:
            self.logger.error(f"HTTP error: {e}", status=e.response.status_code)
            return Result.failure(
                QiError.network_error(
                    f"HTTP {e.response.status_code}: {e.response.text}",
                    str(e.request.url),
                    e.response.status_code
                )
            )
        except Exception as e:
            self.logger.error(f"Request failed: {e}")
            return Result.failure(
                QiError.network_error(
                    f"Request failed: {e}",
                    path
                )
            )
    
    # Operation 2: POST request
    @circuit(failure_threshold=5, recovery_timeout=60)
    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=4, max=10)
    )
    async def post(
        self,
        path: str,
        json: Optional[Dict[str, Any]] = None,
        data: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None
    ) -> Result[httpx.Response]:
        """POST request with circuit breaker"""
        try:
            self.logger.info(f"POST {path}")
            response = await self._client.post(
                path,
                json=json,
                data=data,
                headers=headers
            )
            response.raise_for_status()
            return Result.success(response)
            
        except httpx.HTTPStatusError as e:
            self.logger.error(f"HTTP error: {e}", status=e.response.status_code)
            return Result.failure(
                QiError.network_error(
                    f"HTTP {e.response.status_code}: {e.response.text}",
                    str(e.request.url),
                    e.response.status_code
                )
            )
        except Exception as e:
            self.logger.error(f"Request failed: {e}")
            return Result.failure(
                QiError.network_error(
                    f"Request failed: {e}",
                    path
                )
            )
    
    # Operation 3: PUT request
    async def put(
        self,
        path: str,
        json: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None
    ) -> Result[httpx.Response]:
        """PUT request"""
        try:
            response = await self._client.put(path, json=json, headers=headers)
            response.raise_for_status()
            return Result.success(response)
        except Exception as e:
            return Result.failure(
                QiError.network_error(f"PUT failed: {e}", path)
            )
    
    # Operation 4: DELETE request
    async def delete(
        self,
        path: str,
        headers: Optional[Dict[str, str]] = None
    ) -> Result[httpx.Response]:
        """DELETE request"""
        try:
            response = await self._client.delete(path, headers=headers)
            response.raise_for_status()
            return Result.success(response)
        except Exception as e:
            return Result.failure(
                QiError.network_error(f"DELETE failed: {e}", path)
            )
    
    # Operation 5: Stream response
    async def stream(
        self,
        path: str,
        params: Optional[Dict[str, Any]] = None
    ) -> Result[httpx.Response]:
        """Stream response data"""
        try:
            async with self._client.stream('GET', path, params=params) as response:
                response.raise_for_status()
                return Result.success(response)
        except Exception as e:
            return Result.failure(
                QiError.network_error(f"Stream failed: {e}", path)
            )
    
    # Operation 6: Close client
    async def close(self) -> None:
        """Close HTTP client connections"""
        await self._client.aclose()
    
    # Operation 7: Check circuit state
    def is_circuit_open(self) -> bool:
        """Check if circuit breaker is open"""
        # This would need actual circuit state access
        return False  # Simplified