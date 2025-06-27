# src/qicore/application/web/framework.py
import time
import uuid
from collections.abc import Callable
from typing import Any

from fastapi import Depends, FastAPI, HTTPException, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse

from ...base.result import Result
from ...core.logging import StructuredLogger, correlation_id


class WebApplication:
    """FastAPI integration with QiCore patterns"""
    
    def __init__(
        self,
        title: str = "QiCore API",
        version: str = "4.0.1",
        logger: StructuredLogger | None = None
    ):
        self.app = FastAPI(title=title, version=version)
        self.logger = logger or StructuredLogger("web_app")
        self._setup_middleware()
        self._setup_error_handlers()
    
    # Operation 1: Add route
    def route(
        self,
        path: str,
        methods: list[str] | None = None,
        response_model: Any | None = None,
        status_code: int = 200
    ) -> Callable:
        """Decorator for Result-based routes"""
        if methods is None:
            methods = ["GET"]
        def decorator(fn: Callable) -> Callable:
            async def wrapper(*args, **kwargs) -> Any:
                result = await fn(*args, **kwargs)
                
                if isinstance(result, Result):
                    if result.is_success():
                        return result.unwrap_or(None)
                    # Extract error from failed result
                    error = (
                        result._inner._inner_value 
                        if hasattr(result._inner, '_inner_value') 
                        else None
                    )
                    if error and hasattr(error, 'message'):
                        raise HTTPException(
                            status_code=400,
                            detail=error.message
                        )
                    raise HTTPException(
                        status_code=500,
                        detail="Internal server error"
                    )
                return result
            
            # Register with FastAPI
            self.app.add_api_route(
                path,
                wrapper,
                methods=methods,
                response_model=response_model,
                status_code=status_code
            )
            
            return wrapper
        return decorator
    
    # Operation 2: Add middleware
    def add_middleware(self, middleware_class: Any, **options: Any) -> None:
        """Add middleware to application"""
        self.app.add_middleware(middleware_class, **options)
    
    # Operation 3: Add exception handler
    def add_exception_handler(
        self,
        exc_class: type,
        handler: Callable[[Request, Any], Response]
    ) -> None:
        """Add custom exception handler"""
        self.app.add_exception_handler(exc_class, handler)
    
    # Operation 4: Get app instance
    def get_app(self) -> FastAPI:
        """Get FastAPI application instance"""
        return self.app
    
    # Operation 5: Add startup handler
    def on_startup(self, fn: Callable) -> Callable:
        """Add startup event handler"""
        self.app.add_event_handler("startup", fn)
        return fn
    
    # Operation 6: Add shutdown handler
    def on_shutdown(self, fn: Callable) -> Callable:
        """Add shutdown event handler"""
        self.app.add_event_handler("shutdown", fn)
        return fn
    
    # Operation 7: Add dependency
    def dependency(self, fn: Callable) -> Callable:
        """Create FastAPI dependency"""
        return Depends(fn)
    
    # Operation 8: Run server
    def run(self, host: str = "0.0.0.0", port: int = 8000) -> None:
        """Run the application with Uvicorn"""
        import uvicorn
        uvicorn.run(self.app, host=host, port=port)
    
    def _setup_middleware(self) -> None:
        """Configure middleware stack"""
        # CORS middleware
        self.app.add_middleware(
            CORSMiddleware,
            allow_origins=["*"],
            allow_methods=["*"],
            allow_headers=["*"],
        )
        
        # Request ID middleware
        @self.app.middleware("http")
        async def add_correlation_id_middleware(request: Request, call_next: Callable) -> Response:
            request_id = str(uuid.uuid4())
            correlation_id.set(request_id)
            
            start_time = time.time()
            response = await call_next(request)
            process_time = time.time() - start_time
            
            response.headers["X-Request-ID"] = request_id
            response.headers["X-Process-Time"] = str(process_time)
            
            self.logger.info(
                "Request processed",
                method=request.method,
                path=request.url.path,
                status=response.status_code,
                duration=process_time
            )
            
            return response
    
    def _setup_error_handlers(self) -> None:
        """Configure error handlers"""
        @self.app.exception_handler(HTTPException)
        async def http_exception_handler(_: Request, exc: HTTPException):
            return JSONResponse(
                status_code=exc.status_code,
                content={
                    "error": {
                        "message": exc.detail,
                        "status_code": exc.status_code,
                        "request_id": correlation_id.get()
                    }
                }
            )