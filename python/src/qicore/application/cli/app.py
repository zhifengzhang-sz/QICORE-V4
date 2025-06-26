# src/qicore/application/cli/app.py
import click
from typing import Any, Callable, Optional, List
from ...base.result import Result
from ...base.error import QiError
from ...core.logging import StructuredLogger
import asyncio
from functools import wraps
from rich.console import Console
from rich.table import Table
from rich.progress import Progress, SpinnerColumn, TextColumn

class CLIApplication:
    """Command-line application with Result-based error handling"""
    
    def __init__(
        self,
        name: str,
        version: str = "4.0.1",
        logger: Optional[StructuredLogger] = None
    ):
        self.name = name
        self.version = version
        self.logger = logger or StructuredLogger("cli")
        self.cli = click.Group(name=name)
        self.console = Console()
        self._setup_global_options()
    
    def _setup_global_options(self) -> None:
        """Add global CLI options"""
        self.cli = click.version_option(version=self.version)(self.cli)
    
    # Operation 1: Add command
    def command(
        self,
        name: Optional[str] = None,
        **kwargs: Any
    ) -> Callable:
        """Decorator for Result-based commands"""
        def decorator(fn: Callable) -> Callable:
            # Handle async functions
            if asyncio.iscoroutinefunction(fn):
                @wraps(fn)
                def wrapper(*args, **kwargs):
                    async def run():
                        result = await fn(*args, **kwargs)
                        return self._handle_result(result)
                    
                    return asyncio.run(run())
            else:
                @wraps(fn)
                def wrapper(*args, **kwargs):
                    result = fn(*args, **kwargs)
                    return self._handle_result(result)
            
            # Register with Click
            return self.cli.command(name=name, **kwargs)(wrapper)
        
        return decorator
    
    # Operation 2: Add group
    def group(self, name: str) -> click.Group:
        """Add command group"""
        group = click.Group(name)
        self.cli.add_command(group)
        return group
    
    # Operation 3: Run application
    def run(self) -> None:
        """Run the CLI application"""
        try:
            self.cli()
        except Exception as e:
            self.console.print(f"[red]Error: {e}[/red]")
            self.logger.error(f"CLI error: {e}", exc_info=True)
    
    # Operation 4: Show progress
    def progress(self, description: str) -> Progress:
        """Create progress indicator"""
        return Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=self.console
        )
    
    # Operation 5: Display table
    def display_table(self, title: str, columns: List[str], rows: List[List[Any]]) -> None:
        """Display data in table format"""
        table = Table(title=title, show_header=True)
        
        for column in columns:
            table.add_column(column)
        
        for row in rows:
            table.add_row(*[str(item) for item in row])
        
        self.console.print(table)
    
    def _handle_result(self, result: Any) -> Any:
        """Handle Result return values"""
        if isinstance(result, Result):
            if result.is_success():
                value = result.unwrap_or(None)
                if value is not None:
                    self.console.print(value)
                return 0
            else:
                # Extract error from failed result
                try:
                    error = result._inner._inner_value if hasattr(result._inner, '_inner_value') else None
                    if error and hasattr(error, 'message'):
                        self.logger.error(f"Command failed: {error.message}")
                        self.console.print(f"[red]Error: {error.message}[/red]")
                    else:
                        self.logger.error("Command failed with unknown error")
                        self.console.print("[red]Error: Unknown error occurred[/red]")
                except Exception as e:
                    self.logger.error(f"Error handling failed result: {e}")
                    self.console.print("[red]Error: Failed to process error[/red]")
                return 1
        return result