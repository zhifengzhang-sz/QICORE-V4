/**
 * QiCore v4.0 - CLI Application Component
 *
 * Mathematical Contract-Based TypeScript Library
 * Component 8: CLIApplication - Command-line processing (5 operations)
 *
 * Using current 2024-2025 packages with corrections applied
 */

import { QiError } from "../../base/error.js";
import { Result } from "../../base/result.js";

/**
 * CLI application configuration
 */
export interface CLIOptions {
  name: string;
  version: string;
  description?: string;
}

/**
 * Command handler function type
 */
export type CommandHandler = (...args: unknown[]) => Promise<Result<unknown>> | Result<unknown>;

/**
 * CLIApplication - Command-line processing with modern tools
 *
 * Note: Using simplified implementation without external dependencies
 * Following corrected template patterns for TypeScript strict mode
 */
export class CLIApplication {
  private commands = new Map<string, { description: string; handler: CommandHandler }>();
  private options: CLIOptions;

  constructor(options: CLIOptions) {
    this.options = options;
  }

  /**
   * Operation 1: Add command
   */
  command(name: string, description: string, handler: CommandHandler): this {
    this.commands.set(name, { description, handler });
    return this;
  }

  /**
   * Operation 2: Add option (simplified implementation)
   */
  option(flags: string, description: string, defaultValue?: unknown): this {
    // Simplified option handling - in real implementation would use commander.js v14
    console.log(`Option registered: ${flags} - ${description}, default: ${defaultValue}`);
    return this;
  }

  /**
   * Operation 3: Run application
   */
  async run(argv?: string[]): Promise<void> {
    try {
      const args = argv || process.argv.slice(2);

      if (args.length === 0) {
        this.showHelp();
        return;
      }

      const commandName = args[0];
      const command = this.commands.get(commandName);

      if (!command) {
        console.error(`Unknown command: ${commandName}`);
        process.exit(1);
      }

      const result = await command.handler(...args.slice(1));
      this.handleResult(result);
    } catch (error: unknown) {
      this.handleError(error);
      process.exit(1);
    }
  }

  /**
   * Operation 4: Show spinner (simplified without ora dependency)
   */
  spinner(text: string): {
    start: () => void;
    stop: () => void;
    succeed: (text?: string) => void;
    fail: (text?: string) => void;
  } {
    let isSpinning = false;
    return {
      start: () => {
        isSpinning = true;
        console.log(`🔄 ${text}`);
      },
      stop: () => {
        isSpinning = false;
      },
      succeed: (successText?: string) => {
        if (isSpinning) {
          console.log(`✅ ${successText || text}`);
          isSpinning = false;
        }
      },
      fail: (failText?: string) => {
        if (isSpinning) {
          console.log(`❌ ${failText || text}`);
          isSpinning = false;
        }
      },
    };
  }

  /**
   * Operation 5: Display table (simplified without cli-table3)
   */
  table(headers: string[], rows: string[][]): void {
    console.log(`\n${headers.join("\t")}`);
    console.log(headers.map(() => "---").join("\t"));
    for (const row of rows) {
      console.log(row.join("\t"));
    }
    console.log("");
  }

  /**
   * Show help information
   */
  private showHelp(): void {
    console.log(`\n${this.options.name} v${this.options.version}`);
    if (this.options.description) {
      console.log(this.options.description);
    }
    console.log("\nCommands:");

    this.commands.forEach((command, name) => {
      console.log(`  ${name.padEnd(20)} ${command.description}`);
    });
    console.log("");
  }

  /**
   * Handle command result
   */
  private handleResult(result: Result<unknown>): void {
    if (result.isSuccess()) {
      const value = result.unwrap();
      if (value !== undefined && value !== null) {
        if (typeof value === "object") {
          console.log(JSON.stringify(value, null, 2));
        } else {
          console.log(value);
        }
      }
    } else {
      const error = result.error();
      console.error(`Error: ${error.message}`);
      process.exit(1);
    }
  }

  /**
   * Handle unhandled errors
   */
  private handleError(error: unknown): void {
    const message = error instanceof Error ? error.message : String(error);
    console.error(`Error: ${message}`);
  }
}
