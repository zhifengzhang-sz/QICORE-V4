import { defineConfig } from "vitest/config";

export default defineConfig({
	test: {
		globals: true,
		environment: "node",
		include: ["../tests/app/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
		exclude: ["node_modules", "dist", ".idea", ".git", ".cache"],
		coverage: {
			provider: "v8",
			reporter: ["text", "json", "html"],
			include: ["src/**/*.ts"],
			exclude: [
				"node_modules/",
				"dist/",
				"**/*.d.ts",
				"**/*.config.*",
				"**/coverage/**",
				"src/**/index.ts", // Exclude index.ts files (just exports)
			],
		},
		typecheck: {
			enabled: true,
		},
		// Vitest 3.0+ ES modules configuration
		pool: "forks",
		poolOptions: {
			forks: {
				singleFork: true,
			},
		},
	},
	resolve: {
		alias: {
			"@": new URL("./src", import.meta.url).pathname,
			"@qi/core/base": new URL("../../lib/src/qicore/base/index.ts", import.meta.url).pathname,
			"@qi/core": new URL("../../lib/src/qicore/index.ts", import.meta.url).pathname,
			"@qi/mcp": new URL("../lib/src/qimcp/client.ts", import.meta.url).pathname,
			"@qi/mcp/tools/file": new URL("../lib/src/qimcp/tools/file.ts", import.meta.url).pathname,
			"@qi/mcp/tools/memory": new URL("../lib/src/qimcp/tools/memory.ts", import.meta.url).pathname,
			"@qi/agent": new URL("../lib/src/qiagent/index.ts", import.meta.url).pathname,
			"@qi/prompt": new URL("../lib/src/qiprompt/index.ts", import.meta.url).pathname,
		},
	},
	// Vitest 3.0+ esbuild configuration for proper ES modules
	esbuild: {
		target: "node18",
	},
});
