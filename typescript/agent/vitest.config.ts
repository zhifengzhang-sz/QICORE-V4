import { defineConfig } from "vitest/config";

export default defineConfig({
	test: {
		globals: true,
		environment: "node",
		include: ["**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
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
	},
	resolve: {
		alias: {
			"@": new URL("./src", import.meta.url).pathname,
			"@mcp": new URL("./src/mcp", import.meta.url).pathname,
			"@agents": new URL("./src/agents", import.meta.url).pathname,
			"@utils": new URL("./src/utils", import.meta.url).pathname,
		},
	},
});
