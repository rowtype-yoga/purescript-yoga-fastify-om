import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Include the compiled PureScript test output
    include: ["output/Test.Operators/index.js"],
    globals: true,
    // Use jsdom environment to provide window object
    environment: "jsdom",
  },
});
