import { defineConfig } from 'vitest/config'
import dts from "vite-plugin-dts";
import wasm from "vite-plugin-wasm";

export default defineConfig({
  plugins: [wasm(), dts()],
  resolve: {
    conditions: ['node', 'import', 'types']  // or whatever matches your needs
  },
  test: {
    tags: [
      {
        name: 'lifecycle',
        description: 'A basic contract lifecycle execution test',
      },
      {
        name: 'marlowe-cli',
        description: 'Tests that use solely the marlowe-cli tool for the contract interactions',
      },
      {
        name: 'marlowe-runtime-cli',
        description: 'Tests that use solely the marlowe-runtime-cli tool for the contract interactions',
      },
      {
        name: 'store',
        description: 'Tests for the contract source store (upload + queries)',
      },
    ],
  },
})
