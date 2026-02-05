#!/usr/bin/env node

/**
 * TypeScript entry point for the PureScript example server
 *
 * This file imports the compiled PureScript module and starts the server.
 * Run with: bun run example
 */

import { main } from "../output/Example.Server/index.js";

console.log("Starting PureScript Fastify server...");
main();
