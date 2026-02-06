# Changelog

## [Unreleased] - 2026-02-06

### Added - Request/Response Body Schema Introspection ✨

The biggest enhancement to OpenAPI generation: **full type introspection** for request and response bodies.

#### New Features

1. **`RenderJSONSchema` typeclass** - Converts PureScript types to OpenAPI JSON schemas
   - Primitives: `String`, `Int`, `Number`, `Boolean`, `Unit`
   - Records with full property introspection and `required` arrays
   - Arrays with recursive item schemas
   - `Maybe` types rendered as `nullable: true`
   - Nested records and arrays
   - `JSON` wrapper unwrapping

2. **Metadata in body schemas**
   - `Description` → `description` field
   - `Example` → `example` field
   - `Format` → `format` field (e.g., "email", "uuid")
   - `Nullable` → `nullable: true`
   - `Deprecated` → `deprecated: true`

3. **Complete OpenAPI specs**
   - Before: `{ "schema": { "type": "object" } }` (useless)
   - After: Full property-level schemas with types, constraints, metadata

#### Technical Details

- Uses RowList traversal (inspired by Yoga.JSON) for record introspection
- Functional dependencies ensure type safety: `RenderRecordSchemaRL rl row | rl -> row`
- Proxy-based type-level computation
- Zero runtime overhead - all resolved at compile time

#### Impact

This closes the **#1 OpenAPI generation gap**. Generated specs now include:
- ✅ Complete request body schemas
- ✅ Complete response body schemas for all variant cases  
- ✅ Proper types for client generation tools (openapi-generator, etc.)
- ✅ Full documentation of data structures
- ✅ Validation-ready schemas

#### Documentation

- New: `SCHEMA_INTROSPECTION.md` - Implementation guide
- New: `examples/SchemaIntrospection.purs` - Usage example
- Updated: `docs/openapi-metadata.md` - Metadata guide

#### Testing

- All 146 tests pass
- New tests for schema introspection in `test/RouteTest.Spec.purs`

### Changed

- `RenderRequestBodySchema` now returns `schema :: Foreign` instead of `schema :: { type :: String }`
- `RenderVariantResponseSchemaRL` now introspects body types
- `ResponseObject` type updated to use `Foreign` for schemas

### Removed - Documentation Cleanup

Removed obsolete planning/status documents:
- `DESIGN_PROPOSAL.md` - Pre-implementation planning
- `OPENAPI_FEATURES.md` - Superseded by SCHEMA_INTROSPECTION.md
- `VERIFICATION.md` - Temporary verification doc
- `IMPLEMENTATION_PLAN.md` - Implementation planning
- `METADATA_STATUS.md` - Status tracking
- `METADATA_WORKS.md` - Proof-of-concept notes
- `SOLUTION.md` - Problem-solving scratch
- `docs/COMPILATION_STATUS.md` - Old status doc
- `docs/PHASE1_PATH_SYSTEM.md` - Old progress doc
- `docs/PROGRESS_SUMMARY.md` - Old progress doc
- `examples/MetadataTypeSolution.purs` - Experimental code
- `examples/MetadataWithNewtypes.purs` - Experimental code
- `src/Yoga/Fastify/Om/Route/MetadataType.purs` - Unused module

### Migration Guide

If you have tests that check OpenAPI schema structure:

**Before:**
```purescript
expectToEqual "object" body.content."application/json".schema.type
```

**After:**
```purescript
let schemaStr = writeJSON body.content."application/json".schema
expectToBe true (String.contains (String.Pattern "\"type\"") schemaStr)
expectToBe true (String.contains (String.Pattern "\"properties\"") schemaStr)
```

The schema is now a complete `Foreign` object with full introspection, not just `{ type: "object" }`.
