# Running Tests

## Option 1: Manual Console Tests (Simplest)

Run the test files directly to see output:

```bash
# Build the project
spago build

# Run operator rendering tests
spago run -m Test.OperatorTest

# Run parser tests  
spago run -m Test.ParserTest
```

These will output test results showing:
- Path rendering to OpenAPI format
- URL parsing to typed records
- Required vs optional query param handling
- Error cases

## Option 2: Vitest (Automated)

For automated testing with vitest:

```bash
# Install dependencies
bun install

# Build PureScript
spago build

# Run tests
bun test

# Or watch mode
bun run test:watch
```

**All 20 tests passing!** ✅

## What's Being Tested

✅ Path Rendering
- Simple paths with captures: `/users/{id}/posts`
- Paths with query params: `/api/posts?page={page}&sort={sort}`

✅ Segment Parsing
- Matching literal segments
- Failing on mismatches

✅ Capture Parsing  
- Parsing Int captures
- Parsing String captures
- Failing on invalid types

✅ Complete Path Parsing
- Extracting path captures into records
- Handling invalid paths

✅ Optional Query Parameters
- All params present → `{ limit: Just 10, offset: Just 20 }`
- Some missing → `{ limit: Just 10, offset: Nothing }`
- All missing → `{ limit: Nothing, offset: Nothing }`

✅ Required Query Parameters
- Present → `{ limit: 10 }` (plain Int, not Maybe)
- Missing → `Left ["Missing required query parameter: limit"]`

✅ Error Handling
- Invalid path structures
- Type mismatches in captures
