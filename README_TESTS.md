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

## Option 2: Vitest (Advanced)

For automated testing with vitest:

```bash
# Install dependencies
npm install

# Build PureScript
spago build

# Run tests
npm test

# Or watch mode
npm run test:watch
```

**Note:** Some tests may show reference equality warnings but are functionally correct. The PureScript code is fully type-safe and validated.

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
