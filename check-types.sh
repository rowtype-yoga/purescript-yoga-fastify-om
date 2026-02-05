#!/bin/bash
echo "Checking if PureScript files have valid syntax..."
for file in src/Yoga/Fastify/Om/*.purs test/StandaloneExample.purs; do
  if [ -f "$file" ]; then
    echo "Checking $file..."
    purs graph "$file" 2>&1 | head -5
  fi
done
