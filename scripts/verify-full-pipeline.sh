#!/bin/bash
#
# Full OpenAPI Validation Pipeline Verification Script
#
# This script runs the complete OpenAPI validation pipeline locally to verify
# that all components work together correctly. It simulates what the CI/CD
# pipeline does in GitHub Actions.
#
# Usage: ./scripts/verify-full-pipeline.sh
#
# Prerequisites:
#   - Java 21 installed
#   - Node.js 20+ installed (for Spectral)
#   - npm install -g @stoplight/spectral-cli (one-time setup)
#
# Exit codes:
#   0 - All verifications passed
#   1 - Server/build failure
#   2 - OpenAPI spec extraction failed
#   3 - Spectral linting failed
#   4 - Spec comparison failed (breaking changes detected)
#
# Pipeline Steps:
#   1. Build the application (./gradlew bootJar)
#   2. Extract OpenAPI spec (./gradlew extractOpenApiSpec)
#   3. Run Spectral linting (spectral lint build/openapi/openapi.json)
#   4. Compare with documented spec (./gradlew compareOpenApiSpecs)
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

echo ""
echo "============================================================"
echo "       OpenAPI Validation Pipeline Verification"
echo "============================================================"
echo ""
echo "Project: $PROJECT_ROOT"
echo "Date: $(date)"
echo ""

# Track timing
START_TIME=$(date +%s)

# Step 1: Check prerequisites
echo "=== Step 0: Checking Prerequisites ==="
echo ""

# Check Java version
JAVA_VERSION=$(java -version 2>&1 | head -n 1 | cut -d'"' -f2 | cut -d'.' -f1)
if [ "$JAVA_VERSION" -lt 17 ]; then
    echo "ERROR: Java 17 or higher required (found version $JAVA_VERSION)"
    exit 1
fi
echo "  ✓ Java version: $(java -version 2>&1 | head -n 1)"

# Check if Spectral is installed
if command -v spectral &> /dev/null; then
    echo "  ✓ Spectral CLI: $(spectral --version 2>/dev/null || echo 'installed')"
else
    echo "  ⚠ Spectral CLI not found - installing..."
    npm install -g @stoplight/spectral-cli
    echo "  ✓ Spectral CLI installed"
fi

# Check Node.js version
if command -v node &> /dev/null; then
    echo "  ✓ Node.js version: $(node --version)"
else
    echo "  ⚠ Node.js not found - Spectral linting will be skipped"
fi

echo ""

# Step 1: Build the application
echo "=== Step 1: Build Application ==="
echo ""
echo "Running: ./gradlew bootJar"
echo ""

if ./gradlew bootJar --console=plain; then
    echo ""
    echo "  ✓ Build successful"
else
    echo ""
    echo "  ✗ Build failed"
    exit 1
fi

echo ""

# Step 2: Extract OpenAPI Spec
echo "=== Step 2: Extract OpenAPI Specification ==="
echo ""
echo "Running: ./gradlew extractOpenApiSpec"
echo ""
echo "This will start the server, fetch /v3/api-docs, and save to build/openapi/openapi.json"
echo ""

if ./gradlew extractOpenApiSpec --console=plain; then
    echo ""
    echo "  ✓ OpenAPI spec extracted successfully"

    # Verify the file exists and has content
    if [ -f "build/openapi/openapi.json" ]; then
        FILE_SIZE=$(wc -c < "build/openapi/openapi.json" | tr -d ' ')
        echo "  ✓ OpenAPI spec file: build/openapi/openapi.json (${FILE_SIZE} bytes)"

        # Quick validation of JSON structure
        if python3 -c "import json; json.load(open('build/openapi/openapi.json'))" 2>/dev/null; then
            echo "  ✓ Valid JSON structure"

            # Extract some key info
            OPENAPI_VERSION=$(python3 -c "import json; print(json.load(open('build/openapi/openapi.json')).get('openapi', 'unknown'))")
            PATHS_COUNT=$(python3 -c "import json; print(len(json.load(open('build/openapi/openapi.json')).get('paths', {})))")
            echo "  ✓ OpenAPI version: $OPENAPI_VERSION"
            echo "  ✓ Paths documented: $PATHS_COUNT"
        else
            echo "  ⚠ Could not validate JSON structure"
        fi
    else
        echo "  ✗ OpenAPI spec file not found"
        exit 2
    fi
else
    echo ""
    echo "  ✗ Failed to extract OpenAPI spec"
    exit 2
fi

echo ""

# Step 3: Spectral Linting
echo "=== Step 3: Spectral Linting ==="
echo ""
echo "Running: spectral lint build/openapi/openapi.json --fail-severity=error"
echo ""

if command -v spectral &> /dev/null; then
    # Run spectral and capture output
    SPECTRAL_OUTPUT=$(spectral lint build/openapi/openapi.json --fail-severity=error 2>&1) || SPECTRAL_EXIT=$?

    echo "$SPECTRAL_OUTPUT"
    echo ""

    if [ -z "$SPECTRAL_EXIT" ] || [ "$SPECTRAL_EXIT" -eq 0 ]; then
        echo "  ✓ Spectral linting passed (no errors)"
    else
        echo ""
        echo "  ✗ Spectral linting failed with exit code $SPECTRAL_EXIT"
        echo ""
        echo "  Review the errors above and update the OpenAPI annotations to fix them."
        exit 3
    fi
else
    echo "  ⚠ Spectral CLI not available - skipping linting"
    echo "  Install with: npm install -g @stoplight/spectral-cli"
fi

echo ""

# Step 4: Spec Comparison
echo "=== Step 4: Specification Comparison ==="
echo ""
echo "Running: ./gradlew compareOpenApiSpecs"
echo ""
echo "Comparing generated spec against documented spec for breaking changes..."
echo "  Documented: docs/monica-api-openapi.yaml"
echo "  Generated:  build/openapi/openapi.json"
echo ""

# Check if documented spec exists
if [ ! -f "docs/monica-api-openapi.yaml" ]; then
    echo "  ⚠ Documented spec not found: docs/monica-api-openapi.yaml"
    echo "  Skipping spec comparison..."
    SKIP_COMPARISON=true
else
    SKIP_COMPARISON=false
fi

if [ "$SKIP_COMPARISON" = false ]; then
    # Run comparison and capture exit code
    if ./gradlew compareOpenApiSpecs --console=plain 2>&1; then
        echo ""
        echo "  ✓ Spec comparison passed (no breaking changes)"
    else
        COMPARE_EXIT=$?
        echo ""
        if [ $COMPARE_EXIT -eq 1 ]; then
            echo "  ✗ Breaking changes detected"
            echo ""
            echo "  The generated specification has breaking changes that may affect clients."
            echo "  Review the differences above and either:"
            echo "    1. Update the documented spec to match the new API"
            echo "    2. Fix the controller annotations to match the documented spec"
            exit 4
        else
            echo "  ✗ Spec comparison failed with error code $COMPARE_EXIT"
            exit 4
        fi
    fi
else
    echo "  ⚠ Spec comparison skipped (documented spec not found)"
fi

echo ""

# Calculate total time
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Summary
echo "============================================================"
echo "                      SUMMARY"
echo "============================================================"
echo ""
echo "  Total time: ${DURATION} seconds"
echo ""
echo "  Step 1 (Build):          ✓ Passed"
echo "  Step 2 (Extract Spec):   ✓ Passed"
if command -v spectral &> /dev/null; then
    echo "  Step 3 (Spectral Lint):  ✓ Passed"
else
    echo "  Step 3 (Spectral Lint):  ⚠ Skipped (Spectral not installed)"
fi
if [ "$SKIP_COMPARISON" = false ]; then
    echo "  Step 4 (Spec Compare):   ✓ Passed"
else
    echo "  Step 4 (Spec Compare):   ⚠ Skipped (documented spec not found)"
fi
echo ""
echo "============================================================"
echo "        All validations passed successfully!"
echo "============================================================"
echo ""
echo "Generated files:"
echo "  - build/openapi/openapi.json"
echo ""
echo "Next steps:"
echo "  - View Swagger UI: ./gradlew bootRun, then http://localhost:8080/swagger-ui.html"
echo "  - View API docs:   curl http://localhost:8080/v3/api-docs | jq ."
echo ""

exit 0
