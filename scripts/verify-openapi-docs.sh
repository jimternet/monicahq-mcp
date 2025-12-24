#!/bin/bash
#
# Verify /v3/api-docs returns valid OpenAPI 3.0.3 JSON.
# This script validates the OpenAPI specification structure and content.
#
# Usage: ./scripts/verify-openapi-docs.sh [--no-server]
#
# Options:
#   --no-server    Assume server is already running (skip starting server)
#
# Exit codes:
#   0 - All verifications passed
#   1 - Server failed to start
#   2 - API docs not accessible (non-200 response)
#   3 - Invalid JSON response
#   4 - Invalid OpenAPI version (not 3.0.x)
#   5 - Missing required paths
#   6 - Missing required tags
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SERVER_PORT=8080
MAX_WAIT_SECONDS=60
NO_SERVER=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-server)
            NO_SERVER=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=== OpenAPI Docs Verification Script ==="
echo "Project: $PROJECT_ROOT"
echo "Port: $SERVER_PORT"
echo ""

# Function to cleanup on exit
cleanup() {
    if [ "$NO_SERVER" = false ] && [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        echo "Stopping server (PID: $SERVER_PID)..."
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

cd "$PROJECT_ROOT"

# Start the server if not in no-server mode
if [ "$NO_SERVER" = false ]; then
    echo "Starting server..."
    ./gradlew bootRun --console=plain > /tmp/server-output.log 2>&1 &
    SERVER_PID=$!
    echo "Server PID: $SERVER_PID"

    # Wait for server to be ready
    echo "Waiting for server to be ready..."
    SECONDS_WAITED=0
    while [ $SECONDS_WAITED -lt $MAX_WAIT_SECONDS ]; do
        if curl -s -o /dev/null -w '' "http://localhost:$SERVER_PORT/health" 2>/dev/null; then
            echo "Server is ready!"
            break
        fi
        sleep 1
        SECONDS_WAITED=$((SECONDS_WAITED + 1))
        if [ $((SECONDS_WAITED % 10)) -eq 0 ]; then
            echo "  Still waiting... ($SECONDS_WAITED seconds)"
        fi
    done

    if [ $SECONDS_WAITED -ge $MAX_WAIT_SECONDS ]; then
        echo "ERROR: Server failed to start within $MAX_WAIT_SECONDS seconds"
        echo "Server output:"
        cat /tmp/server-output.log
        exit 1
    fi
else
    echo "Assuming server is already running on port $SERVER_PORT"
fi

echo ""
echo "=== Running OpenAPI Docs Verifications ==="

# Verification 1: API docs endpoint returns HTTP 200
echo ""
echo "1. Checking HTTP status of /v3/api-docs..."
API_DOCS_STATUS=$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:$SERVER_PORT/v3/api-docs")
if [ "$API_DOCS_STATUS" = "200" ]; then
    echo "   OK: API docs returned HTTP 200"
else
    echo "   FAIL: API docs returned HTTP $API_DOCS_STATUS (expected 200)"
    exit 2
fi

# Fetch API docs for subsequent checks
API_DOCS=$(curl -s "http://localhost:$SERVER_PORT/v3/api-docs")

# Verification 2: Response is valid JSON
echo ""
echo "2. Validating JSON response..."
if echo "$API_DOCS" | python3 -c "import sys, json; json.load(sys.stdin)" 2>/dev/null; then
    echo "   OK: Response is valid JSON"
else
    echo "   FAIL: Response is not valid JSON"
    echo "   First 500 characters of response:"
    echo "$API_DOCS" | head -c 500
    exit 3
fi

# Verification 3: OpenAPI version is 3.0.x or 3.1.x
echo ""
echo "3. Checking OpenAPI version..."
OPENAPI_VERSION=$(echo "$API_DOCS" | python3 -c "import sys, json; print(json.load(sys.stdin).get('openapi', ''))")
if [[ "$OPENAPI_VERSION" == 3.0* ]] || [[ "$OPENAPI_VERSION" == 3.1* ]]; then
    echo "   OK: OpenAPI version is $OPENAPI_VERSION"
else
    echo "   FAIL: OpenAPI version is '$OPENAPI_VERSION' (expected 3.0.x or 3.1.x)"
    exit 4
fi

# Verification 4: Info section is present
echo ""
echo "4. Checking info section..."
INFO_TITLE=$(echo "$API_DOCS" | python3 -c "import sys, json; print(json.load(sys.stdin).get('info', {}).get('title', ''))")
INFO_VERSION=$(echo "$API_DOCS" | python3 -c "import sys, json; print(json.load(sys.stdin).get('info', {}).get('version', ''))")
if [ -n "$INFO_TITLE" ] && [ -n "$INFO_VERSION" ]; then
    echo "   OK: Info present - Title: '$INFO_TITLE', Version: '$INFO_VERSION'"
else
    echo "   WARN: Info section incomplete - Title: '$INFO_TITLE', Version: '$INFO_VERSION'"
fi

# Verification 5: Required paths are documented
echo ""
echo "5. Checking required paths..."
PATHS_JSON=$(echo "$API_DOCS" | python3 -c "import sys, json; d=json.load(sys.stdin); print(','.join(d.get('paths', {}).keys()))")

check_path() {
    local path=$1
    if echo "$PATHS_JSON" | grep -q "$path"; then
        echo "   OK: $path is documented"
        return 0
    else
        echo "   FAIL: $path is NOT documented"
        return 1
    fi
}

PATHS_OK=true
check_path "/health" || PATHS_OK=false
check_path "/health/live" || PATHS_OK=false
check_path "/health/ready" || PATHS_OK=false
check_path "/health/detailed" || PATHS_OK=false
check_path "/health/ping" || PATHS_OK=false
check_path "/mcp" || PATHS_OK=false

if [ "$PATHS_OK" = false ]; then
    echo ""
    echo "   Some required paths are missing!"
    exit 5
fi

# Verification 6: Required tags are present
echo ""
echo "6. Checking required tags..."
TAGS_JSON=$(echo "$API_DOCS" | python3 -c "import sys, json; d=json.load(sys.stdin); tags=d.get('tags', []); print(','.join([t.get('name', '') for t in tags]))")

check_tag() {
    local tag=$1
    if echo "$TAGS_JSON" | grep -q "$tag"; then
        echo "   OK: '$tag' tag is present"
        return 0
    else
        echo "   FAIL: '$tag' tag is NOT present"
        return 1
    fi
}

TAGS_OK=true
check_tag "Health" || TAGS_OK=false
check_tag "MCP" || TAGS_OK=false

if [ "$TAGS_OK" = false ]; then
    echo ""
    echo "   Some required tags are missing!"
    exit 6
fi

# Verification 7: Operations have required fields
echo ""
echo "7. Checking operation structure..."
OPERATIONS_CHECK=$(echo "$API_DOCS" | python3 -c "
import sys, json
data = json.load(sys.stdin)
paths = data.get('paths', {})
errors = []
for path, methods in paths.items():
    for method, operation in methods.items():
        if method in ['get', 'post', 'put', 'delete', 'patch']:
            if not operation.get('operationId'):
                errors.append(f'{method.upper()} {path}: missing operationId')
            if not operation.get('responses'):
                errors.append(f'{method.upper()} {path}: missing responses')
            if not operation.get('tags'):
                errors.append(f'{method.upper()} {path}: missing tags')
if errors:
    for e in errors:
        print(f'   WARN: {e}')
else:
    print('   OK: All operations have operationId, responses, and tags')
")
echo "$OPERATIONS_CHECK"

# Verification 8: Response content types
echo ""
echo "8. Checking response content types..."
CONTENT_TYPE_CHECK=$(echo "$API_DOCS" | python3 -c "
import sys, json
data = json.load(sys.stdin)
paths = data.get('paths', {})
has_json_responses = False
for path, methods in paths.items():
    for method, operation in methods.items():
        if method in ['get', 'post', 'put', 'delete', 'patch']:
            responses = operation.get('responses', {})
            for code, response in responses.items():
                content = response.get('content', {})
                if 'application/json' in content:
                    has_json_responses = True
                    break
print('OK: JSON responses are defined' if has_json_responses else 'WARN: No JSON responses found')
")
echo "   $CONTENT_TYPE_CHECK"

echo ""
echo "=== OpenAPI Docs Verification Complete ==="
echo "All critical checks passed!"
echo ""
echo "Summary:"
echo "  - OpenAPI Version: $OPENAPI_VERSION"
echo "  - Title: $INFO_TITLE"
echo "  - Paths documented: $(echo "$PATHS_JSON" | tr ',' '\n' | wc -l | tr -d ' ')"
echo "  - Tags: $TAGS_JSON"
echo ""
echo "To view full API docs:"
echo "  curl -s http://localhost:$SERVER_PORT/v3/api-docs | jq ."
echo ""
