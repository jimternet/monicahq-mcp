#!/bin/bash
#
# Verify Swagger UI is accessible and functioning correctly.
# This script starts the server, verifies endpoints, and shuts down.
#
# Usage: ./scripts/verify-swagger-ui.sh
#
# Exit codes:
#   0 - All verifications passed
#   1 - Server failed to start
#   2 - Swagger UI not accessible
#   3 - API docs not accessible
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SERVER_PORT=8080
MAX_WAIT_SECONDS=60

echo "=== Swagger UI Verification Script ==="
echo "Project: $PROJECT_ROOT"
echo "Port: $SERVER_PORT"
echo ""

# Function to cleanup on exit
cleanup() {
    if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        echo "Stopping server (PID: $SERVER_PID)..."
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

cd "$PROJECT_ROOT"

# Start the server in background
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

echo ""
echo "=== Running Verifications ==="

# Verify 1: Swagger UI is accessible
echo ""
echo "1. Checking Swagger UI at /swagger-ui.html..."
SWAGGER_STATUS=$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:$SERVER_PORT/swagger-ui.html")
if [ "$SWAGGER_STATUS" = "200" ]; then
    echo "   ✓ Swagger UI returned HTTP 200"
else
    echo "   ✗ Swagger UI returned HTTP $SWAGGER_STATUS (expected 200)"
    exit 2
fi

# Verify 2: API docs endpoint is accessible and returns valid JSON
echo ""
echo "2. Checking API docs at /v3/api-docs..."
API_DOCS_STATUS=$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:$SERVER_PORT/v3/api-docs")
if [ "$API_DOCS_STATUS" = "200" ]; then
    echo "   ✓ API docs returned HTTP 200"
else
    echo "   ✗ API docs returned HTTP $API_DOCS_STATUS (expected 200)"
    exit 3
fi

# Verify 3: API docs contains expected content
echo ""
echo "3. Verifying API docs content..."
API_DOCS=$(curl -s "http://localhost:$SERVER_PORT/v3/api-docs")

# Check for OpenAPI version
OPENAPI_VERSION=$(echo "$API_DOCS" | grep -o '"openapi":"[^"]*"' | head -1 || true)
if [ -n "$OPENAPI_VERSION" ]; then
    echo "   ✓ OpenAPI version: $OPENAPI_VERSION"
else
    echo "   ✗ Could not find OpenAPI version"
fi

# Check for paths
if echo "$API_DOCS" | grep -q '"/health"'; then
    echo "   ✓ /health endpoint documented"
else
    echo "   ✗ /health endpoint not found in docs"
fi

if echo "$API_DOCS" | grep -q '"/mcp"'; then
    echo "   ✓ /mcp endpoint documented"
else
    echo "   ✗ /mcp endpoint not found in docs"
fi

# Check for tags
if echo "$API_DOCS" | grep -q '"name":"Health"'; then
    echo "   ✓ 'Health' tag present"
else
    echo "   ✗ 'Health' tag not found"
fi

if echo "$API_DOCS" | grep -q '"name":"MCP"'; then
    echo "   ✓ 'MCP' tag present"
else
    echo "   ✗ 'MCP' tag not found"
fi

# Verify 4: Health ping endpoint works
echo ""
echo "4. Testing /health/ping endpoint..."
PING_RESPONSE=$(curl -s "http://localhost:$SERVER_PORT/health/ping")
if echo "$PING_RESPONSE" | grep -q '"message":"pong"'; then
    echo "   ✓ /health/ping returned 'pong'"
else
    echo "   ✗ /health/ping did not return expected response"
    echo "   Response: $PING_RESPONSE"
fi

echo ""
echo "=== Verification Complete ==="
echo "All checks passed! Swagger UI is functioning correctly."
echo ""
echo "To view Swagger UI in browser:"
echo "  1. Run: ./gradlew bootRun"
echo "  2. Open: http://localhost:$SERVER_PORT/swagger-ui.html"
