#!/bin/bash

# MonicaHQ MCP Server - Discovery Tools Validation Script
# This script validates discovery tools using the built MCP server

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

log_success() {
    echo -e "${GREEN}✅ PASSED:${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
}

log_error() {
    echo -e "${RED}❌ FAILED:${NC} $1"
}

echo "🧪 MonicaHQ MCP Server - Discovery Tools Validation"
echo "=================================================="

TESTS_PASSED=0
TESTS_FAILED=0

# Step 1: Build the MCP server
echo
echo "Step 1: Building MCP Server"
echo "=========================="

log_info "Building MCP server JAR..."
if ./gradlew build; then
    log_success "MCP server built successfully"
else
    log_error "Failed to build MCP server"
    exit 1
fi

# Step 2: Start the MCP server in background
echo
echo "Step 2: Starting MCP Server"
echo "=========================="

# Set dummy environment variables for testing
export MONICA_API_URL="https://httpbin.org/status/200"  # Mock endpoint that returns 200
export MONICA_API_TOKEN="dummy-token-for-testing"

log_info "Starting MCP server in background..."
java -jar build/libs/monicahqmcp-0.1.0.jar --server.port=8082 &
MCP_PID=$!

log_info "Waiting for MCP server to start..."
sleep 10

# Function to cleanup background process
cleanup() {
    if [ ! -z "$MCP_PID" ]; then
        log_info "Stopping MCP server (PID: $MCP_PID)"
        kill $MCP_PID 2>/dev/null || true
    fi
}
trap cleanup EXIT

# Step 3: Test tool listing
echo
echo "Step 3: Testing Tool Discovery"
echo "============================="

log_info "Testing tools/list endpoint..."

TOOLS_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' \
    http://localhost:8082/mcp/tools)

if echo "$TOOLS_RESPONSE" | jq -e '.result.tools' > /dev/null 2>&1; then
    TOOL_COUNT=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools | length')
    log_success "Tool listing works - Found $TOOL_COUNT tools"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    
    # Check if our discovery tools are present
    if echo "$TOOLS_RESPONSE" | jq -e '.result.tools[] | select(.name=="gender_list")' > /dev/null 2>&1; then
        log_success "gender_list tool is registered"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "gender_list tool is NOT registered"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
    
    if echo "$TOOLS_RESPONSE" | jq -e '.result.tools[] | select(.name=="contact_field_type_list")' > /dev/null 2>&1; then
        log_success "contact_field_type_list tool is registered"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "contact_field_type_list tool is NOT registered"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
    
else
    log_error "Tool listing failed - Response: $TOOLS_RESPONSE"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Step 4: Test schema validation
echo
echo "Step 4: Testing Tool Schemas"
echo "==========================="

# Test gender_list schema
GENDER_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="gender_list") | .inputSchema')
if [ "$GENDER_SCHEMA" != "null" ]; then
    log_success "gender_list has proper schema definition"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    
    # Verify it requires no parameters (discovery tool)
    REQUIRED_PARAMS=$(echo "$GENDER_SCHEMA" | jq -r '.required // [] | length')
    if [ "$REQUIRED_PARAMS" = "0" ]; then
        log_success "gender_list correctly requires no parameters"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "gender_list should not require parameters (discovery tool)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
else
    log_error "gender_list missing schema definition"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test contact_field_type_list schema
FIELD_TYPE_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="contact_field_type_list") | .inputSchema')
if [ "$FIELD_TYPE_SCHEMA" != "null" ]; then
    log_success "contact_field_type_list has proper schema definition"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    
    # Verify it requires no parameters (discovery tool)
    REQUIRED_PARAMS=$(echo "$FIELD_TYPE_SCHEMA" | jq -r '.required // [] | length')
    if [ "$REQUIRED_PARAMS" = "0" ]; then
        log_success "contact_field_type_list correctly requires no parameters"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "contact_field_type_list should not require parameters (discovery tool)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
else
    log_error "contact_field_type_list missing schema definition"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Step 5: Test updated contact schemas
echo
echo "Step 5: Testing Updated Contact Schemas"
echo "===================================="

CONTACT_CREATE_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="contact_create") | .inputSchema')
GENDER_DESC=$(echo "$CONTACT_CREATE_SCHEMA" | jq -r '.properties.genderId.description')

if echo "$GENDER_DESC" | grep -q "gender_list"; then
    log_success "contact_create schema references gender_list discovery tool"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    log_error "contact_create schema does not reference gender_list tool"
    log_error "Current description: $GENDER_DESC"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Check if hardcoded enum is removed
GENDER_ENUM=$(echo "$CONTACT_CREATE_SCHEMA" | jq -r '.properties.genderId.enum // empty')
if [ -z "$GENDER_ENUM" ]; then
    log_success "contact_create no longer has hardcoded gender enum"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    log_error "contact_create still has hardcoded gender enum: $GENDER_ENUM"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Step 6: Test contact field schema
CONTACT_FIELD_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="contact_field_create") | .inputSchema')
FIELD_TYPE_DESC=$(echo "$CONTACT_FIELD_SCHEMA" | jq -r '.properties.contactFieldTypeId.description')

if echo "$FIELD_TYPE_DESC" | grep -q "contact_field_type_list"; then
    log_success "contact_field_create schema references contact_field_type_list discovery tool"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    log_error "contact_field_create schema does not reference contact_field_type_list tool"
    log_error "Current description: $FIELD_TYPE_DESC"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

echo
echo "Validation Summary"
echo "================="
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "All discovery tool validations passed!"
    echo
    echo "🎉 Constitutional Principle VII Implementation Validated!"
    echo "✅ Discovery tools are properly registered"
    echo "✅ Schemas reference discovery tools instead of hardcoded values"
    echo "✅ Tools follow constitutional requirements"
    echo
    echo "Next steps:"
    echo "1. Tools are ready for Claude Desktop testing"
    echo "2. Run against real Monica instance when ready"
    echo "3. Users can now discover valid IDs instead of guessing"
    exit 0
else
    log_error "Some validations failed. Check the details above."
    exit 1
fi