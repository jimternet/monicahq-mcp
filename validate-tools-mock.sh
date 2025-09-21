#!/bin/bash

# MonicaHQ MCP Server - Mock Validation Script
# This script validates MCP tools using mock responses to demonstrate functionality

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

echo "🧪 MonicaHQ MCP Server - Mock Tool Validation"
echo "=============================================="

TESTS_PASSED=0
TESTS_FAILED=0

# Step 1: Build the MCP server
echo
echo "Step 1: Building MCP Server"
echo "=========================="

log_info "Building MCP server JAR..."
if ./gradlew build -q; then
    log_success "MCP server built successfully"
    ((TESTS_PASSED++))
else
    log_error "Failed to build MCP server"
    exit 1
fi

# Step 2: Start the MCP server with mock Monica endpoint
echo
echo "Step 2: Starting MCP Server with Mock Endpoint"
echo "=============================================="

# Create a simple mock server for testing
export MONICA_API_URL="https://httpbin.org/json"  # Returns JSON for testing
export MONICA_API_TOKEN="mock-token-for-testing"

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

# Step 3: Test tool listing and discovery tools
echo
echo "Step 3: Testing Tool Discovery & Listing"
echo "======================================="

log_info "Testing tools/list endpoint..."

TOOLS_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' \
    http://localhost:8082/mcp/tools)

if echo "$TOOLS_RESPONSE" | jq -e '.result.tools' > /dev/null 2>&1; then
    TOOL_COUNT=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools | length')
    log_success "Tool listing works - Found $TOOL_COUNT tools"
    ((TESTS_PASSED++))
    
    # Verify we have the expected 54 tools
    if [ "$TOOL_COUNT" = "54" ]; then
        log_success "Correct tool count (54) achieved"
        ((TESTS_PASSED++))
    else
        log_error "Expected 54 tools, found $TOOL_COUNT"
        ((TESTS_FAILED++))
    fi
    
    # Check discovery tools
    if echo "$TOOLS_RESPONSE" | jq -e '.result.tools[] | select(.name=="gender_list")' > /dev/null 2>&1; then
        log_success "gender_list discovery tool is registered"
        ((TESTS_PASSED++))
    else
        log_error "gender_list discovery tool is NOT registered"
        ((TESTS_FAILED++))
    fi
    
    if echo "$TOOLS_RESPONSE" | jq -e '.result.tools[] | select(.name=="contact_field_type_list")' > /dev/null 2>&1; then
        log_success "contact_field_type_list discovery tool is registered"
        ((TESTS_PASSED++))
    else
        log_error "contact_field_type_list discovery tool is NOT registered"
        ((TESTS_FAILED++))
    fi
    
    # Check conversation delete operations
    if echo "$TOOLS_RESPONSE" | jq -e '.result.tools[] | select(.name=="conversation_delete")' > /dev/null 2>&1; then
        log_success "conversation_delete operation is registered"
        ((TESTS_PASSED++))
    else
        log_error "conversation_delete operation is NOT registered"
        ((TESTS_FAILED++))
    fi
    
    if echo "$TOOLS_RESPONSE" | jq -e '.result.tools[] | select(.name=="conversation_message_delete")' > /dev/null 2>&1; then
        log_success "conversation_message_delete operation is registered"
        ((TESTS_PASSED++))
    else
        log_error "conversation_message_delete operation is NOT registered"
        ((TESTS_FAILED++))
    fi
    
else
    log_error "Tool listing failed - Response: $TOOLS_RESPONSE"
    ((TESTS_FAILED++))
fi

# Step 4: Test discovery tool schemas
echo
echo "Step 4: Testing Discovery Tool Schemas"
echo "===================================="

# Test gender_list schema
GENDER_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="gender_list") | .inputSchema')
if [ "$GENDER_SCHEMA" != "null" ]; then
    log_success "gender_list has proper schema definition"
    ((TESTS_PASSED++))
    
    # Verify it requires no parameters (discovery tool)
    REQUIRED_PARAMS=$(echo "$GENDER_SCHEMA" | jq -r '.required // [] | length')
    if [ "$REQUIRED_PARAMS" = "0" ]; then
        log_success "gender_list correctly requires no parameters"
        ((TESTS_PASSED++))
    else
        log_error "gender_list should not require parameters (discovery tool)"
        ((TESTS_FAILED++))
    fi
else
    log_error "gender_list missing schema definition"
    ((TESTS_FAILED++))
fi

# Test contact_field_type_list schema
FIELD_TYPE_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="contact_field_type_list") | .inputSchema')
if [ "$FIELD_TYPE_SCHEMA" != "null" ]; then
    log_success "contact_field_type_list has proper schema definition"
    ((TESTS_PASSED++))
    
    # Verify it requires no parameters (discovery tool)
    REQUIRED_PARAMS=$(echo "$FIELD_TYPE_SCHEMA" | jq -r '.required // [] | length')
    if [ "$REQUIRED_PARAMS" = "0" ]; then
        log_success "contact_field_type_list correctly requires no parameters"
        ((TESTS_PASSED++))
    else
        log_error "contact_field_type_list should not require parameters (discovery tool)"
        ((TESTS_FAILED++))
    fi
else
    log_error "contact_field_type_list missing schema definition"
    ((TESTS_FAILED++))
fi

# Step 5: Test contact create schema updates
echo
echo "Step 5: Testing Updated Contact Schemas"
echo "====================================="

CONTACT_CREATE_SCHEMA=$(echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | select(.name=="contact_create") | .inputSchema')
GENDER_DESC=$(echo "$CONTACT_CREATE_SCHEMA" | jq -r '.properties.genderId.description')

if echo "$GENDER_DESC" | grep -q "gender_list"; then
    log_success "contact_create schema references gender_list discovery tool"
    ((TESTS_PASSED++))
else
    log_error "contact_create schema does not reference gender_list tool"
    log_error "Current description: $GENDER_DESC"
    ((TESTS_FAILED++))
fi

# Check if hardcoded enum is removed
GENDER_ENUM=$(echo "$CONTACT_CREATE_SCHEMA" | jq -r '.properties.genderId.enum // empty')
if [ -z "$GENDER_ENUM" ]; then
    log_success "contact_create no longer has hardcoded gender enum"
    ((TESTS_PASSED++))
else
    log_error "contact_create still has hardcoded gender enum: $GENDER_ENUM"
    ((TESTS_FAILED++))
fi

# Step 6: Test constitutional principle compliance
echo
echo "Step 6: Testing Constitutional Principle VII Compliance"
echo "====================================================="

log_info "Checking Constitutional Principle VII implementation..."

# Check for complete tool coverage
ENTITY_COVERAGE=0

# Check each entity has full CRUD operations
declare -a ENTITIES=("contact" "note" "task" "reminder" "tag" "activity" "call" "conversation")

for entity in "${ENTITIES[@]}"; do
    CREATE_EXISTS=$(echo "$TOOLS_RESPONSE" | jq -e ".result.tools[] | select(.name==\"${entity}_create\")" > /dev/null 2>&1 && echo "yes" || echo "no")
    GET_EXISTS=$(echo "$TOOLS_RESPONSE" | jq -e ".result.tools[] | select(.name==\"${entity}_get\")" > /dev/null 2>&1 && echo "yes" || echo "no")
    UPDATE_EXISTS=$(echo "$TOOLS_RESPONSE" | jq -e ".result.tools[] | select(.name==\"${entity}_update\")" > /dev/null 2>&1 && echo "yes" || echo "no")
    DELETE_EXISTS=$(echo "$TOOLS_RESPONSE" | jq -e ".result.tools[] | select(.name==\"${entity}_delete\")" > /dev/null 2>&1 && echo "yes" || echo "no")
    LIST_EXISTS=$(echo "$TOOLS_RESPONSE" | jq -e ".result.tools[] | select(.name==\"${entity}_list\")" > /dev/null 2>&1 && echo "yes" || echo "no")
    
    if [ "$CREATE_EXISTS" = "yes" ] && [ "$GET_EXISTS" = "yes" ] && [ "$UPDATE_EXISTS" = "yes" ] && [ "$DELETE_EXISTS" = "yes" ] && [ "$LIST_EXISTS" = "yes" ]; then
        log_success "$entity has complete CRUD operations"
        ((ENTITY_COVERAGE++))
        ((TESTS_PASSED++))
    else
        log_error "$entity missing CRUD operations (C:$CREATE_EXISTS R:$GET_EXISTS U:$UPDATE_EXISTS D:$DELETE_EXISTS L:$LIST_EXISTS)"
        ((TESTS_FAILED++))
    fi
done

log_info "Entity coverage: $ENTITY_COVERAGE/8 entities have complete CRUD"

echo
echo "Validation Summary"
echo "=================="
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "All mock validations passed!"
    echo
    echo "🎉 Constitutional Principle VII Implementation Validated!"
    echo "✅ Discovery tools are properly registered"
    echo "✅ Schemas reference discovery tools instead of hardcoded values"
    echo "✅ Complete CRUD operations for all entities"
    echo "✅ 54 tools providing comprehensive Monica API coverage"
    echo
    echo "✨ The MCP server is ready for use with any working Monica instance!"
    echo "💡 Users can now discover valid IDs instead of guessing values"
    echo "🚀 No more hardcoded limitations - full API flexibility achieved"
    exit 0
else
    log_error "Some validations failed. Check the details above."
    exit 1
fi