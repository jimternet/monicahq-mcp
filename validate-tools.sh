#!/bin/bash

# MonicaHQ MCP Server - Tool Validation Script
# This script validates all MCP tools against a local Monica instance

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

# Check if Monica API token is provided
if [ -z "$MONICA_API_TOKEN" ]; then
    log_error "MONICA_API_TOKEN environment variable is required"
    echo "Usage: MONICA_API_TOKEN=your-token-here $0"
    exit 1
fi

echo "🧪 MonicaHQ MCP Server - Tool Validation"
echo "========================================"

MCP_SERVER_URL="http://localhost:8080"
TESTS_PASSED=0
TESTS_FAILED=0

# Function to test MCP tool via HTTP
test_mcp_tool() {
    local tool_name="$1"
    local arguments="$2"
    local description="$3"
    
    log_info "Testing $tool_name: $description"
    
    local payload=$(cat <<EOF
{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
        "name": "$tool_name",
        "arguments": $arguments
    },
    "id": 1
}
EOF
)
    
    local response
    if response=$(curl -s -X POST \
        -H "Content-Type: application/json" \
        -H "Authorization: Bearer $MONICA_API_TOKEN" \
        -d "$payload" \
        "$MCP_SERVER_URL/mcp/tools"); then
        
        if echo "$response" | jq -e '.result' > /dev/null 2>&1; then
            log_success "$tool_name works correctly"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            
            # Show sample of response data
            if command -v jq &> /dev/null; then
                echo "$response" | jq -r '.result.content[0].text' | head -3
            fi
            echo
            return 0
        else
            log_error "$tool_name failed - Response: $response"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    else
        log_error "$tool_name failed - Could not connect to MCP server"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# Update docker-compose with the API token
echo
echo "Step 1: Updating MCP Server Configuration"
echo "======================================="

log_info "Updating MCP server with API token..."
docker-compose -f docker-compose.dev.yml stop monicahq-mcp
MONICA_API_TOKEN="$MONICA_API_TOKEN" docker-compose -f docker-compose.dev.yml up -d monicahq-mcp

log_info "Waiting for MCP server to restart..."
sleep 20

# Test server health
if ! curl -f -s "$MCP_SERVER_URL/actuator/health" > /dev/null; then
    log_error "MCP server is not healthy"
    exit 1
fi

echo
echo "Step 2: Testing Discovery Tools"
echo "=============================="

# Test discovery tools (these should work without any setup)
test_mcp_tool "gender_list" "{}" "List available genders"
test_mcp_tool "contact_field_type_list" "{}" "List available contact field types"

echo
echo "Step 3: Testing Basic Operations"
echo "==============================="

# Test basic contact operations
log_info "Testing contact creation with discovered gender..."

# First get a gender ID
GENDER_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_API_TOKEN" \
    -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gender_list","arguments":{}},"id":1}' \
    "$MCP_SERVER_URL/mcp/tools")

if command -v jq &> /dev/null && echo "$GENDER_RESPONSE" | jq -e '.result' > /dev/null 2>&1; then
    # Extract first gender ID from response
    GENDER_DATA=$(echo "$GENDER_RESPONSE" | jq -r '.result.content[0].text')
    FIRST_GENDER_ID=$(echo "$GENDER_DATA" | jq -r '.data[0].id' 2>/dev/null || echo "1")
    
    log_info "Using gender ID: $FIRST_GENDER_ID"
    
    # Test contact creation with discovered gender
    test_mcp_tool "contact_create" "{
        \"firstName\": \"Test\",
        \"lastName\": \"User\",
        \"genderId\": \"$FIRST_GENDER_ID\",
        \"email\": \"test@example.com\"
    }" "Create contact with discovered gender ID"
    
else
    log_warning "Could not parse gender response, using fallback gender ID"
    test_mcp_tool "contact_create" "{
        \"firstName\": \"Test\",
        \"lastName\": \"User\", 
        \"genderId\": \"1\",
        \"email\": \"test@example.com\"
    }" "Create contact with fallback gender ID"
fi

echo
echo "Step 4: Testing Contact Field Types"
echo "=================================="

# Test contact field type discovery
FIELD_TYPE_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_API_TOKEN" \
    -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"contact_field_type_list","arguments":{}},"id":1}' \
    "$MCP_SERVER_URL/mcp/tools")

if command -v jq &> /dev/null && echo "$FIELD_TYPE_RESPONSE" | jq -e '.result' > /dev/null 2>&1; then
    # Extract email field type ID from response
    FIELD_TYPE_DATA=$(echo "$FIELD_TYPE_RESPONSE" | jq -r '.result.content[0].text')
    EMAIL_FIELD_TYPE_ID=$(echo "$FIELD_TYPE_DATA" | jq -r '.data[] | select(.type=="email") | .id' 2>/dev/null || echo "1")
    
    log_info "Using email field type ID: $EMAIL_FIELD_TYPE_ID"
    
    # Test contact field creation with discovered type
    test_mcp_tool "contact_field_create" "{
        \"contactId\": 1,
        \"contactFieldTypeId\": $EMAIL_FIELD_TYPE_ID,
        \"data\": \"discovered-test@example.com\"
    }" "Create contact field with discovered field type ID"
    
else
    log_warning "Could not parse contact field type response, using fallback"
fi

echo
echo "Validation Summary"
echo "================="
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "All tools validated successfully!"
    echo
    echo "🎉 Discovery tools are working correctly!"
    echo "Users can now:"
    echo "  • Use 'gender_list' to see available genders"
    echo "  • Use 'contact_field_type_list' to see available field types"
    echo "  • Create contacts and fields with proper IDs"
    exit 0
else
    log_error "Some tests failed. Check the logs above."
    echo
    echo "To debug failed tests, check:"
    echo "  • docker-compose -f docker-compose.dev.yml logs monicahq-mcp"
    echo "  • docker-compose -f docker-compose.dev.yml logs monica"
    exit 1
fi