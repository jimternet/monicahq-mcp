#!/bin/bash

# Interactive API Gap Fix Testing Script
# Tests ALL 14 new operations against live Docker Monica instance

set -e

echo "🧪 INTERACTIVE API Gap Fix Testing"
echo "=================================="

# Source environment variables from .env file
if [ -f ".env" ]; then
    source .env
    export MONICA_API_URL
    export MONICA_API_TOKEN
    export MCP_SERVER_PORT
    export MCP_DEBUG
    echo "✅ Environment loaded from .env file"
else
    echo "❌ .env file not found"
    exit 1
fi

# Check if Docker Monica is running
echo "📋 Checking Docker Monica instance..."
if ! curl -s "$MONICA_API_URL/api/contacts?limit=1" > /dev/null 2>&1; then
    echo "❌ Monica Docker instance not accessible at $MONICA_API_URL"
    exit 1
fi
echo "✅ Monica instance is running"

# Check if MCP server JAR exists
if [ ! -f "build/libs/monicahqmcp-0.1.0.jar" ]; then
    echo "❌ MCP server JAR not found. Run: ./gradlew build"
    exit 1
fi
echo "✅ MCP server JAR found"

echo ""
echo "🚀 Starting MCP server in background..."

# Start MCP server in background
java -jar build/libs/monicahqmcp-0.1.0.jar &
MCP_PID=$!
echo "📡 MCP server started with PID: $MCP_PID"

# Give server time to start
sleep 3

# Function to test MCP operation via echo/pipe
test_mcp_operation() {
    local operation=$1
    local params=$2  
    local description=$3
    local expect_success=${4:-true}
    
    echo ""
    echo "🔬 Testing: $description"
    echo "   Operation: $operation"
    echo "   Parameters: $params"
    
    # Create test request
    local request="{\"jsonrpc\": \"2.0\", \"id\": 1, \"method\": \"tools/call\", \"params\": {\"name\": \"$operation\", \"arguments\": $params}}"
    
    # Send request to server
    local result
    if result=$(echo "$request" | timeout 10s nc localhost 3000 2>/dev/null); then
        if echo "$result" | grep -q '"error"'; then
            if [ "$expect_success" = "false" ]; then
                echo "   ✅ PASSED: Expected error received"
                echo "   📄 Response: $(echo "$result" | head -c 150)..."
            else
                echo "   ❌ FAILED: Unexpected error"
                echo "   📄 Response: $(echo "$result" | head -c 200)..."
            fi
        else
            if [ "$expect_success" = "true" ]; then
                echo "   ✅ PASSED: Operation completed successfully"
                echo "   📄 Response: $(echo "$result" | head -c 150)..."
            else
                echo "   ❌ FAILED: Expected error but got success"
                echo "   📄 Response: $(echo "$result" | head -c 200)..."
            fi
        fi
    else
        echo "   ❌ FAILED: Connection failed or timeout"
    fi
}

# Cleanup function
cleanup() {
    echo ""
    echo "🧹 Cleaning up..."
    if kill -0 $MCP_PID 2>/dev/null; then
        kill $MCP_PID
        echo "✅ MCP server stopped"
    fi
}

# Set trap to cleanup on exit
trap cleanup EXIT

echo ""
echo "🔧 Testing simple contact list first..."
echo '{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}' | timeout 5s nc localhost 3000 2>/dev/null || echo "Connection test failed"

echo ""
echo "📞 TESTING CONTACT OPERATIONS (4/14)"
echo "======================================"

# Test 1: Contact Search
test_mcp_operation "contact_search" '{"query": "FieldTest", "limit": 5}' "Contact Search with FieldTest query"

# Test 2: Contact Career Update  
test_mcp_operation "contact_career_update" '{"id": 1, "jobTitle": "Senior Developer", "company": "Anthropic Inc"}' "Contact Career Update for contact ID 1"

# Test 3: Contact Audit Logs
test_mcp_operation "contact_audit_logs" '{"id": 1, "limit": 10}' "Contact Audit Logs for contact ID 1"

# Test 4: Contacts by Tag
test_mcp_operation "contacts_by_tag" '{"id": 999, "limit": 5}' "Contacts by Tag with non-existent tag ID" "false"

echo ""
echo "👤 TESTING USER OPERATIONS (5/14)"
echo "================================="

# Test 5-9: User operations (expected to fail)
test_mcp_operation "user_list" '{"limit": 5}' "User List - Expected Admin-Only Error" "false"
test_mcp_operation "user_get" '{"id": 1}' "User Get - Expected Admin-Only Error" "false" 
test_mcp_operation "user_create" '{"firstName": "Test", "lastName": "User", "email": "test@example.com"}' "User Create - Expected Admin-Only Error" "false"
test_mcp_operation "user_update" '{"id": 1, "firstName": "Updated"}' "User Update - Expected Admin-Only Error" "false"
test_mcp_operation "user_delete" '{"id": 1}' "User Delete - Expected Admin-Only Error" "false"

echo ""
echo "📋 TESTING COMPLIANCE OPERATIONS (5/14)" 
echo "======================================="

# Test 10-14: Compliance operations
test_mcp_operation "compliance_list" '{"limit": 5}' "Compliance List - Should Return Terms/Privacy"
test_mcp_operation "compliance_get" '{"id": 1}' "Compliance Get - Should Return Specific Term"
test_mcp_operation "compliance_create" '{"type": "test_policy", "description": "Test policy document"}' "Compliance Create - Test Policy" "false"
test_mcp_operation "compliance_update" '{"id": 1, "description": "Updated terms"}' "Compliance Update - Update Existing Term" "false" 
test_mcp_operation "compliance_delete" '{"id": 999}' "Compliance Delete - Non-existent ID" "false"

echo ""
echo "📊 INTERACTIVE TEST COMPLETE"
echo "============================"
echo "All 14 API gap fix operations have been tested against the live Docker Monica instance."
echo "Review the results above to validate the implementation."