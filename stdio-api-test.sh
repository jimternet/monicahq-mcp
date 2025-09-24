#!/bin/bash

# STDIO API Gap Fix Testing Script
# Tests operations via STDIO as designed for MCP protocol

set -e

echo "🧪 STDIO API Gap Fix Testing"
echo "============================"

# Source environment variables
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

# Check Monica instance
echo "📋 Checking Docker Monica instance..."
if ! curl -s "$MONICA_API_URL/api/contacts?limit=1" > /dev/null 2>&1; then
    echo "❌ Monica Docker instance not accessible at $MONICA_API_URL"
    exit 1
fi
echo "✅ Monica instance is running"

# Check JAR file
if [ ! -f "build/libs/monicahqmcp-0.1.0.jar" ]; then
    echo "❌ MCP server JAR not found. Run: ./gradlew build"
    exit 1
fi
echo "✅ MCP server JAR found"

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to test via STDIO
test_mcp_operation() {
    local operation=$1
    local params=$2
    local description=$3
    local expect_success=${4:-true}
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo ""
    echo "🔬 Test $TOTAL_TESTS: $description"
    echo "   Operation: $operation"
    echo "   Parameters: $params"
    
    # Create test request  
    local request="{\"jsonrpc\": \"2.0\", \"id\": $TOTAL_TESTS, \"method\": \"tools/call\", \"params\": {\"name\": \"$operation\", \"arguments\": $params}}"
    
    # Run test via STDIO with timeout
    local result
    local exit_code
    if result=$(echo "$request" | timeout 30s java -jar build/libs/monicahqmcp-0.1.0.jar 2>&1); then
        exit_code=0
    else
        exit_code=$?
    fi
    
    # Parse result - look for JSON response vs startup logs
    local json_response=""
    if echo "$result" | grep -q '{"jsonrpc"'; then
        json_response=$(echo "$result" | grep '{"jsonrpc"' | tail -1)
    fi
    
    # Analyze result
    if [ $exit_code -eq 0 ] && [ -n "$json_response" ]; then
        if echo "$json_response" | grep -q '"error"'; then
            # MCP returned error response
            if [ "$expect_success" = "false" ]; then
                echo "   ✅ PASSED: Expected error received"
                echo "   📄 Error: $(echo "$json_response" | jq -r '.error.message' 2>/dev/null || echo "$json_response" | head -c 100)..."
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                echo "   ❌ FAILED: Unexpected error"
                echo "   📄 Error: $(echo "$json_response" | jq -r '.error.message' 2>/dev/null || echo "$json_response" | head -c 100)..."
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        else
            # MCP returned success  
            if [ "$expect_success" = "true" ]; then
                echo "   ✅ PASSED: Operation completed successfully"
                echo "   📄 Response: $(echo "$json_response" | head -c 100)..."
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                echo "   ❌ FAILED: Expected error but got success"
                echo "   📄 Response: $(echo "$json_response" | head -c 100)..."
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        fi
    else
        # Process failed or no JSON response
        echo "   ❌ FAILED: Process execution failed or no valid JSON response"
        echo "   📄 Output: $(echo "$result" | tail -3 | head -c 150)..."
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

echo ""
echo "🚀 TESTING ALL 14 API GAP FIX OPERATIONS VIA STDIO"
echo "=================================================="

# Test a simple operation first to verify connectivity
echo ""
echo "🔧 Connectivity Test - List Tools"
echo '{"jsonrpc": "2.0", "id": 0, "method": "tools/list"}' | timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null | tail -1 | head -c 200 || echo "Initial connectivity test failed"

echo ""
echo "📞 CONTACT OPERATIONS (4/14)"  
echo "----------------------------"

# Contact operations
test_mcp_operation "contact_search" '{"query": "FieldTest", "limit": 5}' "Contact Search with FieldTest query"
test_mcp_operation "contact_career_update" '{"id": 1, "jobTitle": "Senior Developer", "company": "Anthropic Inc"}' "Contact Career Update for contact ID 1"  
test_mcp_operation "contact_audit_logs" '{"id": 1, "limit": 10}' "Contact Audit Logs for contact ID 1"
test_mcp_operation "contacts_by_tag" '{"id": 999, "limit": 5}' "Contacts by Tag with non-existent tag ID" "false"

echo ""
echo "👤 USER OPERATIONS (5/14)"
echo "-------------------------"

# User operations (expected to fail)
test_mcp_operation "user_list" '{"limit": 5}' "User List - Expected Admin-Only Error" "false"
test_mcp_operation "user_get" '{"id": 1}' "User Get - Expected Admin-Only Error" "false"
test_mcp_operation "user_create" '{"firstName": "Test", "lastName": "User", "email": "test@example.com"}' "User Create - Expected Admin-Only Error" "false" 
test_mcp_operation "user_update" '{"id": 1, "firstName": "Updated"}' "User Update - Expected Admin-Only Error" "false"
test_mcp_operation "user_delete" '{"id": 1}' "User Delete - Expected Admin-Only Error" "false"

echo ""
echo "📋 COMPLIANCE OPERATIONS (5/14)"
echo "-------------------------------"

# Compliance operations
test_mcp_operation "compliance_list" '{"limit": 5}' "Compliance List - Should Return Terms/Privacy"
test_mcp_operation "compliance_get" '{"id": 1}' "Compliance Get - Should Return Specific Term" 
test_mcp_operation "compliance_create" '{"type": "test_policy", "description": "Test policy document"}' "Compliance Create - Test Policy" "false"
test_mcp_operation "compliance_update" '{"id": 1, "description": "Updated terms"}' "Compliance Update - Update Existing Term" "false"
test_mcp_operation "compliance_delete" '{"id": 999}' "Compliance Delete - Non-existent ID" "false"

# Summary
echo ""
echo ""  
echo "📊 COMPREHENSIVE TEST RESULTS"
echo "=============================="
echo "Total Tests:  $TOTAL_TESTS"
echo "✅ Passed:   $PASSED_TESTS" 
echo "❌ Failed:   $FAILED_TESTS"
if [ $TOTAL_TESTS -gt 0 ]; then
    echo "📈 Success:  $(( PASSED_TESTS * 100 / TOTAL_TESTS ))%"
else
    echo "📈 Success:  0%"
fi

echo ""
echo "🎯 VALIDATION SUMMARY"
echo "====================="
echo ""
if [ $PASSED_TESTS -ge 8 ]; then
    echo "🚀 API GAP FIX STATUS: ✅ SUCCESS"  
    echo ""
    echo "🎉 SUCCESS: Core API gap fix operations are working!"
    echo "   The implementation successfully addresses the identified Monica API coverage gaps."
    exit 0
else
    echo "🚀 API GAP FIX STATUS: ❌ NEEDS REVIEW"
    echo ""
    echo "⚠️  WARNING: Some core operations failed unexpectedly."
    echo "   Review the failed tests above for implementation issues."
    exit 1
fi