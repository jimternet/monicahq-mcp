#!/bin/bash

# Direct API Gap Fix Testing Script
# Tests operations via direct STDIO input

set -e

echo "🧪 DIRECT API Gap Fix Testing"
echo "=============================="

# Source environment variables
if [ -f ".env" ]; then
    source .env
    export MONICA_API_URL
    export MONICA_API_TOKEN
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

echo ""
echo "🔧 Testing MCP server initialization..."
echo '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05", "capabilities": {}, "clientInfo": {"name": "test-client", "version": "1.0.0"}}}' | java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null | grep -o '{"jsonrpc.*}' | head -1

echo ""  
echo "🔧 Testing tools list..."
echo '{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}' | java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null | grep -o '{"jsonrpc.*}' | head -1

echo ""
echo "🚀 TESTING API GAP FIX OPERATIONS"
echo "================================="

# Function to test single operation 
test_operation() {
    local operation=$1
    local params=$2
    local description=$3
    
    echo ""
    echo "🔬 Testing: $description"
    echo "   Operation: $operation"
    
    # Create request
    local request="{\"jsonrpc\": \"2.0\", \"id\": 3, \"method\": \"tools/call\", \"params\": {\"name\": \"$operation\", \"arguments\": $params}}"
    
    # Execute and capture JSON response
    local result
    if result=$(echo "$request" | java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null | grep -o '{"jsonrpc.*}' | head -1); then
        if [ -n "$result" ]; then
            if echo "$result" | grep -q '"error"'; then
                echo "   ❌ ERROR: $(echo "$result" | grep -o '"message":"[^"]*"' | cut -d'"' -f4)"
            elif echo "$result" | grep -q '"result"'; then
                echo "   ✅ SUCCESS: Operation completed"
            else
                echo "   ❓ UNKNOWN: $(echo "$result" | head -c 100)"
            fi
        else
            echo "   ❌ FAILED: No JSON response received"
        fi
    else
        echo "   ❌ FAILED: Process execution failed"
    fi
}

# Test key operations from each category
echo ""
echo "📞 CONTACT OPERATIONS"
echo "--------------------"
test_operation "contact_search" '{"query": "John", "limit": 5}' "Contact Search"
test_operation "contact_career_update" '{"id": 1, "jobTitle": "Developer"}' "Contact Career Update"

echo ""
echo "👤 USER OPERATIONS (Expected to fail - admin only)"
echo "------------------------------------------------"
test_operation "user_list" '{"limit": 5}' "User List"
test_operation "user_get" '{"id": 1}' "User Get"

echo ""
echo "📋 COMPLIANCE OPERATIONS"
echo "------------------------"
test_operation "compliance_list" '{"limit": 5}' "Compliance List"
test_operation "compliance_get" '{"id": 1}' "Compliance Get"

echo ""
echo "📊 DIRECT TESTING COMPLETE"
echo "=========================="
echo "Basic validation of API gap fix operations completed."
echo "Review results above for operation status."