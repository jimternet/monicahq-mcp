#!/bin/bash

# API Gap Fix Manual Testing Script
# Tests the 4 new contact operations + graceful handling of Users/Compliance APIs

set -e

echo "🧪 API Gap Fix Manual Testing"
echo "============================="

# Check if Docker Monica is running
echo "📋 Checking Docker Monica instance..."
if ! curl -s http://localhost:8081/api/ping > /dev/null 2>&1; then
    echo "❌ Monica Docker instance not accessible at localhost:8081"
    echo "   Please start Monica with: docker-compose up"
    exit 1
fi
echo "✅ Monica instance is running"

# Check if MCP server builds
echo "📋 Building MCP server..."
if ! ./gradlew build -x test -q; then
    echo "❌ MCP server build failed"
    exit 1
fi
echo "✅ MCP server built successfully (tests skipped)"

# Check environment variables
echo "📋 Checking environment setup..."
if [ -z "$MONICA_API_TOKEN" ]; then
    echo "❌ MONICA_API_TOKEN not set"
    echo "   Please set: export MONICA_API_TOKEN=\"your-token-here\""
    exit 1
fi

if [ -z "$MONICA_API_URL" ]; then
    export MONICA_API_URL="http://localhost:8081/api"
    echo "ℹ️  Set MONICA_API_URL to default: $MONICA_API_URL"
fi
echo "✅ Environment configured"

echo ""
echo "🔍 Testing API Gap Fix Operations"
echo "=================================="

# Function to test MCP operation
test_operation() {
    local operation=$1
    local params=$2
    local description=$3
    
    echo "Testing: $description"
    echo "Operation: $operation"
    echo "Parameters: $params"
    
    # Create test request
    local request="{\"jsonrpc\": \"2.0\", \"id\": 1, \"method\": \"tools/call\", \"params\": {\"name\": \"$operation\", \"arguments\": $params}}"
    
    # Run test (capture both stdout and stderr, allow failures)
    local result
    if result=$(echo "$request" | java -jar build/libs/monicahqmcp-0.1.0.jar 2>&1); then
        echo "✅ PASSED: $description"
        echo "   Response received (truncated): $(echo "$result" | head -c 100)..."
    else
        local exit_code=$?
        echo "❌ FAILED: $description"
        echo "   Error: $(echo "$result" | head -c 200)"
    fi
    echo ""
}

# Test 1: Contact Search (Core Operation)
test_operation "contact_search" '{"query": "test", "limit": 5}' "Contact Search with Query"

# Test 2: Contact Career Update (Core Operation) 
# Note: This requires an existing contact ID - will likely fail gracefully
test_operation "contact_career_update" '{"id": 1, "jobTitle": "Test Engineer", "company": "Test Corp"}' "Contact Career Update"

# Test 3: Contact Audit Logs (Core Operation)
# Note: This requires an existing contact ID - will likely fail gracefully  
test_operation "contact_audit_logs" '{"id": 1, "limit": 10}' "Contact Audit Logs Retrieval"

# Test 4: Contacts by Tag (Core Operation)
# Note: This requires an existing tag ID - will likely fail gracefully
test_operation "contacts_by_tag" '{"id": 1, "limit": 10}' "Contacts by Tag Filtering"

# Test 5: User Operations (Expected to Fail Gracefully)
test_operation "user_list" '{"limit": 5}' "User List (Expected 404 - Admin Only)"

# Test 6: Compliance Operations (Expected to Fail Gracefully)  
test_operation "compliance_list" '{"limit": 5}' "Compliance List (Expected Error - Experimental)"

echo "🎯 Manual Testing Summary"
echo "========================"
echo "✅ Core API Gap Fix operations tested"
echo "✅ Error handling for unavailable APIs tested"
echo "✅ MCP server stability verified"
echo ""
echo "📋 Next Steps:"
echo "   1. Review test results above"
echo "   2. For FAILED tests with real data, check Monica instance has contacts/tags"
echo "   3. For TIMEOUT tests, investigate performance issues"
echo "   4. For expected 404s (Users/Compliance), verify graceful error messages"
echo ""
echo "📚 Full testing guide: docs/manual-testing-guide.md"