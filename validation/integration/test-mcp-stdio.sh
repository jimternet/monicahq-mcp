#!/bin/bash
source ../setup/load-env.sh
set -e

# Simple test to verify MCP server STDIO communication
set -e

echo "🧪 Testing MCP Server STDIO Communication"
echo "========================================="

# Test using echo and pipe directly to MCP server
echo "Testing tools/list request..."

RESPONSE=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
    MONICA_API_URL="http://localhost:8081/api" \
    MONICA_API_TOKEN="$MONICA_API_TOKEN" \
    java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null)

echo "Response received:"
echo "$RESPONSE"

if echo "$RESPONSE" | jq -e '.result.tools' > /dev/null 2>&1; then
    TOOL_COUNT=$(echo "$RESPONSE" | jq -r '.result.tools | length')
    echo "✅ SUCCESS: Found $TOOL_COUNT tools"
    
    # Test gender_list discovery tool
    echo
    echo "Testing gender_list discovery tool..."
    GENDER_RESPONSE=$(echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gender_list","arguments":{}},"id":1}' | \
        MONICA_API_URL="http://localhost:8081/api" \
        MONICA_API_TOKEN="$MONICA_API_TOKEN" \
        java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null)
    
    echo "Gender discovery response:"
    echo "$GENDER_RESPONSE"
    
    if echo "$GENDER_RESPONSE" | jq -e '.result.content[0].text' > /dev/null 2>&1; then
        echo "✅ SUCCESS: Gender discovery tool works!"
        
        # Extract and show gender IDs
        GENDER_DATA=$(echo "$GENDER_RESPONSE" | jq -r '.result.content[0].text')
        echo "Discovered genders:"
        echo "$GENDER_DATA" | head -5
        
        echo
        echo "🎉 STDIO COMMUNICATION WORKING!"
        echo "✅ MCP Server responds to JSON-RPC requests"
        echo "✅ Discovery tools working via STDIO"
        echo "✅ Ready for full CRUD validation"
    else
        echo "❌ FAILED: Gender discovery tool failed"
        echo "Response: $GENDER_RESPONSE"
    fi
else
    echo "❌ FAILED: No tools found in response"
    echo "Response: $RESPONSE"
fi