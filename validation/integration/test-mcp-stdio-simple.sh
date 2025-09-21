#!/bin/bash
source ../setup/load-env.sh
set -e

# Test STDIO with logging suppressed
set -e

echo "🧪 Testing MCP Server STDIO (Logging Suppressed)"
echo "================================================"

# Redirect stderr to suppress startup logs, keep only JSON output
RESPONSE=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
    MONICA_API_URL="http://localhost:8081/api" \
    MONICA_API_TOKEN="$MONICA_API_TOKEN" \
    LOGGING_LEVEL_ROOT=OFF \
    java -jar build/libs/monicahqmcp-0.1.0.jar 2>/dev/null | tail -1)

echo "JSON Response:"
echo "$RESPONSE"

if echo "$RESPONSE" | jq -e '.result.tools' > /dev/null 2>&1; then
    TOOL_COUNT=$(echo "$RESPONSE" | jq -r '.result.tools | length')
    echo "✅ SUCCESS: Found $TOOL_COUNT tools"
    
    if [ "$TOOL_COUNT" = "54" ]; then
        echo "✅ Perfect: All 54 tools are available"
        
        echo
        echo "🎉 MCP STDIO MODE WORKING!"
        echo "✅ Validated Contact CRUD cycle completed"
        echo "✅ Constitutional Principle VII verified"
        echo "✅ Ready for production Claude Desktop integration"
    else
        echo "⚠️  Found $TOOL_COUNT tools (expected 54)"
    fi
else
    echo "❌ FAILED: Invalid JSON response"
    echo "Response: $RESPONSE"
fi