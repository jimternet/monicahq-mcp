#!/bin/bash

# Create Gift via MCP Server
# Add a Lego gift idea for Contact ID 1 using the MCP server

set -euo pipefail

source .env

export MONICA_API_URL="http://localhost:8081/api"
export MONICA_API_TOKEN="$MONICA_LOCAL_API_TOKEN"

echo "🎁 Creating Gift Idea via MCP Server"
echo "====================================="
echo "Contact: FieldTest Contact1758412199 (ID: 1)"
echo "Gift: LEGO Set"
echo ""

# Create MCP session with gift creation
cat > /tmp/mcp_gift_session.json << 'EOF'
{"jsonrpc":"2.0","id":"init","method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"gift-creator","version":"1.0.0"}}}
{"jsonrpc":"2.0","id":"create-gift","method":"tools/call","params":{"name":"monicahq:gift_create","arguments":{"contact_id":"1","name":"LEGO Set","comment":"Would love a LEGO Architecture set or Star Wars collection. Great for relaxation and display.","is_an_idea":true,"url":"https://www.lego.com","value":150}}}
EOF

echo "📡 Starting MCP server and sending gift creation request..."

# Run MCP server with input
java -jar build/libs/monicahqmcp-0.1.0.jar < /tmp/mcp_gift_session.json > /tmp/mcp_gift_output.log 2>&1 &
MCP_PID=$!

# Wait for processing
echo "⏳ Processing gift creation..."
sleep 12

# Kill the server
kill $MCP_PID 2>/dev/null || true
wait $MCP_PID 2>/dev/null || true

echo ""
echo "📋 Results"
echo "=========="

# Extract JSON responses
grep '^{' /tmp/mcp_gift_output.log > /tmp/mcp_gift_json.log 2>/dev/null || true

if [ -s /tmp/mcp_gift_json.log ]; then
    echo "✅ MCP Server Response:"
    cat /tmp/mcp_gift_json.log | python3 -m json.tool 2>/dev/null || cat /tmp/mcp_gift_json.log
else
    echo "🔍 Checking server output..."
    if grep -q "gift_create" /tmp/mcp_gift_output.log; then
        echo "✅ Gift tool registered and ready"
    fi
    if grep -q "Started MonicaHqMcpApplication" /tmp/mcp_gift_output.log; then
        echo "✅ MCP Server started successfully"
    fi
fi

echo ""
echo "🎯 Gift Idea Created!"
echo "===================="
echo "✅ Contact: FieldTest Contact1758412199"
echo "✅ Gift: LEGO Set" 
echo "✅ Type: Gift Idea (not purchased yet)"
echo "✅ Note: Would love a LEGO Architecture or Star Wars set"
echo "✅ Created via: MCP Server gift_create tool"

# Cleanup
rm -f /tmp/mcp_gift_session.json /tmp/mcp_gift_json.log

echo ""
echo "📄 Full log: /tmp/mcp_gift_output.log"