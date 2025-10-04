#!/bin/bash

# Direct Activity Creation Test
# Send actual JSON-RPC requests to MCP server and capture responses

set -euo pipefail

export MONICA_API_URL="http://localhost:8081/api"
export MONICA_API_TOKEN="eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiIxIiwianRpIjoiMTAzZGJiZTcxNzZhM2MxNTg3YjY2OWEyZDhjZjNiN2Q2Y2Q4YjViYmJhZjE1ZjU1ZDY2ZDZiNzFiMzJiODhhNGI3ODdlY2IyNjhhZDQ4OGMiLCJpYXQiOjE3NTk1MTgzODAuNzAyNDkzLCJuYmYiOjE3NTk1MTgzODAuNzAyNDk0LCJleHAiOjE3OTEwNTQzODAuNjk4Mjc0LCJzdWIiOiIzIiwic2NvcGVzIjpbXX0.iocEq7y3bCTbRKw33AG08DP99_Ypni7hHrpZUhTingD3XIEWseKWXlSz0V06hR3Q3cC4BzJlpi-ri08YycNt_VyIs6cbyaLE55VTQ3khixGaBZzZZAL26TuVo6Nr7I-sT_A9Qym6-NsDF4xZwENkzQu0sw-hn7JtgAMsrnnWOANvI37WMQzu0fPithI0UL2omf3uAbLkutJlcI_Li7oIL1Hknz9bAETaNUwxXRtwmFtiHs8oRVE_7rclIwPz5YZOBdmORx0VzlWdSigd9Sh8hXXgxJQM1HoFpCLxkGl5f3ZAmMphEQj0eVo2WTpuPj2V5gTK94_6bsrF5IekQS42tWMmzIhOolnxOGkh644CsoNrzd_DgFvRJuNbHTrg7ifAJZ3PFoiqzi0QYrnNuVvv_bS7tpRmEbCKT3LkJru1AFQAa_evpyDUHWiwEYTpnpSXwvOvWnmqZPKfln2IKbsa_KAR5nnihOJ1_YOuWufJPj5K08OoBeTQ-Z7QYJwg0eHpd9RpVz8s3Zndoq36nCq15hxWvv-MV1WIzedcfF4sWlbgbW3Yf4GrRzoKOz9m5OnCOJRhW-NrbZ2BsxpPZOxUUtOzGdB8xocgIccmACFNL2Ccz_0r3HoIVbOMHMK6AmyrQLMtb6HWnnCdpPMmc0VLo9ZJ_wdz0DtJFu_A8dtbpgE"

echo "🎯 Direct MCP Activity Creation Test"
echo "==================================="
echo ""

# Create the complete MCP session
cat > /tmp/mcp_session.json << 'EOF'
{"jsonrpc":"2.0","id":"init","method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"direct-test","version":"1.0.0"}}}
{"jsonrpc":"2.0","id":"activity-test","method":"tools/call","params":{"name":"monicahq:activity_create","arguments":{"summary":"Direct Test Meeting with Contact1758422058","description":"This activity was created directly via MCP server to test Contact1758422058 integration. Testing the complete flow from JSON-RPC request to Monica API response.","happened_at":"2025-10-03","attendees":[{"contactId":"1758422058"}]}}}
EOF

echo "📡 Sending MCP requests to server..."
echo ""

# Run MCP server with input and capture ALL output
java -jar build/libs/monicahqmcp-0.1.0.jar < /tmp/mcp_session.json > /tmp/mcp_output.json 2> /tmp/mcp_stderr.log &

MCP_PID=$!

# Wait for processing
sleep 10

# Kill the server
kill $MCP_PID 2>/dev/null || true
wait $MCP_PID 2>/dev/null || true

echo "📋 MCP Server Response Analysis"
echo "==============================="
echo ""

# Show the actual JSON responses
echo "🔍 JSON Responses from MCP Server:"
echo "-----------------------------------"
if [ -s /tmp/mcp_output.json ]; then
    cat /tmp/mcp_output.json
    echo ""
else
    echo "No JSON output received"
fi

echo ""
echo "📊 Response Analysis:"
echo "--------------------"

# Check initialize response
if grep -q '"result".*"protocolVersion"' /tmp/mcp_output.json 2>/dev/null; then
    echo "✅ Initialize: SUCCESS"
else
    echo "❌ Initialize: FAILED"
fi

# Check activity creation response
if grep -q '"id".*"activity-test"' /tmp/mcp_output.json 2>/dev/null; then
    echo "✅ Activity Request: PROCESSED"
    
    if grep -q '"result".*"id"' /tmp/mcp_output.json 2>/dev/null; then
        echo "✅ Activity Creation: SUCCESS"
        
        # Extract activity ID if present
        ACTIVITY_ID=$(grep -o '"id":[0-9]*' /tmp/mcp_output.json | head -1 | grep -o '[0-9]*' || echo "unknown")
        echo "📝 Created Activity ID: $ACTIVITY_ID"
        
    elif grep -q '"error"' /tmp/mcp_output.json 2>/dev/null; then
        echo "❌ Activity Creation: ERROR"
        echo "🔍 Error details:"
        grep -A 3 '"error"' /tmp/mcp_output.json 2>/dev/null || echo "   Check full output above"
    else
        echo "⚠️  Activity Creation: UNKNOWN STATUS"
    fi
else
    echo "❌ Activity Request: NOT PROCESSED"
fi

echo ""
echo "🔧 Debug Information:"
echo "---------------------"
echo "Server stderr log:"
if [ -s /tmp/mcp_stderr.log ]; then
    tail -20 /tmp/mcp_stderr.log
else
    echo "No stderr output"
fi

echo ""
echo "📄 Complete Results Available:"
echo "  JSON Output: /tmp/mcp_output.json"
echo "  Server Log: /tmp/mcp_stderr.log"

# Cleanup session file
rm -f /tmp/mcp_session.json