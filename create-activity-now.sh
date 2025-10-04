#!/bin/bash

# Actually Create Activity with Contact1758422058
# This script will send the complete MCP session and capture the JSON response

set -euo pipefail

export MONICA_API_URL="http://localhost:8081/api"
export MONICA_API_TOKEN="eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiIxIiwianRpIjoiMTAzZGJiZTcxNzZhM2MxNTg3YjY2OWEyZDhjZjNiN2Q2Y2Q4YjViYmJhZjE1ZjU1ZDY2ZDZiNzFiMzJiODhhNGI3ODdlY2IyNjhhZDQ4OGMiLCJpYXQiOjE3NTk1MTgzODAuNzAyNDkzLCJuYmYiOjE3NTk1MTgzODAuNzAyNDk0LCJleHAiOjE3OTEwNTQzODAuNjk4Mjc0LCJzdWIiOiIzIiwic2NvcGVzIjpbXX0.iocEq7y3bCTbRKw33AG08DP99_Ypni7hHrpZUhTingD3XIEWseKWXlSz0V06hR3Q3cC4BzJlpi-ri08YycNt_VyIs6cbyaLE55VTQ3khixGaBZzZZAL26TuVo6Nr7I-sT_A9Qym6-NsDF4xZwENkzQu0sw-hn7JtgAMsrnnWOANvI37WMQzu0fPithI0UL2omf3uAbLkutJlcI_Li7oIL1Hknz9bAETaNUwxXRtwmFtiHs8oRVE_7rclIwPz5YZOBdmORx0VzlWdSigd9Sh8hXXgxJQM1HoFpCLxkGl5f3ZAmMphEQj0eVo2WTpuPj2V5gTK94_6bsrF5IekQS42tWMmzIhOolnxOGkh644CsoNrzd_DgFvRJuNbHTrg7ifAJZ3PFoiqzi0QYrnNuVvv_bS7tpRmEbCKT3LkJru1AFQAa_evpyDUHWiwEYTpnpSXwvOvWnmqZPKfln2IKbsa_KAR5nnihOJ1_YOuWufJPj5K08OoBeTQ-Z7QYJwg0eHpd9RpVz8s3Zndoq36nCq15hxWvv-MV1WIzedcfF4sWlbgbW3Yf4GrRzoKOz9m5OnCOJRhW-NrbZ2BsxpPZOxUUtOzGdB8xocgIccmACFNL2Ccz_0r3HoIVbOMHMK6AmyrQLMtb6HWnnCdpPMmc0VLo9ZJ_wdz0DtJFu_A8dtbpgE"

echo "🎯 CREATING ACTIVITY WITH CONTACT1758422058"
echo "=========================================="
echo ""

# Create input file with proper MCP session
cat > /tmp/mcp_create_activity.input << 'EOF'
{"jsonrpc":"2.0","id":"init","method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"activity-creator","version":"1.0.0"}}}
{"jsonrpc":"2.0","id":"create-activity","method":"tools/call","params":{"name":"monicahq:activity_create","arguments":{"summary":"Meeting with Contact1758422058 - MCP Test","description":"This activity was created via MCP server to demonstrate the working integration with Contact1758422058. The attendees parameter uses the fixed format that handles both array and object types.","happened_at":"2025-10-03","attendees":[{"contactId":"1758422058"}]}}}
EOF

echo "📡 Starting MCP server and sending requests..."

# Start MCP server in background and pipe input
java -jar build/libs/monicahqmcp-0.1.0.jar < /tmp/mcp_create_activity.input > /tmp/mcp_full_output.log 2>&1 &
MCP_PID=$!

# Wait for processing
echo "⏳ Waiting for MCP server to process requests..."
sleep 15

# Kill the server
kill $MCP_PID 2>/dev/null || true
wait $MCP_PID 2>/dev/null || true

echo ""
echo "📋 EXTRACTING JSON RESPONSES"
echo "============================"

# Extract only JSON lines (lines starting with {)
echo "🔍 JSON Responses from MCP Server:"
echo "-----------------------------------"
grep '^{' /tmp/mcp_full_output.log > /tmp/mcp_json_only.log 2>/dev/null || echo "No JSON responses found"

if [ -s /tmp/mcp_json_only.log ]; then
    echo ""
    echo "✅ Found JSON responses:"
    cat /tmp/mcp_json_only.log | while read line; do
        echo "$line" | python3 -m json.tool 2>/dev/null || echo "$line"
        echo ""
    done
else
    echo "❌ No JSON responses extracted"
    echo ""
    echo "🔍 Full output (last 50 lines):"
    echo "--------------------------------"
    tail -50 /tmp/mcp_full_output.log
fi

echo ""
echo "📊 ANALYSIS"
echo "==========="

# Check for successful activity creation
if grep -q '"result".*"id"' /tmp/mcp_json_only.log 2>/dev/null; then
    ACTIVITY_ID=$(grep '"result".*"id"' /tmp/mcp_json_only.log | grep -o '"id":[0-9]*' | head -1 | grep -o '[0-9]*')
    echo "✅ SUCCESS: Activity created with ID $ACTIVITY_ID"
    echo "📝 Activity Details:"
    echo "   - Contact: 1758422058"
    echo "   - Summary: Meeting with Contact1758422058 - MCP Test"
    echo "   - Date: 2025-10-03"
    echo "   - Created via: MCP Server"
elif grep -q '"error"' /tmp/mcp_json_only.log 2>/dev/null; then
    echo "❌ ERROR: Activity creation failed"
    echo "🔍 Error details:"
    grep '"error"' /tmp/mcp_json_only.log
else
    echo "⚠️  UNKNOWN: Check output for status"
fi

echo ""
echo "📄 Files created:"
echo "  - Full output: /tmp/mcp_full_output.log"
echo "  - JSON only: /tmp/mcp_json_only.log"
echo "  - Input: /tmp/mcp_create_activity.input"

# Cleanup input file
rm -f /tmp/mcp_create_activity.input