#!/bin/bash

# Test script for MCP operations
# This shows the correct parameter format for each operation

# Check if environment variables are set
if [ -z "$MONICA_API_URL" ] || [ -z "$MONICA_API_TOKEN" ]; then
    echo "Error: Please set MONICA_API_URL and MONICA_API_TOKEN environment variables"
    echo "Example:"
    echo "export MONICA_API_URL=https://your-monica-instance.com/api"
    echo "export MONICA_API_TOKEN=your-oauth2-bearer-token"
    exit 1
fi

JAR="build/libs/monicahqmcp-0.1.0.jar"

echo "Testing MCP Operations - Correct Parameter Formats"
echo "=================================================="
echo ""

echo "1. List contacts (no parameters needed):"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"contact_list","arguments":{}},"id":1}'
echo ""
echo "Response:"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"contact_list","arguments":{}},"id":1}' | java -jar $JAR --stdio 2>/dev/null | jq '.result.data | length'
echo ""

echo "2. Get specific contact (requires 'id' parameter):"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"contact_get","arguments":{"id":2939}},"id":2}'
echo ""

echo "3. Create contact (camelCase parameters required):"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"contact_create","arguments":{"firstName":"Test","lastName":"User","genderId":"3","isBirthdateKnown":false,"isDeceased":false,"isDeceasedDateKnown":false}},"id":3}'
echo ""

echo "4. Create note (requires contactId, not contact_id):"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"note_create","arguments":{"contactId":2939,"body":"Test note"}},"id":4}'
echo ""

echo "5. List contact fields (requires contactId):"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"contact_field_list","arguments":{"contactId":2939}},"id":5}'
echo ""

echo "Common parameter mistakes:"
echo "- Using snake_case instead of camelCase (contact_id vs contactId)"
echo "- Using string IDs instead of numbers (\"2939\" vs 2939)"
echo "- Missing required boolean fields for contact creation"
echo "- Using wrong parameter names (id vs contact_id vs contactId)"