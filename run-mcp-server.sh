#!/bin/bash

# MonicaHQ MCP Server Runner for Testing
# This script sets up environment variables and runs the MCP server in STDIO mode

# Check if environment variables are set
if [ -z "$MONICA_API_URL" ] || [ -z "$MONICA_API_TOKEN" ]; then
    echo "Error: Please set MONICA_API_URL and MONICA_API_TOKEN environment variables"
    echo "Example:"
    echo "export MONICA_API_URL=https://your-monica-instance.com/api"
    echo "export MONICA_API_TOKEN=your-oauth2-bearer-token"
    exit 1
fi

# Enable STDIO mode
export MCP_STDIO_MODE=true

echo "Starting MonicaHQ MCP Server in STDIO mode..."
echo "You can now connect with MCP Inspector or send JSON-RPC messages via stdin"
echo "Press Ctrl+C to stop the server"
echo ""
echo "Example test command:"
echo '  echo {"jsonrpc":"2.0","method":"tools/list","id":1} | ./run-mcp-server.sh'
echo ""

# Run the server
java -jar build/libs/monicahqmcp-0.1.0.jar --stdio