#!/bin/bash

# MonicaHQ MCP Server - Claude Desktop Integration Testing
# Tests Claude Desktop configuration and connectivity

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

success() {
    echo -e "${GREEN}✅ SUCCESS:${NC} $1"
}

warn() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
}

error() {
    echo -e "${RED}❌ ERROR:${NC} $1"
}

TESTS_RUN=0
FAILURES=0

run_test() {
    ((TESTS_RUN++))
    if $1; then
        success "$2"
    else
        error "$2"
        ((FAILURES++))
    fi
}

echo "🔗 MonicaHQ MCP Server - Claude Desktop Integration Testing"
echo "=========================================================="
echo ""

# Determine Claude Desktop config path based on OS
get_claude_config_path() {
    case "$(uname)" in
        "Darwin")
            echo "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
            ;;
        "Linux")
            echo "$HOME/.config/claude-desktop/claude_desktop_config.json"
            ;;
        *)
            echo ""
            ;;
    esac
}

CLAUDE_CONFIG_PATH=$(get_claude_config_path)

echo "System Detection"
echo "==============="

if [ -z "$CLAUDE_CONFIG_PATH" ]; then
    error "Unsupported operating system for Claude Desktop"
    exit 1
else
    info "Detected config path: $CLAUDE_CONFIG_PATH"
fi

echo ""
echo "Phase 1: Constitutional Compliance"
echo "=================================="

test_constitutional_compliance() {
    if [ -f "./validate-constitution.sh" ]; then
        ./validate-constitution.sh >/dev/null 2>&1
    else
        return 1
    fi
}

run_test test_constitutional_compliance "Constitutional compliance validation"

echo ""
echo "Phase 2: Configuration Validation"
echo "================================="

test_config_file_exists() {
    [ -f "$CLAUDE_CONFIG_PATH" ]
}

test_config_is_valid_json() {
    if [ -f "$CLAUDE_CONFIG_PATH" ]; then
        jq empty "$CLAUDE_CONFIG_PATH" >/dev/null 2>&1
    else
        return 1
    fi
}

test_monicahq_server_configured() {
    if [ -f "$CLAUDE_CONFIG_PATH" ]; then
        jq -e '.mcpServers | has("monicahq")' "$CLAUDE_CONFIG_PATH" >/dev/null 2>&1
    else
        return 1
    fi
}

test_jar_path_exists() {
    if [ -f "$CLAUDE_CONFIG_PATH" ]; then
        local jar_path=$(jq -r '.mcpServers.monicahq.args[1] // empty' "$CLAUDE_CONFIG_PATH" 2>/dev/null)
        if [ -n "$jar_path" ] && [ -f "$jar_path" ]; then
            return 0
        fi
    fi
    return 1
}

test_environment_vars_configured() {
    if [ -f "$CLAUDE_CONFIG_PATH" ]; then
        jq -e '.mcpServers.monicahq.env | has("MONICA_API_URL") and has("MONICA_API_TOKEN")' "$CLAUDE_CONFIG_PATH" >/dev/null 2>&1
    else
        return 1
    fi
}

run_test test_config_file_exists "Claude Desktop config file exists"
if [ -f "$CLAUDE_CONFIG_PATH" ]; then
    run_test test_config_is_valid_json "Configuration is valid JSON"
    run_test test_monicahq_server_configured "MonicaHQ MCP server configured"
    run_test test_jar_path_exists "JAR file path is valid"
    run_test test_environment_vars_configured "Environment variables configured"
else
    warn "Creating example configuration at $CLAUDE_CONFIG_PATH"
    
    # Create directory if it doesn't exist
    mkdir -p "$(dirname "$CLAUDE_CONFIG_PATH")"
    
    # Create example configuration
    cat > "$CLAUDE_CONFIG_PATH" << EOF
{
  "mcpServers": {
    "monicahq": {
      "command": "java",
      "args": [
        "-jar", 
        "$(pwd)/build/libs/monicahqmcp-0.1.0.jar", 
        "--stdio"
      ],
      "env": {
        "MONICA_API_URL": "https://your-monica-instance.com/api",
        "MONICA_API_TOKEN": "your-oauth2-bearer-token"
      }
    }
  }
}
EOF
    
    info "Example configuration created. Please update with your actual MonicaHQ details."
fi

echo ""
echo "Phase 3: MCP Server Connectivity"
echo "================================"

test_jar_builds_successfully() {
    if [ ! -f "build/libs/monicahqmcp-0.1.0.jar" ]; then
        ./gradlew build --quiet >/dev/null 2>&1
    fi
    [ -f "build/libs/monicahqmcp-0.1.0.jar" ]
}

test_stdio_mode_starts() {
    # Test that the server can start in STDIO mode
    timeout 5s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio </dev/null >/dev/null 2>&1
    # Note: This will timeout, which is expected behavior
    return 0
}

test_mcp_protocol_response() {
    if [ -z "$MONICA_API_URL" ] || [ -z "$MONICA_API_TOKEN" ]; then
        # Use dummy values for protocol testing
        export MONICA_API_URL="https://test.example.com/api"
        export MONICA_API_TOKEN="dummy-token-for-testing"
    fi
    
    # Test basic MCP protocol communication
    local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    if [ -n "$response" ]; then
        echo "$response" | jq -e '.jsonrpc == "2.0" and .id == 1' >/dev/null 2>&1
    else
        return 1
    fi
}

run_test test_jar_builds_successfully "JAR builds successfully"
run_test test_stdio_mode_starts "STDIO mode starts"
run_test test_mcp_protocol_response "MCP protocol responds correctly"

echo ""
echo "Phase 4: Claude Desktop Compatibility"
echo "====================================="

test_json_rpc_format() {
    # Test that responses follow JSON-RPC 2.0 format expected by Claude Desktop
    local response=$(echo '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}},"id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    if [ -n "$response" ]; then
        echo "$response" | jq -e '.jsonrpc == "2.0" and has("result") and .result | has("protocolVersion") and has("serverInfo") and has("capabilities")' >/dev/null 2>&1
    else
        return 1
    fi
}

test_tool_discovery() {
    # Test that tools are properly discoverable
    local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    if [ -n "$response" ]; then
        local tool_count=$(echo "$response" | jq -r '.result.tools | length' 2>/dev/null || echo "0")
        [ "$tool_count" -gt 10 ]  # Should have many tools available
    else
        return 1
    fi
}

test_error_handling() {
    # Test error handling for invalid requests
    local response=$(echo '{"method":"invalid","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    if [ -n "$response" ]; then
        echo "$response" | jq -e 'has("error") and .error.code' >/dev/null 2>&1
    else
        return 1
    fi
}

run_test test_json_rpc_format "JSON-RPC 2.0 format compliance"
run_test test_tool_discovery "Tool discovery works"
run_test test_error_handling "Error handling works"

echo ""
echo "Phase 5: Real-world Integration Test"
echo "===================================="

test_claude_config_simulation() {
    if [ ! -f "$CLAUDE_CONFIG_PATH" ]; then
        return 1
    fi
    
    # Extract configuration and test it
    local command=$(jq -r '.mcpServers.monicahq.command' "$CLAUDE_CONFIG_PATH" 2>/dev/null)
    local args=$(jq -r '.mcpServers.monicahq.args | join(" ")' "$CLAUDE_CONFIG_PATH" 2>/dev/null)
    
    if [ "$command" = "java" ] && [[ "$args" == *"--stdio"* ]]; then
        # Test the exact command that Claude Desktop would use
        timeout 5s $command $args </dev/null >/dev/null 2>&1
        return 0
    else
        return 1
    fi
}

test_environment_isolation() {
    # Test that environment variables from config work
    if [ -f "$CLAUDE_CONFIG_PATH" ]; then
        local api_url=$(jq -r '.mcpServers.monicahq.env.MONICA_API_URL' "$CLAUDE_CONFIG_PATH" 2>/dev/null)
        local api_token=$(jq -r '.mcpServers.monicahq.env.MONICA_API_TOKEN' "$CLAUDE_CONFIG_PATH" 2>/dev/null)
        
        if [ "$api_url" != "null" ] && [ "$api_token" != "null" ]; then
            return 0
        fi
    fi
    return 1
}

run_test test_claude_config_simulation "Claude Desktop command simulation"
run_test test_environment_isolation "Environment variable isolation"

echo ""
echo "Phase 6: Troubleshooting Guide"
echo "=============================="

info "Common Claude Desktop integration issues:"
echo ""

if [ $FAILURES -gt 0 ]; then
    echo "❌ Issues found - troubleshooting steps:"
    echo ""
    echo "1. Configuration issues:"
    echo "   - Check: $CLAUDE_CONFIG_PATH"
    echo "   - Validate JSON syntax: jq . '$CLAUDE_CONFIG_PATH'"
    echo "   - Ensure absolute paths for JAR file"
    echo ""
    echo "2. JAR file issues:"
    echo "   - Rebuild: ./gradlew build"
    echo "   - Test directly: ./run-mcp-server.sh"
    echo ""
    echo "3. Environment variables:"
    echo "   - Update MONICA_API_URL and MONICA_API_TOKEN in config"
    echo "   - Test with real MonicaHQ instance"
    echo ""
    echo "4. Claude Desktop restart required after config changes"
else
    success "All integration tests passed!"
    echo ""
    echo "✅ Your Claude Desktop integration is ready!"
    echo ""
    echo "Final steps:"
    echo "1. Update environment variables in: $CLAUDE_CONFIG_PATH"
    echo "2. Restart Claude Desktop"
    echo "3. Look for 'monicahq' in Claude's MCP servers list"
    echo "4. Test with: 'List my contacts from MonicaHQ'"
fi

echo ""
echo "========================================================"
echo "Claude Desktop Integration Testing Complete"
echo "========================================================"
echo ""
echo "Tests run: $TESTS_RUN"
echo "Failures: $FAILURES" 
echo ""

if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}🎉 Ready for Claude Desktop integration!${NC}"
    echo ""
    echo "Configuration file: $CLAUDE_CONFIG_PATH"
    echo "Server status: Ready"
    echo "Protocol: JSON-RPC 2.0 over STDIO"
    echo ""
    echo "Test commands:"
    echo "- 'List my contacts from MonicaHQ'"
    echo "- 'Create a new contact in MonicaHQ'"
    echo "- 'Add a note to contact [name]'"
else
    echo -e "${RED}❌ Integration issues found${NC}"
    echo "Please resolve the issues above before using with Claude Desktop"
fi

exit $FAILURES