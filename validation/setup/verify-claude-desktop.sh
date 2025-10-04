#!/bin/bash

# Claude Desktop Setup Verification Script
# Validates that Claude Desktop can connect to and use the MonicaHQ MCP server

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LOG_FILE="/tmp/claude-desktop-verification.log"
CLAUDE_CONFIG_DIR="$HOME/Library/Application Support/Claude"
CLAUDE_CONFIG_FILE="$CLAUDE_CONFIG_DIR/claude_desktop_config.json"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Error handling
error_exit() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    log "ERROR: $1"
    exit 1
}

# Success message
success() {
    echo -e "${GREEN}✓ $1${NC}"
    log "SUCCESS: $1"
}

# Warning message
warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
    log "WARNING: $1"
}

# Info message
info() {
    echo -e "${BLUE}ℹ $1${NC}"
    log "INFO: $1"
}

# Check if Claude Desktop is installed
check_claude_desktop_installed() {
    info "Checking if Claude Desktop is installed..."
    
    if [ -d "/Applications/Claude.app" ]; then
        success "Claude Desktop found at /Applications/Claude.app"
        return 0
    fi
    
    # Check alternative installation locations
    for location in "/Applications/Claude Desktop.app" "/System/Applications/Claude.app"; do
        if [ -d "$location" ]; then
            success "Claude Desktop found at $location"
            return 0
        fi
    done
    
    error_exit "Claude Desktop not found. Please install Claude Desktop from https://claude.ai/download"
}

# Check if Claude Desktop config directory exists
check_config_directory() {
    info "Checking Claude Desktop configuration directory..."
    
    if [ ! -d "$CLAUDE_CONFIG_DIR" ]; then
        warning "Claude Desktop config directory not found. Creating it..."
        mkdir -p "$CLAUDE_CONFIG_DIR" || error_exit "Failed to create config directory"
    fi
    
    success "Claude Desktop config directory exists: $CLAUDE_CONFIG_DIR"
}

# Check MonicaHQ MCP server JAR
check_mcp_server_jar() {
    info "Checking MonicaHQ MCP server JAR file..."
    
    local jar_file="$PROJECT_ROOT/build/libs/monica-hq-mcp-*.jar"
    if ls $jar_file 1> /dev/null 2>&1; then
        success "MCP server JAR found: $(ls $jar_file | head -1)"
        return 0
    fi
    
    warning "MCP server JAR not found. Building it now..."
    cd "$PROJECT_ROOT"
    ./gradlew build -x test || error_exit "Failed to build MCP server JAR"
    
    if ls $jar_file 1> /dev/null 2>&1; then
        success "MCP server JAR built successfully: $(ls $jar_file | head -1)"
    else
        error_exit "Failed to build MCP server JAR"
    fi
}

# Check environment variables
check_environment_variables() {
    info "Checking required environment variables..."
    
    if [ -z "${MONICA_API_URL:-}" ]; then
        error_exit "MONICA_API_URL environment variable is not set"
    fi
    success "MONICA_API_URL is set: $MONICA_API_URL"
    
    if [ -z "${MONICA_API_TOKEN:-}" ]; then
        error_exit "MONICA_API_TOKEN environment variable is not set"
    fi
    success "MONICA_API_TOKEN is set (length: ${#MONICA_API_TOKEN} characters)"
}

# Generate Claude Desktop configuration
generate_claude_config() {
    info "Generating Claude Desktop configuration..."
    
    local jar_file=$(ls "$PROJECT_ROOT/build/libs/monica-hq-mcp-"*.jar | head -1)
    local java_path="/usr/bin/java"
    
    # Check for Java installation
    if command -v java >/dev/null 2>&1; then
        java_path=$(which java)
        success "Java found at: $java_path"
    else
        error_exit "Java not found. Please install Java 17 or later"
    fi
    
    # Create the configuration
    cat > "$CLAUDE_CONFIG_FILE" << EOF
{
  "mcpServers": {
    "monicahq": {
      "command": "$java_path",
      "args": [
        "-jar",
        "$jar_file"
      ],
      "env": {
        "MONICA_API_URL": "$MONICA_API_URL",
        "MONICA_API_TOKEN": "$MONICA_API_TOKEN",
        "MCP_DEBUG": "false"
      }
    }
  }
}
EOF
    
    success "Claude Desktop configuration created: $CLAUDE_CONFIG_FILE"
}

# Generate debug configuration
generate_debug_config() {
    info "Generating Claude Desktop debug configuration..."
    
    local jar_file=$(ls "$PROJECT_ROOT/build/libs/monica-hq-mcp-"*.jar | head -1)
    local java_path=$(which java)
    local debug_config_file="$CLAUDE_CONFIG_DIR/claude_desktop_config_debug.json"
    
    cat > "$debug_config_file" << EOF
{
  "mcpServers": {
    "monicahq-debug": {
      "command": "$java_path",
      "args": [
        "-jar",
        "$jar_file"
      ],
      "env": {
        "MONICA_API_URL": "$MONICA_API_URL",
        "MONICA_API_TOKEN": "$MONICA_API_TOKEN",
        "MCP_DEBUG": "true"
      }
    }
  }
}
EOF
    
    success "Claude Desktop debug configuration created: $debug_config_file"
    info "To use debug mode, copy this file to: $CLAUDE_CONFIG_FILE"
}

# Test MCP server directly
test_mcp_server_direct() {
    info "Testing MCP server directly..."
    
    cd "$PROJECT_ROOT"
    local jar_file=$(ls "$PROJECT_ROOT/build/libs/monica-hq-mcp-"*.jar | head -1)
    
    # Test initialize message
    local test_message='{"jsonrpc":"2.0","id":"test-1","method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"test-client","version":"1.0.0"}}}'
    
    info "Sending initialize message to MCP server..."
    echo "$test_message" | timeout 10s java -jar "$jar_file" > /tmp/mcp_test_output.json 2>/tmp/mcp_test_error.log
    
    if [ $? -eq 0 ]; then
        success "MCP server responded successfully to initialize message"
        log "MCP Response: $(cat /tmp/mcp_test_output.json)"
    else
        error_exit "MCP server failed to respond. Check /tmp/mcp_test_error.log for details"
    fi
    
    # Test tools/list message
    local tools_message='{"jsonrpc":"2.0","id":"test-2","method":"tools/list","params":{}}'
    
    info "Testing tools/list endpoint..."
    echo "$tools_message" | timeout 10s java -jar "$jar_file" > /tmp/mcp_tools_output.json 2>/tmp/mcp_tools_error.log
    
    if [ $? -eq 0 ]; then
        local tool_count=$(cat /tmp/mcp_tools_output.json | grep -o '"name"' | wc -l)
        success "MCP server tools/list responded with $tool_count tools"
    else
        warning "MCP server tools/list test failed. Check /tmp/mcp_tools_error.log"
    fi
}

# Validate configuration
validate_configuration() {
    info "Validating Claude Desktop configuration..."
    
    if [ ! -f "$CLAUDE_CONFIG_FILE" ]; then
        error_exit "Claude Desktop configuration file not found: $CLAUDE_CONFIG_FILE"
    fi
    
    # Check JSON syntax
    if ! python3 -m json.tool "$CLAUDE_CONFIG_FILE" >/dev/null 2>&1; then
        error_exit "Claude Desktop configuration has invalid JSON syntax"
    fi
    
    success "Claude Desktop configuration is valid JSON"
    
    # Check for required fields
    if ! grep -q "monicahq" "$CLAUDE_CONFIG_FILE"; then
        error_exit "Configuration missing 'monicahq' server definition"
    fi
    
    success "Configuration contains MonicaHQ MCP server definition"
}

# Display setup instructions
display_setup_instructions() {
    echo ""
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}                    SETUP COMPLETE                          ${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${GREEN}✓ MonicaHQ MCP Server is ready for Claude Desktop${NC}"
    echo ""
    echo -e "${YELLOW}Next Steps:${NC}"
    echo "1. Restart Claude Desktop if it's currently running"
    echo "2. Open Claude Desktop"
    echo "3. Look for the 🔌 icon in the input area - this indicates MCP servers are connected"
    echo "4. Try asking Claude: 'Can you list my contacts from MonicaHQ?'"
    echo ""
    echo -e "${YELLOW}Configuration Files:${NC}"
    echo "• Production config: $CLAUDE_CONFIG_FILE"
    echo "• Debug config: $CLAUDE_CONFIG_DIR/claude_desktop_config_debug.json"
    echo ""
    echo -e "${YELLOW}Debug Mode:${NC}"
    echo "To enable debug mode, replace the main config with the debug config:"
    echo "cp \"$CLAUDE_CONFIG_DIR/claude_desktop_config_debug.json\" \"$CLAUDE_CONFIG_FILE\""
    echo ""
    echo -e "${YELLOW}Logs:${NC}"
    echo "• Setup log: $LOG_FILE"
    echo "• Debug logs will appear in stderr when MCP_DEBUG=true"
    echo ""
    echo -e "${YELLOW}Troubleshooting:${NC}"
    echo "If you encounter issues:"
    echo "1. Check Claude Desktop logs (Help > Show Logs in Claude Desktop)"
    echo "2. Enable debug mode for detailed logging"
    echo "3. Run the validation script: $SCRIPT_DIR/../integration/test-claude-desktop.sh"
    echo ""
}

# Main execution
main() {
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}           MonicaHQ MCP Server - Claude Desktop Setup        ${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo ""
    
    log "Starting Claude Desktop setup verification"
    
    # Clear previous log
    > "$LOG_FILE"
    
    # Run verification steps
    check_claude_desktop_installed
    check_config_directory
    check_mcp_server_jar
    check_environment_variables
    generate_claude_config
    generate_debug_config
    validate_configuration
    test_mcp_server_direct
    
    display_setup_instructions
    
    log "Claude Desktop setup verification completed successfully"
    echo -e "${GREEN}✓ All checks passed! MonicaHQ MCP Server is ready for Claude Desktop.${NC}"
}

# Run main function
main "$@"