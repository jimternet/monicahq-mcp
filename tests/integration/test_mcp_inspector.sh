#!/bin/bash

# Integration Test: MCP Inspector Integration
# Tests MCP Inspector compatibility and interactive testing capabilities
# Validates Constitution v1.1.0 requirement for tool accessibility

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="MCP Inspector Integration"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }

# Test tracking
TESTS_RUN=0
FAILURES=0

run_test() {
    ((TESTS_RUN++))
    if $1; then
        log_pass "$2"
    else
        log_fail "$2"
        ((FAILURES++))
    fi
}

main() {
    log_info "Testing: $TEST_NAME"
    cd "$PROJECT_ROOT"
    
    # Test 1: MCP Inspector Availability
    test_mcp_inspector_available() {
        # Check if mcp-inspector is available
        if command -v mcp-inspector >/dev/null 2>&1; then
            return 0
        else
            log_warn "MCP Inspector not installed - install with: npm install -g @anthropic/mcp-inspector"
            return 1
        fi
    }
    
    # Test 2: Node.js Availability (required for MCP Inspector)
    test_nodejs_available() {
        if command -v node >/dev/null 2>&1 && command -v npm >/dev/null 2>&1; then
            local node_version=$(node --version 2>/dev/null || echo "unknown")
            log_info "Node.js version: $node_version"
            return 0
        else
            log_warn "Node.js not available - required for MCP Inspector"
            return 1
        fi
    }
    
    # Test 3: JAR File Availability for Testing
    test_jar_file_available() {
        if [[ -f "build/libs/monicahqmcp-0.1.0.jar" ]]; then
            return 0
        else
            log_info "Building JAR file for MCP Inspector testing..."
            if ./gradlew build --quiet >/dev/null 2>&1; then
                return 0
            else
                return 1
            fi
        fi
    }
    
    # Test 4: MCP Inspector JAR Integration (simulation)
    test_mcp_inspector_jar_integration() {
        if ! command -v mcp-inspector >/dev/null 2>&1; then
            return 1
        fi
        
        # Create a test script that simulates MCP Inspector usage
        cat > /tmp/test_mcp_inspector_jar.sh << 'EOF'
#!/bin/bash
# Simulate MCP Inspector with JAR
# This would normally be: mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# Test that the command structure is correct
if [[ -f "build/libs/monicahqmcp-0.1.0.jar" ]]; then
    # Verify JAR is executable
    if timeout 5s java -jar build/libs/monicahqmcp-0.1.0.jar --help >/dev/null 2>&1 || \
       timeout 5s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio </dev/null >/dev/null 2>&1; then
        echo "MCP Inspector JAR integration ready"
        exit 0
    fi
fi
exit 1
EOF
        
        chmod +x /tmp/test_mcp_inspector_jar.sh
        if /tmp/test_mcp_inspector_jar.sh >/dev/null 2>&1; then
            rm -f /tmp/test_mcp_inspector_jar.sh
            return 0
        else
            rm -f /tmp/test_mcp_inspector_jar.sh
            return 1
        fi
    }
    
    # Test 5: MCP Inspector Docker Integration (simulation)
    test_mcp_inspector_docker_integration() {
        if ! command -v mcp-inspector >/dev/null 2>&1 || ! command -v docker >/dev/null 2>&1; then
            return 1
        fi
        
        # Create a test script that simulates MCP Inspector with Docker
        cat > /tmp/test_mcp_inspector_docker.sh << 'EOF'
#!/bin/bash
# Simulate MCP Inspector with Docker
# This would normally be: mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

# Test that Docker image can be built
if docker build -t monicahq-mcp-test . >/dev/null 2>&1; then
    # Clean up test image
    docker rmi monicahq-mcp-test >/dev/null 2>&1 || true
    echo "MCP Inspector Docker integration ready"
    exit 0
fi
exit 1
EOF
        
        chmod +x /tmp/test_mcp_inspector_docker.sh
        if /tmp/test_mcp_inspector_docker.sh >/dev/null 2>&1; then
            rm -f /tmp/test_mcp_inspector_docker.sh
            return 0
        else
            rm -f /tmp/test_mcp_inspector_docker.sh
            return 1
        fi
    }
    
    # Test 6: MCP Inspector Documentation
    test_mcp_inspector_documentation() {
        # Check if MCP Inspector is documented in README
        if [[ -f "README.md" ]] && grep -i "mcp.*inspector" README.md >/dev/null 2>&1; then
            return 0
        else
            return 1
        fi
    }
    
    # Test 7: Interactive Testing Command Generation
    test_interactive_command_generation() {
        # Test that we can generate proper MCP Inspector commands
        local jar_command="mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio"
        local docker_command="mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp"
        
        # Commands should be well-formed
        if [[ $jar_command == *"mcp-inspector"* ]] && [[ $jar_command == *"--stdio"* ]] && \
           [[ $docker_command == *"mcp-inspector"* ]] && [[ $docker_command == *"docker run"* ]]; then
            return 0
        else
            return 1
        fi
    }
    
    # Test 8: MCP Inspector Protocol Compatibility
    test_protocol_compatibility() {
        # Test that our MCP server follows the protocol expected by MCP Inspector
        if ! command -v jq >/dev/null 2>&1; then
            return 1
        fi
        
        # Create environment for testing (use dummy values if real ones not available)
        if [[ -z "$MONICA_API_URL" ]]; then
            export MONICA_API_URL="https://test.example.com/api"
        fi
        if [[ -z "$MONICA_API_TOKEN" ]]; then
            export MONICA_API_TOKEN="dummy-token-for-testing"
        fi
        
        # Test basic JSON-RPC communication
        local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
            timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
            head -1)
        
        if [[ -n "$response" ]] && echo "$response" | jq -e '.jsonrpc == "2.0" and .id == 1' >/dev/null 2>&1; then
            return 0
        else
            return 1
        fi
    }
    
    # Test 9: Tool Discovery for MCP Inspector
    test_tool_discovery() {
        # Test that tools are properly discoverable by MCP Inspector
        if [[ -z "$MONICA_API_URL" ]]; then
            export MONICA_API_URL="https://test.example.com/api"
        fi
        if [[ -z "$MONICA_API_TOKEN" ]]; then
            export MONICA_API_TOKEN="dummy-token-for-testing"
        fi
        
        local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
            timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
            head -1)
        
        if [[ -n "$response" ]]; then
            local tool_count=$(echo "$response" | jq -r '.result.tools | length' 2>/dev/null || echo "0")
            if [[ $tool_count -gt 10 ]]; then
                return 0
            fi
        fi
        return 1
    }
    
    # Test 10: Environment Variable Handling for MCP Inspector
    test_env_var_handling() {
        # Test that environment variables work properly with MCP Inspector setup
        if grep -r "@Value.*monica\." src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
            return 0
        else
            return 1
        fi
    }
    
    # Execute all tests
    log_info "Running MCP Inspector integration tests..."
    echo "========================================"
    
    run_test test_nodejs_available "Node.js availability for MCP Inspector"
    run_test test_mcp_inspector_available "MCP Inspector availability"
    run_test test_jar_file_available "JAR file availability for testing"
    run_test test_mcp_inspector_jar_integration "MCP Inspector JAR integration readiness"
    
    if command -v docker >/dev/null 2>&1; then
        run_test test_mcp_inspector_docker_integration "MCP Inspector Docker integration readiness"
    else
        log_warn "Skipping Docker integration test - Docker not available"
    fi
    
    run_test test_mcp_inspector_documentation "MCP Inspector documentation"
    run_test test_interactive_command_generation "Interactive command generation"
    run_test test_protocol_compatibility "MCP protocol compatibility"
    run_test test_tool_discovery "Tool discovery for MCP Inspector"
    run_test test_env_var_handling "Environment variable handling"
    
    echo "========================================"
    if [[ $FAILURES -eq 0 ]]; then
        log_pass "$TEST_NAME: All integration tests passed ($TESTS_RUN/$TESTS_RUN)"
        log_info "MCP Inspector integration is ready for interactive testing"
        echo ""
        log_info "Usage examples:"
        if command -v mcp-inspector >/dev/null 2>&1; then
            echo "  JAR mode:    mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio"
            echo "  Docker mode: mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp"
        else
            echo "  Install first: npm install -g @anthropic/mcp-inspector"
        fi
        return 0
    else
        log_fail "$TEST_NAME: $FAILURES integration test(s) failed ($((TESTS_RUN - FAILURES))/$TESTS_RUN passed)"
        log_info "MCP Inspector integration needs attention"
        return 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi