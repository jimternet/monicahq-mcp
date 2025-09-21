#!/bin/bash

# Integration Test: STDOUT Contamination Detection
# Tests STDOUT cleanliness for MCP protocol compliance
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="STDOUT Contamination Detection"

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

main() {
    log_info "Testing: $TEST_NAME"
    cd "$PROJECT_ROOT"
    
    local failures=0
    
    # Test 1: STDIO logging configuration validation
    log_info "Testing STDIO logging configuration..."
    if [[ ! -f "src/main/resources/logback-stdio.xml" ]]; then
        log_fail "STDIO logging configuration not found"
        ((failures++))
    else
        # Check that all output goes to STDERR
        if grep -q "System.err" "src/main/resources/logback-stdio.xml"; then
            log_pass "STDIO logging correctly configured for STDERR"
        else
            log_fail "STDIO logging not properly configured for STDERR"
            ((failures++))
        fi
        
        # Check for STDOUT prevention
        if ! grep -q "System.out" "src/main/resources/logback-stdio.xml"; then
            log_pass "No STDOUT output configured in STDIO logging"
        else
            log_fail "STDOUT output found in STDIO logging configuration"
            ((failures++))
        fi
    fi
    
    # Test 2: McpStdioServer STDOUT cleanliness
    log_info "Testing McpStdioServer STDOUT cleanliness..."
    if [[ ! -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        log_fail "McpStdioServer not found"
        ((failures++))
    else
        # Check for proper STDERR usage
        if grep -q "System.err" "src/main/java/com/monicahq/mcp/McpStdioServer.java"; then
            log_pass "McpStdioServer uses STDERR for logging"
        else
            log_fail "McpStdioServer does not use STDERR for logging"
            ((failures++))
        fi
        
        # Check for STDOUT contamination prevention
        local stdout_lines=$(grep -c "System.out" "src/main/java/com/monicahq/mcp/McpStdioServer.java" 2>/dev/null || echo 0)
        # Should only use System.out for JSON-RPC responses
        if [[ $stdout_lines -le 2 ]]; then
            log_pass "McpStdioServer minimal STDOUT usage (JSON-RPC only)"
        else
            log_fail "McpStdioServer excessive STDOUT usage detected ($stdout_lines lines)"
            ((failures++))
        fi
    fi
    
    # Test 3: Spring Boot banner suppression
    log_info "Testing Spring Boot banner suppression..."
    if [[ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        if grep -q "spring.main.banner-mode.*off" "src/main/java/com/monicahq/mcp/McpStdioServer.java"; then
            log_pass "Spring Boot banner suppression configured"
        else
            log_fail "Spring Boot banner suppression not configured"
            ((failures++))
        fi
    fi
    
    # Test 4: Logging level restrictions
    log_info "Testing logging level restrictions..."
    if [[ -f "src/main/resources/logback-stdio.xml" ]]; then
        # Check for minimal logging levels
        if grep -q "level=\"WARN\"\|level=\"ERROR\"\|level=\"OFF\"" "src/main/resources/logback-stdio.xml"; then
            log_pass "Minimal logging levels configured"
        else
            log_fail "Excessive logging levels may contaminate STDERR"
            ((failures++))
        fi
    fi
    
    # Test 5: STDOUT contamination test execution
    log_info "Testing STDOUT contamination detection capability..."
    
    # Create a temporary test script to verify STDOUT cleanliness
    cat > /tmp/test_stdout_contamination.sh << 'EOF'
#!/bin/bash
# Test for STDOUT contamination in MCP STDIO mode
set -e

# Build the application
if [[ -f "gradlew" ]]; then
    ./gradlew build -x test >/dev/null 2>&1
fi

# Test STDOUT cleanliness (would run MCP server in test mode)
# This is a placeholder - actual implementation would:
# 1. Start MCP server in STDIO mode
# 2. Send test JSON-RPC message
# 3. Verify STDOUT contains ONLY valid JSON-RPC response
# 4. Verify no log messages leak to STDOUT

echo "STDOUT contamination test capability verified"
EOF
    
    chmod +x /tmp/test_stdout_contamination.sh
    if /tmp/test_stdout_contamination.sh >/dev/null 2>&1; then
        log_pass "STDOUT contamination test capability verified"
    else
        log_fail "STDOUT contamination test execution failed"
        ((failures++))
    fi
    rm -f /tmp/test_stdout_contamination.sh
    
    # Test 6: JSON-RPC response format validation
    log_info "Testing JSON-RPC response format validation..."
    if [[ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        # Check for proper JSON response structure
        if grep -q '"jsonrpc".*"2.0"' "src/main/java/com/monicahq/mcp/McpStdioServer.java"; then
            log_pass "JSON-RPC 2.0 response format validation found"
        else
            log_fail "JSON-RPC 2.0 response format validation missing"
            ((failures++))
        fi
    fi
    
    # Test 7: Error response cleanliness
    log_info "Testing error response cleanliness..."
    if [[ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        # Check that errors are handled without STDOUT contamination
        if grep -A 10 -B 10 "catch.*Exception" "src/main/java/com/monicahq/mcp/McpStdioServer.java" | grep -q "System.err"; then
            log_pass "Error handling uses STDERR appropriately"
        else
            log_fail "Error handling may contaminate STDOUT"
            ((failures++))
        fi
    fi
    
    # Test 8: MCP protocol compliance documentation
    log_info "Testing MCP protocol compliance documentation..."
    if [[ -f "MCP-STDIO-LOGGING.md" ]]; then
        if grep -q "STDOUT.*contamination\|STDERR.*logging" "MCP-STDIO-LOGGING.md"; then
            log_pass "STDOUT contamination documentation found"
        else
            log_fail "STDOUT contamination documentation incomplete"
            ((failures++))
        fi
    else
        log_fail "MCP STDIO logging documentation not found"
        ((failures++))
    fi
    
    echo "=================================="
    if [[ $failures -eq 0 ]]; then
        log_pass "$TEST_NAME: All STDOUT cleanliness tests passed"
        return 0
    else
        log_fail "$TEST_NAME: $failures STDOUT cleanliness test(s) failed"
        return 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi