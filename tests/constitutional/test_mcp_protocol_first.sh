#!/bin/bash

# Constitutional Validation Test: MCP Protocol First Principle
# Tests compliance with MCP Protocol First principle from Constitution v1.1.0
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="MCP Protocol First Compliance"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }

main() {
    log_info "Testing: $TEST_NAME"
    cd "$PROJECT_ROOT"
    
    local failures=0
    
    # Test 1: JSON-RPC 2.0 over STDIO specification compliance
    log_info "Checking JSON-RPC 2.0 specification compliance..."
    if ! find src -name "*.java" -exec grep -l '"jsonrpc".*"2\.0"' {} \; | grep -q .; then
        log_fail "JSON-RPC 2.0 version field not found in responses"
        ((failures++))
    else
        log_pass "JSON-RPC 2.0 version field found"
    fi
    
    # Test 2: Tool categorization and discoverability
    log_info "Checking MCP tool categorization..."
    if ! find src -name "*.java" -exec grep -l "tools/list\|listTools" {} \; | grep -q .; then
        log_fail "MCP tool listing functionality not implemented"
        ((failures++))
    else
        log_pass "MCP tool listing functionality found"
    fi
    
    # Test 3: STDOUT cleanliness for Claude Desktop integration
    log_info "Checking STDOUT cleanliness for MCP protocol..."
    if [[ ! -f "src/main/resources/logback-stdio.xml" ]] || ! grep -q "System.err" "src/main/resources/logback-stdio.xml"; then
        log_fail "STDOUT contamination prevention not configured"
        ((failures++))
    else
        log_pass "STDOUT logging properly redirected to STDERR"
    fi
    
    # Test 4: MCP protocol error handling
    log_info "Checking MCP protocol error handling..."
    if ! find src -name "*.java" -exec grep -l '"error".*"code".*"message"' {} \; | grep -q .; then
        log_fail "MCP protocol error response format not implemented"
        ((failures++))
    else
        log_pass "MCP protocol error handling found"
    fi
    
    # Test 5: Protocol compliance validation script
    log_info "Checking for protocol compliance validation..."
    if [[ ! -f "validate-constitution.sh" ]] || ! grep -q "MCP Protocol" "validate-constitution.sh"; then
        log_fail "MCP protocol compliance validation not implemented"
        ((failures++))
    else
        log_pass "MCP protocol validation script exists"
    fi
    
    echo "=================================="
    if [[ $failures -eq 0 ]]; then
        log_pass "$TEST_NAME: All tests passed"
        return 0
    else
        log_fail "$TEST_NAME: $failures test(s) failed"
        return 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi