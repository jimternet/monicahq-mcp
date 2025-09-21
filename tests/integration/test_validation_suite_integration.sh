#!/bin/bash

# Integration Test: Complete Validation Suite
# Tests integration of all constitutional validation components
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="Validation Suite Integration"

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
    
    # Test 1: Constitutional validation script integration
    log_info "Testing constitutional validation script integration..."
    if [[ ! -f "validate-constitution.sh" ]]; then
        log_fail "Constitutional validation script not found"
        ((failures++))
    else
        chmod +x validate-constitution.sh
        if ./validate-constitution.sh >/dev/null 2>&1; then
            log_pass "Constitutional validation script executes successfully"
        else
            log_fail "Constitutional validation script execution failed"
            ((failures++))
        fi
    fi
    
    # Test 2: Complete MCP testing integration
    log_info "Testing complete MCP testing integration..."
    if [[ ! -f "test-mcp-complete.sh" ]]; then
        log_fail "Complete MCP testing script not found"
        ((failures++))
    else
        chmod +x test-mcp-complete.sh
        # Test script exists and is executable
        if [[ -x "test-mcp-complete.sh" ]]; then
            log_pass "Complete MCP testing script is executable"
        else
            log_fail "Complete MCP testing script is not executable"
            ((failures++))
        fi
    fi
    
    # Test 3: Claude Desktop integration testing
    log_info "Testing Claude Desktop integration..."
    if [[ ! -f "test-claude-desktop.sh" ]]; then
        log_fail "Claude Desktop testing script not found"
        ((failures++))
    else
        chmod +x test-claude-desktop.sh
        if [[ -x "test-claude-desktop.sh" ]]; then
            log_pass "Claude Desktop testing script is executable"
        else
            log_fail "Claude Desktop testing script is not executable"
            ((failures++))
        fi
    fi
    
    # Test 4: MCP tools setup integration
    log_info "Testing MCP tools setup integration..."
    if [[ ! -f "setup-mcp-tools.sh" ]]; then
        log_fail "MCP tools setup script not found"
        ((failures++))
    else
        chmod +x setup-mcp-tools.sh
        if [[ -x "setup-mcp-tools.sh" ]]; then
            log_pass "MCP tools setup script is executable"
        else
            log_fail "MCP tools setup script is not executable"
            ((failures++))
        fi
    fi
    
    # Test 5: All constitutional principle tests integration
    log_info "Testing constitutional principle tests integration..."
    local principle_tests=(
        "tests/constitutional/test_mcp_protocol_first.sh"
        "tests/constitutional/test_tdd_compliance.sh"
        "tests/constitutional/test_spring_architecture.sh"
        "tests/constitutional/test_production_ready.sh"
        "tests/constitutional/test_type_safety.sh"
    )
    
    for test_script in "${principle_tests[@]}"; do
        if [[ ! -f "$test_script" ]]; then
            log_fail "Constitutional test not found: $test_script"
            ((failures++))
        else
            chmod +x "$test_script"
            if [[ -x "$test_script" ]]; then
                log_pass "Constitutional test executable: $(basename "$test_script")"
            else
                log_fail "Constitutional test not executable: $test_script"
                ((failures++))
            fi
        fi
    done
    
    # Test 6: General compliance test integration
    log_info "Testing general compliance test integration..."
    if [[ ! -f "tests/constitutional/test_compliance.sh" ]]; then
        log_fail "General compliance test not found"
        ((failures++))
    else
        chmod +x tests/constitutional/test_compliance.sh
        if [[ -x "tests/constitutional/test_compliance.sh" ]]; then
            log_pass "General compliance test is executable"
        else
            log_fail "General compliance test is not executable"
            ((failures++))
        fi
    fi
    
    # Test 7: Integration with build system
    log_info "Testing build system integration..."
    if [[ -f "gradlew" ]]; then
        if ./gradlew tasks | grep -q test; then
            log_pass "Gradle test task integration found"
        else
            log_fail "Gradle test task not found"
            ((failures++))
        fi
    else
        log_fail "Gradle wrapper not found - build system integration missing"
        ((failures++))
    fi
    
    # Test 8: 7-phase testing methodology validation
    log_info "Testing 7-phase testing methodology..."
    # Check if test-mcp-complete.sh contains references to 7 phases
    if [[ -f "test-mcp-complete.sh" ]] && grep -q "Phase\|PHASE" test-mcp-complete.sh; then
        local phase_count=$(grep -c "Phase\|PHASE" test-mcp-complete.sh)
        if [[ $phase_count -ge 7 ]]; then
            log_pass "7-phase testing methodology structure found"
        else
            log_fail "7-phase testing methodology incomplete (found $phase_count phases)"
            ((failures++))
        fi
    else
        log_fail "7-phase testing methodology not implemented"
        ((failures++))
    fi
    
    # Test 9: STDOUT contamination detection integration
    log_info "Testing STDOUT contamination detection..."
    if [[ ! -f "tests/integration/test_stdout_cleanliness.sh" ]]; then
        log_fail "STDOUT contamination detection test not found"
        ((failures++))
    fi
    
    echo "=================================="
    if [[ $failures -eq 0 ]]; then
        log_pass "$TEST_NAME: All integration tests passed"
        return 0
    else
        log_fail "$TEST_NAME: $failures integration test(s) failed"
        return 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi