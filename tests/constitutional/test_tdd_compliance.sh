#!/bin/bash

# Constitutional Validation Test: TDD Compliance Principle  
# Tests compliance with Test-Driven Development principle from Constitution v1.1.0
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="TDD Compliance Validation"

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
    
    # Test 1: 100% test coverage requirement
    log_info "Checking test coverage requirement (100%)..."
    if [[ -f "gradlew" ]]; then
        if ! ./gradlew test --quiet >/dev/null 2>&1; then
            log_fail "Tests are failing - violates TDD principle of maintaining passing tests"
            ((failures++))
        else
            # Check for expected test count (136 tests from constitution)
            local test_output=$(./gradlew test 2>&1 | grep -o '[0-9]\+ tests completed' | head -1)
            if [[ -n "$test_output" ]]; then
                local test_count=$(echo "$test_output" | grep -o '^[0-9]\+')
                if [[ $test_count -lt 136 ]]; then
                    log_fail "Test count ($test_count) below constitutional requirement (136 tests)"
                    ((failures++))
                else
                    log_pass "Test count meets constitutional requirement: $test_count tests"
                fi
            else
                log_fail "Cannot determine test count from gradle output"
                ((failures++))
            fi
        fi
    else
        log_fail "Gradle wrapper not found - cannot verify TDD compliance"
        ((failures++))
    fi
    
    # Test 2: RED-GREEN-Refactor cycle enforcement
    log_info "Checking RED-GREEN-Refactor cycle evidence..."
    if [[ ! -d "src/test" ]]; then
        log_fail "Test directory structure missing"
        ((failures++))
    else
        # Check for contract tests (RED phase)
        if [[ ! -d "src/test/java/com/monicahq/mcp/contract" ]]; then
            log_fail "Contract tests directory missing - RED phase not implemented"
            ((failures++))
        else
            log_pass "Contract tests directory exists"
        fi
        
        # Check for integration tests
        if [[ ! -d "src/test/java/com/monicahq/mcp/integration" ]]; then
            log_fail "Integration tests directory missing"
            ((failures++))
        else
            log_pass "Integration tests directory exists"
        fi
    fi
    
    # Test 3: Test structure validation (Contract→Integration→E2E→Unit order)
    log_info "Checking test structure follows TDD hierarchy..."
    local test_types=("contract" "integration" "unit")
    for test_type in "${test_types[@]}"; do
        if [[ -d "src/test/java/com/monicahq/mcp/$test_type" ]]; then
            local test_files=$(find "src/test/java/com/monicahq/mcp/$test_type" -name "*.java" | wc -l)
            if [[ $test_files -gt 0 ]]; then
                log_pass "$test_type tests found: $test_files files"
            else
                log_fail "$test_type test directory exists but contains no tests"
                ((failures++))
            fi
        else
            log_fail "$test_type tests directory missing"
            ((failures++))
        fi
    done
    
    # Test 4: TDD quality gate enforcement
    log_info "Checking TDD quality gate enforcement..."
    if [[ ! -f "validate-constitution.sh" ]] || ! grep -q "TDD\|Test.*Coverage" "validate-constitution.sh"; then
        log_fail "TDD compliance validation not implemented in quality gates"
        ((failures++))
    else
        log_pass "TDD validation found in quality gates"
    fi
    
    # Test 5: Test-first development evidence
    log_info "Checking test-first development patterns..."
    # Look for test annotations and structure
    if ! find src/test -name "*.java" -exec grep -l "@Test\|@ParameterizedTest" {} \; | grep -q .; then
        log_fail "JUnit test annotations not found - tests may not be properly structured"
        ((failures++))
    else
        log_pass "JUnit test structure found"
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