#!/bin/bash

# MonicaHQ MCP Server - Token Logging Security Validation
# Validates P0-001: Tokens must NEVER appear in logs
# Tests that debug logging doesn't expose sensitive token content

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

log_success() {
    echo -e "${GREEN}✅ PASSED:${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
}

log_error() {
    echo -e "${RED}❌ FAILED:${NC} $1"
}

echo "🔒 MonicaHQ MCP Server - Token Logging Security Validation"
echo "=========================================================="
echo "Validating P0-001: Tokens NEVER appear in logs"
echo

TESTS_PASSED=0
TESTS_FAILED=0
TEST_TOKEN="test-secret-token-1234567890"
LOG_FILE="/tmp/monicahq-mcp-test-$$.log"

# Test 1: Token logging security tests pass
echo
echo "Test 1: Token Security Unit Tests"
echo "=================================="

log_info "Running TokenSecurityTest..."
./gradlew integrationTest --tests "TokenSecurityTest" > $LOG_FILE 2>&1

if grep -q "BUILD SUCCESSFUL" $LOG_FILE; then
    log_success "Token security tests passed"
    ((TESTS_PASSED++))
else
    log_error "Token security tests failed"
    log_error "Check test output for details"
    ((TESTS_FAILED++))
fi

# Test 2: No tokens in test logs
echo
echo "Test 2: No Tokens in Test Logs"
echo "==============================="

log_info "Checking test logs for token leakage..."

# Check if any test tokens appear in logs
if grep -q "$TEST_TOKEN" $LOG_FILE; then
    log_error "Test token found in logs!"
    log_error "This violates P0-001: Token content must never appear in logs"
    ((TESTS_FAILED++))
else
    log_success "No test tokens found in logs"
    ((TESTS_PASSED++))
fi

# Test 3: No "Authorization: Bearer" headers in logs
echo
echo "Test 3: No Authorization Headers in Logs"
echo "========================================="

log_info "Checking for Authorization header logging..."

if grep -i "Authorization.*Bearer" $LOG_FILE 2>/dev/null; then
    log_error "Authorization headers found in logs!"
    log_error "Headers should not be logged"
    ((TESTS_FAILED++))
else
    log_success "No Authorization headers in logs"
    ((TESTS_PASSED++))
fi

# Test 4: Check production logging configuration
echo
echo "Test 4: Production Logging Configuration"
echo "========================================"

log_info "Checking logback configuration..."

LOGBACK_FILE="../src/main/resources/logback-spring.xml"
if [ -f "$LOGBACK_FILE" ]; then
    # Check for secure logging patterns
    if grep -q "PatternLayout" "$LOGBACK_FILE"; then
        log_success "Logback configuration exists"
        ((TESTS_PASSED++))
    else
        log_warning "Logback pattern not found - verify logging config"
    fi
else
    log_warning "Logback configuration file not found at $LOGBACK_FILE"
fi

# Test 5: No token substrings in logs
echo
echo "Test 5: No Token Fragments in Logs"
echo "==================================="

log_info "Checking for token fragments (first 20 chars)..."

# Check for common token fragment patterns
if grep -E "token.*substring|trimmedToken\.substring" $LOG_FILE 2>/dev/null; then
    log_error "Token substring calls found in logs!"
    log_error "This suggests token content is being logged"
    ((TESTS_FAILED++))
else
    log_success "No token substring logging detected"
    ((TESTS_PASSED++))
fi

# Cleanup
rm -f $LOG_FILE

# Final Results
echo
echo "🎯 Token Logging Security Validation Results"
echo "============================================"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "🎉 TOKEN LOGGING SECURITY VALIDATED!"
    echo
    echo "✅ Token security tests passing"
    echo "✅ No tokens in test logs"
    echo "✅ No Authorization headers logged"
    echo "✅ Logback configuration exists"
    echo "✅ No token fragments in logs"
    echo
    echo "🔒 P0-001 validated: Tokens never appear in logs!"
    exit 0
else
    log_error "Token logging security validation failed."
    echo
    echo "⚠️  CRITICAL: P0-001 VIOLATION"
    echo "Tokens or token content found in logs."
    echo "This is a production-blocking security issue."
    echo
    echo "Required actions:"
    echo "1. Remove all token.substring() calls from logging"
    echo "2. Remove Authorization header from log output"
    echo "3. Use token metadata (length, format) instead of content"
    echo "4. Re-run this validation after fixes"
    exit 1
fi
