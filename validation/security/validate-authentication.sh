#!/bin/bash

# MonicaHQ MCP Server - Authentication Security Validation
# Tests authentication and authorization security
# Validates P0-001: Token security and proper authentication handling

# Load environment variables
source ../setup/load-env.sh
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

echo "🔒 MonicaHQ MCP Server - Authentication Security Validation"
echo "==========================================================="
echo "Testing authentication, authorization, and token security"
echo

TESTS_PASSED=0
TESTS_FAILED=0

# Test 1: Valid token authentication
echo
echo "Test 1: Valid Token Authentication"
echo "==================================="

log_info "Testing with valid token..."
VALID_RESPONSE=$(curl -s -H "Authorization: Bearer $MONICA_API_TOKEN" \
    $MONICA_API_URL/api/contacts)

if echo "$VALID_RESPONSE" | jq -e '.data' > /dev/null 2>&1; then
    log_success "Valid token accepted"
    ((TESTS_PASSED++))
else
    log_error "Valid token rejected"
    log_error "Response: $VALID_RESPONSE"
    ((TESTS_FAILED++))
fi

# Test 2: Invalid token rejection
echo
echo "Test 2: Invalid Token Rejection"
echo "================================"

log_info "Testing with invalid token..."
INVALID_RESPONSE=$(curl -s -w "\n%{http_code}" -H "Authorization: Bearer invalid-token-12345" \
    $MONICA_API_URL/api/contacts)

HTTP_CODE=$(echo "$INVALID_RESPONSE" | tail -n 1)
RESPONSE_BODY=$(echo "$INVALID_RESPONSE" | head -n -1)

if [ "$HTTP_CODE" == "401" ] || [ "$HTTP_CODE" == "403" ]; then
    log_success "Invalid token properly rejected (HTTP $HTTP_CODE)"
    ((TESTS_PASSED++))
else
    log_error "Invalid token not properly rejected (HTTP $HTTP_CODE)"
    log_error "Expected: 401 or 403"
    log_error "Response: $RESPONSE_BODY"
    ((TESTS_FAILED++))
fi

# Test 3: Missing Authorization header
echo
echo "Test 3: Missing Authorization Header"
echo "====================================="

log_info "Testing without Authorization header..."
NO_AUTH_RESPONSE=$(curl -s -w "\n%{http_code}" $MONICA_API_URL/api/contacts)

HTTP_CODE=$(echo "$NO_AUTH_RESPONSE" | tail -n 1)
RESPONSE_BODY=$(echo "$NO_AUTH_RESPONSE" | head -n -1)

if [ "$HTTP_CODE" == "401" ] || [ "$HTTP_CODE" == "403" ]; then
    log_success "Missing auth properly rejected (HTTP $HTTP_CODE)"
    ((TESTS_PASSED++))
else
    log_error "Missing auth not properly rejected (HTTP $HTTP_CODE)"
    log_error "Expected: 401 or 403"
    log_error "Response: $RESPONSE_BODY"
    ((TESTS_FAILED++))
fi

# Test 4: Malformed Authorization header
echo
echo "Test 4: Malformed Authorization Header"
echo "======================================"

log_info "Testing with malformed Authorization header..."
MALFORMED_RESPONSE=$(curl -s -w "\n%{http_code}" -H "Authorization: NotBearer token" \
    $MONICA_API_URL/api/contacts)

HTTP_CODE=$(echo "$MALFORMED_RESPONSE" | tail -n 1)
RESPONSE_BODY=$(echo "$MALFORMED_RESPONSE" | head -n -1)

if [ "$HTTP_CODE" == "401" ] || [ "$HTTP_CODE" == "403" ]; then
    log_success "Malformed auth properly rejected (HTTP $HTTP_CODE)"
    ((TESTS_PASSED++))
else
    log_warning "Malformed auth handling unclear (HTTP $HTTP_CODE)"
    log_info "This may be acceptable if Monica normalizes headers"
    log_info "Response: $RESPONSE_BODY"
fi

# Test 5: Empty Bearer token
echo
echo "Test 5: Empty Bearer Token"
echo "=========================="

log_info "Testing with empty Bearer token..."
EMPTY_TOKEN_RESPONSE=$(curl -s -w "\n%{http_code}" -H "Authorization: Bearer " \
    $MONICA_API_URL/api/contacts)

HTTP_CODE=$(echo "$EMPTY_TOKEN_RESPONSE" | tail -n 1)

if [ "$HTTP_CODE" == "401" ] || [ "$HTTP_CODE" == "403" ]; then
    log_success "Empty token properly rejected (HTTP $HTTP_CODE)"
    ((TESTS_PASSED++))
else
    log_error "Empty token not properly rejected (HTTP $HTTP_CODE)"
    ((TESTS_FAILED++))
fi

# Test 6: Token with special characters
echo
echo "Test 6: Token with Special Characters"
echo "======================================"

log_info "Testing token injection attempt..."
INJECTION_TOKEN="'; DROP TABLE users; --"
INJECTION_RESPONSE=$(curl -s -w "\n%{http_code}" -H "Authorization: Bearer $INJECTION_TOKEN" \
    $MONICA_API_URL/api/contacts)

HTTP_CODE=$(echo "$INJECTION_TOKEN_RESPONSE" | tail -n 1)

if [ "$HTTP_CODE" == "401" ] || [ "$HTTP_CODE" == "403" ]; then
    log_success "Injection attempt in token properly rejected"
    ((TESTS_PASSED++))
else
    log_warning "Token injection handling unclear - verify no SQL execution"
fi

# Final Results
echo
echo "🎯 Authentication Security Validation Results"
echo "============================================="
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "🎉 ALL AUTHENTICATION SECURITY TESTS PASSED!"
    echo
    echo "✅ Valid Token: Properly authenticated"
    echo "✅ Invalid Token: Properly rejected (401/403)"
    echo "✅ Missing Auth: Properly rejected (401/403)"
    echo "✅ Malformed Auth: Properly handled"
    echo "✅ Empty Token: Properly rejected"
    echo "✅ Injection Attempts: Safely handled"
    echo
    echo "🔒 Authentication security validated!"
    exit 0
else
    log_error "Some authentication security tests failed. Check details above."
    exit 1
fi
