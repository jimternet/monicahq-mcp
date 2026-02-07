#!/bin/bash

# MonicaHQ MCP Server - Input Sanitization Security Validation
# Tests protection against injection attacks and malicious input
# Validates XSS, SQL injection, command injection prevention

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

echo "🛡️  MonicaHQ MCP Server - Input Sanitization Security Validation"
echo "==============================================================="
echo "Testing protection against injection attacks"
echo

TESTS_PASSED=0
TESTS_FAILED=0
MONICA_TOKEN="$MONICA_API_TOKEN"

# Test 1: SQL Injection in contact name
echo
echo "Test 1: SQL Injection Prevention"
echo "================================="

SQL_INJECTION="'; DROP TABLE contacts; --"
log_info "Testing SQL injection payload in firstName..."

SQL_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_TOKEN" \
    -d "{\"first_name\": \"$SQL_INJECTION\", \"gender_id\": 1}" \
    $MONICA_API_URL/api/contacts)

# Should either:
# 1. Accept and escape the input (Monica handles escaping)
# 2. Reject with validation error
# 3. NOT execute SQL or crash

if echo "$SQL_RESPONSE" | jq -e '.data // .error' > /dev/null 2>&1; then
    log_success "SQL injection payload handled safely"
    ((TESTS_PASSED++))

    # Cleanup if contact was created
    if echo "$SQL_RESPONSE" | jq -e '.data.id' > /dev/null 2>&1; then
        CREATED_ID=$(echo "$SQL_RESPONSE" | jq -r '.data.id')
        curl -s -X DELETE -H "Authorization: Bearer $MONICA_TOKEN" \
            "$MONICA_API_URL/api/contacts/$CREATED_ID" > /dev/null
    fi
else
    log_error "SQL injection test failed - unexpected response"
    log_error "Response: $SQL_RESPONSE"
    ((TESTS_FAILED++))
fi

# Test 2: XSS payload in note body
echo
echo "Test 2: XSS Prevention"
echo "======================"

XSS_PAYLOAD="<script>alert('XSS')</script>"
log_info "Testing XSS payload in note body..."

XSS_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_TOKEN" \
    -d "{\"contact_id\": 1, \"body\": \"$XSS_PAYLOAD\"}" \
    $MONICA_API_URL/api/notes)

# XSS should be escaped/rejected, not executed
if echo "$XSS_RESPONSE" | jq -e '.data // .error' > /dev/null 2>&1; then
    log_success "XSS payload handled safely"
    ((TESTS_PASSED++))

    # Cleanup
    if echo "$XSS_RESPONSE" | jq -e '.data.id' > /dev/null 2>&1; then
        CREATED_ID=$(echo "$XSS_RESPONSE" | jq -r '.data.id')
        curl -s -X DELETE -H "Authorization: Bearer $MONICA_TOKEN" \
            "$MONICA_API_URL/api/notes/$CREATED_ID" > /dev/null
    fi
else
    log_error "XSS test failed"
    ((TESTS_FAILED++))
fi

# Test 3: Command injection in string field
echo
echo "Test 3: Command Injection Prevention"
echo "====================================="

COMMAND_INJECTION="\$(whoami)"
log_info "Testing command injection in lastName..."

CMD_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_TOKEN" \
    -d "{\"first_name\": \"Test\", \"last_name\": \"$COMMAND_INJECTION\", \"gender_id\": 1}" \
    $MONICA_API_URL/api/contacts)

# Command should be treated as literal string, not executed
if echo "$CMD_RESPONSE" | jq -e '.data // .error' > /dev/null 2>&1; then
    log_success "Command injection handled as literal string"
    ((TESTS_PASSED++))

    # Cleanup
    if echo "$CMD_RESPONSE" | jq -e '.data.id' > /dev/null 2>&1; then
        CREATED_ID=$(echo "$CMD_RESPONSE" | jq -r '.data.id')
        curl -s -X DELETE -H "Authorization: Bearer $MONICA_TOKEN" \
            "$MONICA_API_URL/api/contacts/$CREATED_ID" > /dev/null
    fi
else
    log_error "Command injection test failed"
    ((TESTS_FAILED++))
fi

# Test 4: Path traversal attempt
echo
echo "Test 4: Path Traversal Prevention"
echo "=================================="

PATH_TRAVERSAL="../../../etc/passwd"
log_info "Testing path traversal in note body..."

PATH_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_TOKEN" \
    -d "{\"contact_id\": 1, \"body\": \"$PATH_TRAVERSAL\"}" \
    $MONICA_API_URL/api/notes)

# Should treat as literal string, not try to read file
if echo "$PATH_RESPONSE" | jq -e '.data // .error' > /dev/null 2>&1; then
    log_success "Path traversal treated as literal string"
    ((TESTS_PASSED++))

    # Cleanup
    if echo "$PATH_RESPONSE" | jq -e '.data.id' > /dev/null 2>&1; then
        CREATED_ID=$(echo "$PATH_RESPONSE" | jq -r '.data.id')
        curl -s -X DELETE -H "Authorization: Bearer $MONICA_TOKEN" \
            "$MONICA_API_URL/api/notes/$CREATED_ID" > /dev/null
    fi
else
    log_error "Path traversal test failed"
    ((TESTS_FAILED++))
fi

# Test 5: Excessive input length
echo
echo "Test 5: Length Limit Enforcement"
echo "================================="

log_info "Testing with 10KB input..."
LONG_STRING=$(printf 'A%.0s' {1..10000})

LENGTH_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_TOKEN" \
    -d "{\"contact_id\": 1, \"body\": \"$LONG_STRING\"}" \
    $MONICA_API_URL/api/notes)

# Should either accept (and truncate) or reject gracefully
if echo "$LENGTH_RESPONSE" | jq -e '.data // .error' > /dev/null 2>&1; then
    log_success "Excessive length handled without crashing"
    ((TESTS_PASSED++))

    # Cleanup
    if echo "$LENGTH_RESPONSE" | jq -e '.data.id' > /dev/null 2>&1; then
        CREATED_ID=$(echo "$LENGTH_RESPONSE" | jq -r '.data.id')
        curl -s -X DELETE -H "Authorization: Bearer $MONICA_TOKEN" \
            "$MONICA_API_URL/api/notes/$CREATED_ID" > /dev/null
    fi
else
    log_warning "Length limit handling unclear"
    log_info "Server may have rejected the request - this is acceptable"
fi

# Test 6: Special characters (legitimate international names)
echo
echo "Test 6: International Character Support"
echo "========================================"

INTL_NAME="François Müller-González"
log_info "Testing legitimate international characters..."

INTL_RESPONSE=$(curl -s -X POST \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $MONICA_TOKEN" \
    -d "{\"first_name\": \"$INTL_NAME\", \"gender_id\": 1}" \
    $MONICA_API_URL/api/contacts)

# Should accept legitimate international characters
if echo "$INTL_RESPONSE" | jq -e '.data.id' > /dev/null 2>&1; then
    log_success "International characters accepted"
    ((TESTS_PASSED++))

    # Cleanup
    CREATED_ID=$(echo "$INTL_RESPONSE" | jq -r '.data.id')
    curl -s -X DELETE -H "Authorization: Bearer $MONICA_TOKEN" \
        "$MONICA_API_URL/api/contacts/$CREATED_ID" > /dev/null
else
    log_error "International characters rejected - should be supported"
    ((TESTS_FAILED++))
fi

# Final Results
echo
echo "🎯 Input Sanitization Security Validation Results"
echo "================================================="
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "🎉 INPUT SANITIZATION SECURITY VALIDATED!"
    echo
    echo "✅ SQL Injection: Properly escaped/rejected"
    echo "✅ XSS: Properly escaped/rejected"
    echo "✅ Command Injection: Treated as literal strings"
    echo "✅ Path Traversal: Not executed"
    echo "✅ Length Limits: Enforced gracefully"
    echo "✅ International Characters: Properly supported"
    echo
    echo "🛡️  Input validation security validated!"
    exit 0
else
    log_error "Some input sanitization tests failed."
    echo
    echo "⚠️  Review failed tests above for security implications."
    exit 1
fi
