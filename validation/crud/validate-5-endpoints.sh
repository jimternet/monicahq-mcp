#!/bin/bash

# MonicaHQ MCP Server - 5 Endpoint Validation
# Tests all 5 fixed endpoints against Monica API

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

log_error() {
    echo -e "${RED}❌ FAILED:${NC} $1"
}

echo "🧪 MonicaHQ MCP Server - 5 Endpoint Validation"
echo "==============================================="
echo "Testing all 5 fixed endpoints"
echo

# Load environment
MONICA_API_URL="${MONICA_API_URL:-http://localhost:8081/api}"
MONICA_TOKEN="${MONICA_API_TOKEN_INT_TESTING}"
CONTACT_ID=91
TESTS_PASSED=0
TESTS_FAILED=0

# Test 1: activity_create
echo
echo "Test 1: activity_create"
echo "========================"
HTTP_CODE=$(curl -s -o /tmp/response.json -w "%{http_code}" -X POST \
  "${MONICA_API_URL}/activities" \
  -H "Authorization: Bearer ${MONICA_TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{
    \"summary\": \"Validation test activity\",
    \"description\": \"Testing activity_create endpoint\",
    \"happened_at\": \"2026-02-08T10:00:00Z\",
    \"activity_type_id\": 1,
    \"contacts\": [${CONTACT_ID}]
  }")

if [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "200" ]; then
    ACTIVITY_ID=$(cat /tmp/response.json | grep -o '"id":[0-9]*' | head -1 | cut -d: -f2)
    log_success "activity_create - HTTP $HTTP_CODE, Created ID: $ACTIVITY_ID"
    ((TESTS_PASSED++))
else
    log_error "activity_create - HTTP $HTTP_CODE"
    cat /tmp/response.json
    ((TESTS_FAILED++))
fi

# Test 2: reminder_create
echo
echo "Test 2: reminder_create"
echo "========================"
HTTP_CODE=$(curl -s -o /tmp/response.json -w "%{http_code}" -X POST \
  "${MONICA_API_URL}/reminders" \
  -H "Authorization: Bearer ${MONICA_TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{
    \"contact_id\": ${CONTACT_ID},
    \"title\": \"Validation test reminder\",
    \"initial_date\": \"2026-03-01\",
    \"frequency_type\": \"one_time\",
    \"description\": \"Testing reminder_create endpoint\"
  }")

if [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "200" ]; then
    REMINDER_ID=$(cat /tmp/response.json | grep -o '"id":[0-9]*' | head -1 | cut -d: -f2)
    log_success "reminder_create - HTTP $HTTP_CODE, Created ID: $REMINDER_ID"
    ((TESTS_PASSED++))
else
    log_error "reminder_create - HTTP $HTTP_CODE"
    cat /tmp/response.json
    ((TESTS_FAILED++))
fi

# Test 3: debt_create
echo
echo "Test 3: debt_create"
echo "==================="
HTTP_CODE=$(curl -s -o /tmp/response.json -w "%{http_code}" -X POST \
  "${MONICA_API_URL}/debts" \
  -H "Authorization: Bearer ${MONICA_TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{
    \"contact_id\": ${CONTACT_ID},
    \"amount\": 100,
    \"status\": \"inprogress\",
    \"reason\": \"Validation test debt\"
  }")

if [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "200" ]; then
    DEBT_ID=$(cat /tmp/response.json | grep -o '"id":[0-9]*' | head -1 | cut -d: -f2)
    log_success "debt_create - HTTP $HTTP_CODE, Created ID: $DEBT_ID"
    ((TESTS_PASSED++))
else
    log_error "debt_create - HTTP $HTTP_CODE"
    cat /tmp/response.json
    ((TESTS_FAILED++))
fi

# Test 4: conversation_create
echo
echo "Test 4: conversation_create"
echo "============================"
HTTP_CODE=$(curl -s -o /tmp/response.json -w "%{http_code}" -X POST \
  "${MONICA_API_URL}/conversations" \
  -H "Authorization: Bearer ${MONICA_TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{
    \"contact_id\": ${CONTACT_ID},
    \"happened_at\": \"2026-02-08T10:00:00Z\"
  }")

if [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "200" ]; then
    CONV_ID=$(cat /tmp/response.json | grep -o '"id":[0-9]*' | head -1 | cut -d: -f2)
    log_success "conversation_create - HTTP $HTTP_CODE, Created ID: $CONV_ID"
    ((TESTS_PASSED++))
else
    log_error "conversation_create - HTTP $HTTP_CODE"
    cat /tmp/response.json
    ((TESTS_FAILED++))
fi

# Test 5: contact_field_create
echo
echo "Test 5: contact_field_create"
echo "============================="
HTTP_CODE=$(curl -s -o /tmp/response.json -w "%{http_code}" -X POST \
  "${MONICA_API_URL}/contacts/${CONTACT_ID}/contactfields" \
  -H "Authorization: Bearer ${MONICA_TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{
    \"contact_field_type_id\": 1,
    \"data\": \"validation@test.com\"
  }")

if [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "200" ]; then
    FIELD_ID=$(cat /tmp/response.json | grep -o '"id":[0-9]*' | head -1 | cut -d: -f2)
    log_success "contact_field_create - HTTP $HTTP_CODE, Created ID: $FIELD_ID"
    ((TESTS_PASSED++))
else
    log_error "contact_field_create - HTTP $HTTP_CODE"
    cat /tmp/response.json
    ((TESTS_FAILED++))
fi

# Summary
echo
echo "================================"
echo "Validation Summary"
echo "================================"
echo "Tests Passed: $TESTS_PASSED / 5"
echo "Tests Failed: $TESTS_FAILED / 5"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "ALL 5 ENDPOINTS WORKING! ✅"
    exit 0
else
    log_error "SOME ENDPOINTS FAILED! ❌"
    exit 1
fi
