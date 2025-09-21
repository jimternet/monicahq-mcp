#!/bin/bash

# Contract Test: Contact Operations Content Field Formatting
# Tests that Contact entity operations format content field for Claude Desktop visibility

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

echo "📋 Testing Contact Operations Content Field Formatting"
echo "====================================================="

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

FAILURES=0
TESTS=0

test_pass() {
    echo -e "${GREEN}✅ PASS:${NC} $1"
    ((TESTS++))
}

test_fail() {
    echo -e "${RED}❌ FAIL:${NC} $1"
    ((FAILURES++))
    ((TESTS++))
}

info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

CONTACT_SERVICE="src/main/java/com/monicahq/mcp/service/ContactService.java"

# Test 1: ContactService exists
echo ""
echo "Test 1: ContactService Existence"
echo "---------------------------------"
if [ -f "$CONTACT_SERVICE" ]; then
    test_pass "ContactService.java exists"
else
    test_fail "ContactService.java not found at $CONTACT_SERVICE"
    echo "Cannot continue without ContactService"
    exit 1
fi

# Test 2: Contact create operation formats content field
echo ""
echo "Test 2: Contact Create Content Formatting"
echo "-------------------------------------------"
if grep -q "create.*Contact" "$CONTACT_SERVICE"; then
    if grep -A 20 "create.*Contact" "$CONTACT_SERVICE" | grep -q "content.*=\|formatContent\|ContentFormatter"; then
        test_pass "Contact create operation formats content field"
    else
        test_fail "Contact create operation does not format content field"
    fi
else
    test_fail "Contact create operation not found"
fi

# Test 3: Contact get operation formats content field
echo ""
echo "Test 3: Contact Get Content Formatting"
echo "---------------------------------------"
if grep -q "get.*Contact\|getContact" "$CONTACT_SERVICE"; then
    if grep -A 20 "get.*Contact\|getContact" "$CONTACT_SERVICE" | grep -q "content.*=\|formatContent\|ContentFormatter"; then
        test_pass "Contact get operation formats content field"
    else
        test_fail "Contact get operation does not format content field"
    fi
else
    test_fail "Contact get operation not found"
fi

# Test 4: Contact update operation formats content field
echo ""
echo "Test 4: Contact Update Content Formatting"
echo "------------------------------------------"
if grep -q "update.*Contact" "$CONTACT_SERVICE"; then
    if grep -A 20 "update.*Contact" "$CONTACT_SERVICE" | grep -q "content.*=\|formatContent\|ContentFormatter"; then
        test_pass "Contact update operation formats content field"
    else
        test_fail "Contact update operation does not format content field"
    fi
else
    test_fail "Contact update operation not found"
fi

# Test 5: Contact delete operation formats content field
echo ""
echo "Test 5: Contact Delete Content Formatting"
echo "------------------------------------------"
if grep -q "delete.*Contact" "$CONTACT_SERVICE"; then
    if grep -A 20 "delete.*Contact" "$CONTACT_SERVICE" | grep -q "content.*=\|formatContent\|ContentFormatter"; then
        test_pass "Contact delete operation formats content field"
    else
        test_fail "Contact delete operation does not format content field"
    fi
else
    test_fail "Contact delete operation not found"
fi

# Test 6: Contact list operation formats content field
echo ""
echo "Test 6: Contact List Content Formatting"
echo "----------------------------------------"
if grep -q "list.*Contact\|listContacts" "$CONTACT_SERVICE"; then
    if grep -A 20 "list.*Contact\|listContacts" "$CONTACT_SERVICE" | grep -q "content.*=\|formatContent\|ContentFormatter"; then
        test_pass "Contact list operation formats content field"
    else
        test_fail "Contact list operation does not format content field"
    fi
else
    test_fail "Contact list operation not found"
fi

# Test 7: Contact responses include structured data
echo ""
echo "Test 7: Contact Response Structure"
echo "-----------------------------------"
if grep -q "Contact ID:\|Name:\|Email:\|Phone:" "$CONTACT_SERVICE"; then
    test_pass "Contact responses include structured data fields"
else
    test_fail "Contact responses lack structured formatting"
fi

# Test 8: Contact list includes pagination info
echo ""
echo "Test 8: Contact List Pagination"
echo "--------------------------------"
if grep -q "Total.*contacts\|Page.*of\|showing.*of" "$CONTACT_SERVICE"; then
    test_pass "Contact list includes pagination information"
else
    test_fail "Contact list lacks pagination information in content"
fi

# Test 9: ContentFormatter usage for Contacts
echo ""
echo "Test 9: ContentFormatter Integration"
echo "------------------------------------"
if grep -q "ContentFormatter" "$CONTACT_SERVICE"; then
    if grep -q "formatContact\|format.*Contact" "$CONTACT_SERVICE"; then
        test_pass "ContactService uses ContentFormatter for formatting"
    else
        test_fail "ContactService imports ContentFormatter but doesn't use contact formatting"
    fi
else
    test_fail "ContactService does not use ContentFormatter utility"
fi

# Test 10: No data-only responses
echo ""
echo "Test 10: Data Field Independence"
echo "--------------------------------"
# Check that responses don't rely solely on data field
if grep -q "data.*=.*[^content]" "$CONTACT_SERVICE" && ! grep -q "content.*=" "$CONTACT_SERVICE"; then
    test_fail "Contact operations rely only on data field (violates Claude Desktop visibility)"
else
    test_pass "Contact operations do not rely solely on data field"
fi

# Summary
echo ""
echo "====================================================="
echo "Contact Content Formatting Test Summary"
echo "====================================================="

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}✅ ALL TESTS PASSED${NC} - Contact operations format content correctly"
    echo "Contact entity responses are ready for Claude Desktop visibility"
else
    echo -e "${RED}❌ $FAILURES of $TESTS tests FAILED${NC}"
    echo "Contact operations need content field formatting implementation"
    echo ""
    echo "Required actions for ContactService:"
    echo "1. Import ContentFormatter utility"
    echo "2. Format all operation responses in content field"
    echo "3. Include structured data (ID, Name, Email, Phone)"
    echo "4. Add pagination info for list operations"
    echo "5. Ensure Claude Desktop can access all data via content field"
fi

echo ""
echo "Tests Run: $TESTS"
echo "Failures: $FAILURES"
echo ""

exit $FAILURES