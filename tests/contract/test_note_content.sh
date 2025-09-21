#!/bin/bash

# Contract Test: Note Operations Content Field Formatting
# Tests that Note entity operations format content field for Claude Desktop visibility

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

echo "📋 Testing Note Operations Content Field Formatting"
echo "===================================================="

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

NOTE_SERVICE="src/main/java/com/monicahq/mcp/service/NoteService.java"

# Test 1: NoteService exists
echo ""
echo "Test 1: NoteService Existence"
echo "------------------------------"
if [ -f "$NOTE_SERVICE" ]; then
    test_pass "NoteService.java exists"
else
    test_fail "NoteService.java not found at $NOTE_SERVICE"
    echo "Cannot continue without NoteService"
    exit 1
fi

# Test 2: Note create operation formats content field
echo ""
echo "Test 2: Note Create Content Formatting"
echo "---------------------------------------"
if grep -q "create.*Note" "$NOTE_SERVICE" 2>/dev/null; then
    if grep -A 20 "create.*Note" "$NOTE_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Note create operation formats content field"
    else
        test_fail "Note create operation does not format content field"
    fi
else
    test_fail "Note create operation not found"
fi

# Test 3: Note get operation formats content field
echo ""
echo "Test 3: Note Get Content Formatting"
echo "------------------------------------"
if grep -q "get.*Note\|getNote" "$NOTE_SERVICE" 2>/dev/null; then
    if grep -A 20 "get.*Note\|getNote" "$NOTE_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Note get operation formats content field"
    else
        test_fail "Note get operation does not format content field"
    fi
else
    test_fail "Note get operation not found"
fi

# Test 4: Note update operation formats content field
echo ""
echo "Test 4: Note Update Content Formatting"
echo "---------------------------------------"
if grep -q "update.*Note" "$NOTE_SERVICE" 2>/dev/null; then
    if grep -A 20 "update.*Note" "$NOTE_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Note update operation formats content field"
    else
        test_fail "Note update operation does not format content field"
    fi
else
    test_fail "Note update operation not found"
fi

# Test 5: Note delete operation formats content field
echo ""
echo "Test 5: Note Delete Content Formatting"
echo "---------------------------------------"
if grep -q "delete.*Note" "$NOTE_SERVICE" 2>/dev/null; then
    if grep -A 20 "delete.*Note" "$NOTE_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Note delete operation formats content field"
    else
        test_fail "Note delete operation does not format content field"
    fi
else
    test_fail "Note delete operation not found"
fi

# Test 6: Note list operation formats content field
echo ""
echo "Test 6: Note List Content Formatting"
echo "-------------------------------------"
if grep -q "list.*Note\|listNotes" "$NOTE_SERVICE" 2>/dev/null; then
    if grep -A 20 "list.*Note\|listNotes" "$NOTE_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Note list operation formats content field"
    else
        test_fail "Note list operation does not format content field"
    fi
else
    test_fail "Note list operation not found"
fi

# Test 7: Note responses include structured data
echo ""
echo "Test 7: Note Response Structure"
echo "--------------------------------"
if grep -q "Note ID:\|Title:\|Body:\|Created Date:\|Updated Date:" "$NOTE_SERVICE" 2>/dev/null; then
    test_pass "Note responses include structured data fields"
else
    test_fail "Note responses lack structured formatting"
fi

# Test 8: Note list includes pagination info
echo ""
echo "Test 8: Note List Pagination"
echo "-----------------------------"
if grep -q "Total.*notes\|Page.*of\|showing.*of" "$NOTE_SERVICE" 2>/dev/null; then
    test_pass "Note list includes pagination information"
else
    test_fail "Note list lacks pagination information in content"
fi

# Test 9: ContentFormatter usage for Notes
echo ""
echo "Test 9: ContentFormatter Integration"
echo "-------------------------------------"
if grep -q "ContentFormatter" "$NOTE_SERVICE" 2>/dev/null; then
    if grep -q "formatNote\|format.*Note" "$NOTE_SERVICE" 2>/dev/null; then
        test_pass "NoteService uses ContentFormatter for formatting"
    else
        test_fail "NoteService imports ContentFormatter but doesn't use note formatting"
    fi
else
    test_fail "NoteService does not use ContentFormatter utility"
fi

# Test 10: No data-only responses
echo ""
echo "Test 10: Data Field Independence"
echo "---------------------------------"
# Check that responses don't rely solely on data field
if grep -q "data.*=.*[^content]" "$NOTE_SERVICE" 2>/dev/null && ! grep -q "content.*=" "$NOTE_SERVICE" 2>/dev/null; then
    test_fail "Note operations rely only on data field (violates Claude Desktop visibility)"
else
    test_pass "Note operations do not rely solely on data field"
fi

# Summary
echo ""
echo "======================================================"
echo "Note Content Formatting Test Summary"
echo "======================================================"

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}✅ ALL TESTS PASSED${NC} - Note operations format content correctly"
    echo "Note entity responses are ready for Claude Desktop visibility"
else
    echo -e "${RED}❌ $FAILURES of $TESTS tests FAILED${NC}"
    echo "Note operations need content field formatting implementation"
    echo ""
    echo "Required actions for NoteService:"
    echo "1. Import ContentFormatter utility"
    echo "2. Format all operation responses in content field"
    echo "3. Include structured data (Note ID, Title, Body, Created Date, Updated Date)"
    echo "4. Add pagination info for list operations"
    echo "5. Ensure Claude Desktop can access all data via content field"
fi

echo ""
echo "Tests Run: $TESTS"
echo "Failures: $FAILURES"
echo ""

exit $FAILURES