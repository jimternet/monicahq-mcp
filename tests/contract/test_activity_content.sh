#!/bin/bash

# Contract Test: Activity Operations Content Field Formatting
# Tests that Activity entity operations format content field for Claude Desktop visibility

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

echo "📋 Testing Activity Operations Content Field Formatting"
echo "======================================================"

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

ACTIVITY_SERVICE="src/main/java/com/monicahq/mcp/service/ActivityService.java"

# Test 1: ActivityService exists
echo ""
echo "Test 1: ActivityService Existence"
echo "----------------------------------"
if [ -f "$ACTIVITY_SERVICE" ]; then
    test_pass "ActivityService.java exists"
else
    test_fail "ActivityService.java not found at $ACTIVITY_SERVICE"
    echo "Cannot continue without ActivityService"
    exit 1
fi

# Test 2: Activity create operation formats content field
echo ""
echo "Test 2: Activity Create Content Formatting"
echo "--------------------------------------------"
if grep -q "create.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null; then
    if grep -A 20 "create.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Activity create operation formats content field"
    else
        test_fail "Activity create operation does not format content field"
    fi
else
    test_fail "Activity create operation not found"
fi

# Test 3: Activity get operation formats content field
echo ""
echo "Test 3: Activity Get Content Formatting"
echo "----------------------------------------"
if grep -q "get.*Activity\|getActivity" "$ACTIVITY_SERVICE" 2>/dev/null; then
    if grep -A 20 "get.*Activity\|getActivity" "$ACTIVITY_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Activity get operation formats content field"
    else
        test_fail "Activity get operation does not format content field"
    fi
else
    test_fail "Activity get operation not found"
fi

# Test 4: Activity update operation formats content field
echo ""
echo "Test 4: Activity Update Content Formatting"
echo "-------------------------------------------"
if grep -q "update.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null; then
    if grep -A 20 "update.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Activity update operation formats content field"
    else
        test_fail "Activity update operation does not format content field"
    fi
else
    test_fail "Activity update operation not found"
fi

# Test 5: Activity delete operation formats content field
echo ""
echo "Test 5: Activity Delete Content Formatting"
echo "-------------------------------------------"
if grep -q "delete.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null; then
    if grep -A 20 "delete.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Activity delete operation formats content field"
    else
        test_fail "Activity delete operation does not format content field"
    fi
else
    test_fail "Activity delete operation not found"
fi

# Test 6: Activity list operation formats content field
echo ""
echo "Test 6: Activity List Content Formatting"
echo "-----------------------------------------"
if grep -q "list.*Activity\|listActivities" "$ACTIVITY_SERVICE" 2>/dev/null; then
    if grep -A 20 "list.*Activity\|listActivities" "$ACTIVITY_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Activity list operation formats content field"
    else
        test_fail "Activity list operation does not format content field"
    fi
else
    test_fail "Activity list operation not found"
fi

# Test 7: Activity responses include structured data
echo ""
echo "Test 7: Activity Response Structure"
echo "------------------------------------"
if grep -q "Activity ID:\|Type:\|Description:\|Date:" "$ACTIVITY_SERVICE" 2>/dev/null; then
    test_pass "Activity responses include structured data fields"
else
    test_fail "Activity responses lack structured formatting"
fi

# Test 8: Activity list includes pagination info
echo ""
echo "Test 8: Activity List Pagination"
echo "---------------------------------"
if grep -q "Total.*activities\|Page.*of\|showing.*of" "$ACTIVITY_SERVICE" 2>/dev/null; then
    test_pass "Activity list includes pagination information"
else
    test_fail "Activity list lacks pagination information in content"
fi

# Test 9: ContentFormatter usage for Activities
echo ""
echo "Test 9: ContentFormatter Integration"
echo "-------------------------------------"
if grep -q "ContentFormatter" "$ACTIVITY_SERVICE" 2>/dev/null; then
    if grep -q "formatActivity\|format.*Activity" "$ACTIVITY_SERVICE" 2>/dev/null; then
        test_pass "ActivityService uses ContentFormatter for formatting"
    else
        test_fail "ActivityService imports ContentFormatter but doesn't use activity formatting"
    fi
else
    test_fail "ActivityService does not use ContentFormatter utility"
fi

# Test 10: No data-only responses
echo ""
echo "Test 10: Data Field Independence"
echo "---------------------------------"
# Check that responses don't rely solely on data field
if grep -q "data.*=.*[^content]" "$ACTIVITY_SERVICE" 2>/dev/null && ! grep -q "content.*=" "$ACTIVITY_SERVICE" 2>/dev/null; then
    test_fail "Activity operations rely only on data field (violates Claude Desktop visibility)"
else
    test_pass "Activity operations do not rely solely on data field"
fi

# Summary
echo ""
echo "======================================================"
echo "Activity Content Formatting Test Summary"
echo "======================================================"

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}✅ ALL TESTS PASSED${NC} - Activity operations format content correctly"
    echo "Activity entity responses are ready for Claude Desktop visibility"
else
    echo -e "${RED}❌ $FAILURES of $TESTS tests FAILED${NC}"
    echo "Activity operations need content field formatting implementation"
    echo ""
    echo "Required actions for ActivityService:"
    echo "1. Import ContentFormatter utility"
    echo "2. Format all operation responses in content field"
    echo "3. Include structured data (Activity ID, Type, Description, Date)"
    echo "4. Add pagination info for list operations"
    echo "5. Ensure Claude Desktop can access all data via content field"
fi

echo ""
echo "Tests Run: $TESTS"
echo "Failures: $FAILURES"
echo ""

exit $FAILURES