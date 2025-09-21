#!/bin/bash

# Constitutional Test: Principle VI - MCP Response Content Visibility
# Tests that all MCP tool responses format data in the content field for Claude Desktop

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

echo "🏛️  Testing Principle VI: MCP Response Content Visibility"
echo "========================================================="

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

# Test 1: ContentFormatter utility exists
echo ""
echo "Test 1: ContentFormatter Utility"
echo "---------------------------------"
if [ -f "src/main/java/com/monicahq/mcp/util/ContentFormatter.java" ]; then
    test_pass "ContentFormatter utility class exists"
else
    test_fail "ContentFormatter utility class missing at src/main/java/com/monicahq/mcp/util/ContentFormatter.java"
fi

# Test 2: Services use content formatting
echo ""
echo "Test 2: Service Content Formatting"
echo "-----------------------------------"
SERVICE_COUNT=0
FORMATTED_COUNT=0

for service in ContactService ActivityService TaskService NoteService CallService TagService \
               ReminderService JournalEntryService ConversationService ConversationMessageService \
               ContactFieldService ContactTagService; do
    SERVICE_FILE="src/main/java/com/monicahq/mcp/service/${service}.java"
    if [ -f "$SERVICE_FILE" ]; then
        ((SERVICE_COUNT++))
        # Check if service formats content field
        if grep -q "formatContent\|ContentFormatter\|content.*=.*format" "$SERVICE_FILE"; then
            ((FORMATTED_COUNT++))
            test_pass "$service formats content field"
        else
            test_fail "$service does not format content field"
        fi
    fi
done

if [ "$SERVICE_COUNT" -eq 0 ]; then
    test_fail "No service files found"
elif [ "$FORMATTED_COUNT" -eq "$SERVICE_COUNT" ]; then
    info "All $SERVICE_COUNT services format content field"
else
    info "Only $FORMATTED_COUNT of $SERVICE_COUNT services format content field"
fi

# Test 3: Content field structure validation
echo ""
echo "Test 3: Content Field Structure"
echo "--------------------------------"
# Check for specific formatting patterns that ensure human readability
if [ -f "src/main/java/com/monicahq/mcp/util/ContentFormatter.java" ]; then
    if grep -q "StringBuilder\|String.format\|formatted" "src/main/java/com/monicahq/mcp/util/ContentFormatter.java"; then
        test_pass "ContentFormatter uses structured formatting"
    else
        test_fail "ContentFormatter lacks structured formatting methods"
    fi
else
    test_fail "Cannot test structure - ContentFormatter missing"
fi

# Test 4: Test coverage for content field
echo ""
echo "Test 4: Content Field Test Coverage"
echo "------------------------------------"
TEST_COUNT=0
CONTENT_TEST_COUNT=0

# Check contract tests for content field verification
if [ -d "src/test/java/com/monicahq/mcp/contract" ]; then
    for test_file in src/test/java/com/monicahq/mcp/contract/*Test.java; do
        if [ -f "$test_file" ]; then
            ((TEST_COUNT++))
            if grep -q "content\|getContent()\|\.content\|assertThat.*content" "$test_file"; then
                ((CONTENT_TEST_COUNT++))
            fi
        fi
    done
fi

if [ "$TEST_COUNT" -eq 0 ]; then
    test_fail "No contract tests found"
elif [ "$CONTENT_TEST_COUNT" -eq "$TEST_COUNT" ]; then
    test_pass "All $TEST_COUNT contract tests verify content field"
else
    test_fail "Only $CONTENT_TEST_COUNT of $TEST_COUNT tests verify content field"
fi

# Test 5: Response format validation
echo ""
echo "Test 5: Response Format Validation"
echo "-----------------------------------"
# Check if McpMessageHandler formats responses with content field
if [ -f "src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java" ]; then
    if grep -q "content.*=\|setContent\|response.*content" "src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java"; then
        test_pass "McpMessageHandler includes content field in responses"
    else
        test_fail "McpMessageHandler does not format content field in responses"
    fi
else
    test_fail "McpMessageHandler not found"
fi

# Test 6: List operations include pagination in content
echo ""
echo "Test 6: List Operation Content"
echo "-------------------------------"
LIST_OPS_FOUND=0
LIST_OPS_FORMATTED=0

for service in ContactService ActivityService TaskService NoteService; do
    SERVICE_FILE="src/main/java/com/monicahq/mcp/service/${service}.java"
    if [ -f "$SERVICE_FILE" ]; then
        if grep -q "public.*list\|List<" "$SERVICE_FILE"; then
            ((LIST_OPS_FOUND++))
            if grep -q "Total.*items\|Page.*of\|pagination" "$SERVICE_FILE"; then
                ((LIST_OPS_FORMATTED++))
            fi
        fi
    fi
done

if [ "$LIST_OPS_FOUND" -eq 0 ]; then
    test_fail "No list operations found"
elif [ "$LIST_OPS_FORMATTED" -eq "$LIST_OPS_FOUND" ]; then
    test_pass "All list operations include pagination info in content"
else
    test_fail "Only $LIST_OPS_FORMATTED of $LIST_OPS_FOUND list operations format pagination"
fi

# Test 7: No critical data hidden in data field
echo ""
echo "Test 7: Data Field Independence"
echo "--------------------------------"
# Ensure services don't rely on data field for Claude-needed information
if [ -d "src/main/java/com/monicahq/mcp/service" ]; then
    DATA_ONLY_COUNT=$(grep -r "data.*=.*[^content]" src/main/java/com/monicahq/mcp/service | \
                      grep -v "content" | wc -l)
    if [ "$DATA_ONLY_COUNT" -eq 0 ]; then
        test_pass "No services rely solely on data field"
    else
        test_fail "Found $DATA_ONLY_COUNT instances of data field without content field"
    fi
else
    test_fail "Service directory not found"
fi

# Summary
echo ""
echo "========================================================="
echo "Principle VI Compliance Summary"
echo "========================================================="

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}✅ FULLY COMPLIANT${NC} - All content visibility tests passed"
    echo "MCP responses are properly formatted for Claude Desktop"
else
    echo -e "${RED}❌ NON-COMPLIANT${NC} - $FAILURES of $TESTS tests failed"
    echo "Services must format all data in content field for Claude Desktop visibility"
    echo ""
    echo "Required actions:"
    echo "1. Create ContentFormatter utility class"
    echo "2. Update all 52 MCP operations to format content field"
    echo "3. Ensure tests verify content field completeness"
    echo "4. Remove reliance on data field for Claude-needed information"
fi

echo ""
echo "Tests Run: $TESTS"
echo "Failures: $FAILURES"
echo ""

exit $FAILURES