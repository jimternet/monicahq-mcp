#!/bin/bash

# Contract Test: Task Operations Content Field Formatting
# Tests that Task entity operations format content field for Claude Desktop visibility

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

echo "📋 Testing Task Operations Content Field Formatting"
echo "==================================================="

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

TASK_SERVICE="src/main/java/com/monicahq/mcp/service/TaskService.java"

# Test 1: TaskService exists
echo ""
echo "Test 1: TaskService Existence"
echo "------------------------------"
if [ -f "$TASK_SERVICE" ]; then
    test_pass "TaskService.java exists"
else
    test_fail "TaskService.java not found at $TASK_SERVICE"
    echo "Cannot continue without TaskService"
    exit 1
fi

# Test 2: Task create operation formats content field
echo ""
echo "Test 2: Task Create Content Formatting"
echo "---------------------------------------"
if grep -q "create.*Task" "$TASK_SERVICE" 2>/dev/null; then
    if grep -A 20 "create.*Task" "$TASK_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Task create operation formats content field"
    else
        test_fail "Task create operation does not format content field"
    fi
else
    test_fail "Task create operation not found"
fi

# Test 3: Task get operation formats content field
echo ""
echo "Test 3: Task Get Content Formatting"
echo "------------------------------------"
if grep -q "get.*Task\|getTask" "$TASK_SERVICE" 2>/dev/null; then
    if grep -A 20 "get.*Task\|getTask" "$TASK_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Task get operation formats content field"
    else
        test_fail "Task get operation does not format content field"
    fi
else
    test_fail "Task get operation not found"
fi

# Test 4: Task update operation formats content field
echo ""
echo "Test 4: Task Update Content Formatting"
echo "---------------------------------------"
if grep -q "update.*Task" "$TASK_SERVICE" 2>/dev/null; then
    if grep -A 20 "update.*Task" "$TASK_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Task update operation formats content field"
    else
        test_fail "Task update operation does not format content field"
    fi
else
    test_fail "Task update operation not found"
fi

# Test 5: Task delete operation formats content field
echo ""
echo "Test 5: Task Delete Content Formatting"
echo "---------------------------------------"
if grep -q "delete.*Task" "$TASK_SERVICE" 2>/dev/null; then
    if grep -A 20 "delete.*Task" "$TASK_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Task delete operation formats content field"
    else
        test_fail "Task delete operation does not format content field"
    fi
else
    test_fail "Task delete operation not found"
fi

# Test 6: Task list operation formats content field
echo ""
echo "Test 6: Task List Content Formatting"
echo "-------------------------------------"
if grep -q "list.*Task\|listTasks" "$TASK_SERVICE" 2>/dev/null; then
    if grep -A 20 "list.*Task\|listTasks" "$TASK_SERVICE" 2>/dev/null | grep -q "content.*=\|formatContent\|ContentFormatter" 2>/dev/null; then
        test_pass "Task list operation formats content field"
    else
        test_fail "Task list operation does not format content field"
    fi
else
    test_fail "Task list operation not found"
fi

# Test 7: Task responses include structured data
echo ""
echo "Test 7: Task Response Structure"
echo "--------------------------------"
if grep -q "Task ID:\|Title:\|Description:\|Status:\|Due Date:" "$TASK_SERVICE" 2>/dev/null; then
    test_pass "Task responses include structured data fields"
else
    test_fail "Task responses lack structured formatting"
fi

# Test 8: Task list includes pagination info
echo ""
echo "Test 8: Task List Pagination"
echo "-----------------------------"
if grep -q "Total.*tasks\|Page.*of\|showing.*of" "$TASK_SERVICE" 2>/dev/null; then
    test_pass "Task list includes pagination information"
else
    test_fail "Task list lacks pagination information in content"
fi

# Test 9: ContentFormatter usage for Tasks
echo ""
echo "Test 9: ContentFormatter Integration"
echo "------------------------------------"
if grep -q "ContentFormatter" "$TASK_SERVICE" 2>/dev/null; then
    if grep -q "formatTask\|format.*Task" "$TASK_SERVICE" 2>/dev/null; then
        test_pass "TaskService uses ContentFormatter for formatting"
    else
        test_fail "TaskService imports ContentFormatter but doesn't use task formatting"
    fi
else
    test_fail "TaskService does not use ContentFormatter utility"
fi

# Test 10: No data-only responses
echo ""
echo "Test 10: Data Field Independence"
echo "--------------------------------"
# Check that responses don't rely solely on data field
if grep -q "data.*=.*[^content]" "$TASK_SERVICE" 2>/dev/null && ! grep -q "content.*=" "$TASK_SERVICE" 2>/dev/null; then
    test_fail "Task operations rely only on data field (violates Claude Desktop visibility)"
else
    test_pass "Task operations do not rely solely on data field"
fi

# Summary
echo ""
echo "==================================================="
echo "Task Content Formatting Test Summary"
echo "==================================================="

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}✅ ALL TESTS PASSED${NC} - Task operations format content correctly"
    echo "Task entity responses are ready for Claude Desktop visibility"
else
    echo -e "${RED}❌ $FAILURES of $TESTS tests FAILED${NC}"
    echo "Task operations need content field formatting implementation"
    echo ""
    echo "Required actions for TaskService:"
    echo "1. Import ContentFormatter utility"
    echo "2. Format all operation responses in content field"
    echo "3. Include structured data (Task ID, Title, Description, Status, Due Date)"
    echo "4. Add pagination info for list operations"
    echo "5. Ensure Claude Desktop can access all data via content field"
fi

echo ""
echo "Tests Run: $TESTS"
echo "Failures: $FAILURES"
echo ""

exit $FAILURES