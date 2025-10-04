#!/bin/bash

# T011: Shell script test for all 122 tools discovery
# Verifies all tools are listed and callable via MCP STDIO protocol
# 
# This test MUST FAIL initially (RED phase of TDD).
# Not all 122 tools are properly registered and discoverable yet.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test configuration
TEST_NAME="All Tools Discovery Test"
TIMEOUT_SECONDS=60
MCP_SERVER_JAR="$PROJECT_ROOT/build/libs/monicahq-mcp-1.0.0.jar"
TEMP_DIR="/tmp/mcp-tools-discovery-test-$$"
SERVER_PID=""

# Expected 122 tools across 23 entity types
EXPECTED_TOOLS=(
    # Contact operations (5)
    "contact_create" "contact_get" "contact_update" "contact_delete" "contact_list"
    
    # Activity operations (5)
    "activity_create" "activity_get" "activity_update" "activity_delete" "activity_list"
    
    # Call operations (5)
    "call_create" "call_get" "call_update" "call_delete" "call_list"
    
    # Note operations (5)
    "note_create" "note_get" "note_update" "note_delete" "note_list"
    
    # Task operations (5)
    "task_create" "task_get" "task_update" "task_delete" "task_list"
    
    # Tag operations (5)
    "tag_create" "tag_get" "tag_update" "tag_delete" "tag_list"
    
    # Reminder operations (5)
    "reminder_create" "reminder_get" "reminder_update" "reminder_delete" "reminder_list"
    
    # Journal Entry operations (5)
    "journal_entry_create" "journal_entry_get" "journal_entry_update" "journal_entry_delete" "journal_entry_list"
    
    # Conversation operations (5)
    "conversation_create" "conversation_get" "conversation_update" "conversation_delete" "conversation_list"
    
    # Conversation Message operations (5)
    "conversation_message_create" "conversation_message_get" "conversation_message_update" "conversation_message_delete" "conversation_message_list"
    
    # Contact Field operations (5)
    "contact_field_create" "contact_field_get" "contact_field_update" "contact_field_delete" "contact_field_list"
    
    # Contact Tag operations (3)
    "contact_tag_add" "contact_tag_remove" "contact_tag_list"
    
    # Relationship operations (5)
    "relationship_create" "relationship_get" "relationship_update" "relationship_delete" "relationship_list"
    
    # Company operations (5)
    "company_create" "company_get" "company_update" "company_delete" "company_list"
    
    # Relationship Type operations (3)
    "relationship_type_get" "relationship_type_list" "relationship_type_search"
    
    # Relationship Type Group operations (2)
    "relationship_type_group_list" "relationship_type_group_search"
    
    # Debt operations (5)
    "debt_create" "debt_get" "debt_update" "debt_delete" "debt_list"
    
    # Document operations (5)
    "document_create" "document_get" "document_update" "document_delete" "document_list"
    
    # Photo operations (5)
    "photo_create" "photo_get" "photo_update" "photo_delete" "photo_list"
    
    # Gift operations (5)
    "gift_create" "gift_get" "gift_update" "gift_delete" "gift_list"
    
    # Audit Log operations (2)
    "audit_log_get" "audit_log_list"
    
    # Country operations (2)
    "country_list" "country_search"
    
    # Currency operations (2)
    "currency_list" "currency_search"
)

# Test counters
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0
TOOLS_FOUND=0
TOOLS_MISSING=0
TOOLS_CALLABLE=0
TOOLS_NOT_CALLABLE=0

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

increment_test() {
    ((TESTS_TOTAL++))
}

pass_test() {
    ((TESTS_PASSED++))
    log_success "$1"
}

fail_test() {
    ((TESTS_FAILED++))
    log_error "$1"
}

cleanup() {
    log_info "Cleaning up test environment..."
    
    if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        log_info "Stopping MCP server (PID: $SERVER_PID)"
        kill "$SERVER_PID" 2>/dev/null || true
        sleep 2
        kill -9 "$SERVER_PID" 2>/dev/null || true
    fi
    
    if [ -d "$TEMP_DIR" ]; then
        rm -rf "$TEMP_DIR"
    fi
}

trap cleanup EXIT

setup_test_environment() {
    log_info "Setting up test environment for $TEST_NAME"
    
    mkdir -p "$TEMP_DIR"
    
    if [ ! -f "$MCP_SERVER_JAR" ]; then
        log_error "MCP server JAR not found: $MCP_SERVER_JAR"
        log_info "Please run: ./gradlew build"
        return 1
    fi
    
    export MONICA_API_URL="https://test.monica.example.com/api"
    export MONICA_API_TOKEN="test-token-12345"
    export MCP_STDIO_MODE="true"
    export SPRING_PROFILES_ACTIVE="test"
    
    log_success "Test environment setup complete"
    return 0
}

start_mcp_server() {
    log_info "Starting MCP server for tools discovery..."
    
    # This will FAIL because STDIO mode isn't properly implemented
    java -jar "$MCP_SERVER_JAR" \
        -Dspring.profiles.active=test \
        -Dspring.main.banner-mode=off \
        -Dlogging.level.com.monicahq.mcp=INFO \
        -Dlogging.level.root=WARN \
        > "$TEMP_DIR/server_stdout.log" \
        2> "$TEMP_DIR/server_stderr.log" &
    
    SERVER_PID=$!
    
    log_info "MCP server started with PID: $SERVER_PID"
    
    local attempts=0
    local max_attempts=15
    
    while [ $attempts -lt $max_attempts ]; do
        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
            log_error "MCP server process died"
            if [ -f "$TEMP_DIR/server_stderr.log" ]; then
                log_error "Server stderr output:"
                cat "$TEMP_DIR/server_stderr.log"
            fi
            return 1
        fi
        
        if grep -q "MCP stdio server ready" "$TEMP_DIR/server_stderr.log" 2>/dev/null; then
            log_success "MCP server is ready"
            return 0
        fi
        
        sleep 2
        ((attempts++))
        log_info "Waiting for server to be ready... ($attempts/$max_attempts)"
    done
    
    log_error "MCP server did not become ready within $((max_attempts * 2)) seconds"
    return 1
}

send_mcp_request() {
    local request="$1"
    local timeout="${2:-10}"
    
    echo "$request" | timeout "${timeout}s" nc localhost 8080 2>/dev/null || {
        echo "$request" | timeout "${timeout}s" java -cp "$PROJECT_ROOT/build/classes/java/test" \
            com.monicahq.mcp.util.StdioTestClient 2>/dev/null || true
    }
}

initialize_mcp_connection() {
    log_info "Initializing MCP connection..."
    
    increment_test
    
    local init_request='{
        "jsonrpc": "2.0",
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "tools-discovery-test",
                "version": "1.0.0"
            }
        },
        "id": 1
    }'
    
    local response
    response=$(send_mcp_request "$init_request" 15)
    
    if [ -z "$response" ]; then
        fail_test "No response to initialize request"
        return 1
    fi
    
    if echo "$response" | jq -e '.result' >/dev/null 2>&1; then
        pass_test "MCP connection initialized successfully"
        return 0
    else
        fail_test "Initialize request failed: $response"
        return 1
    fi
}

discover_all_tools() {
    log_info "Discovering all available tools..."
    
    increment_test
    
    local tools_request='{
        "jsonrpc": "2.0",
        "method": "tools/list",
        "params": {},
        "id": 2
    }'
    
    local response
    response=$(send_mcp_request "$tools_request" 15)
    
    if [ -z "$response" ]; then
        fail_test "No response to tools/list request"
        return 1
    fi
    
    # Save response for analysis
    echo "$response" > "$TEMP_DIR/tools_list_response.json"
    
    # Parse tools from response
    local tools_array
    tools_array=$(echo "$response" | jq -r '.result.tools[]?.name // empty' 2>/dev/null || true)
    
    if [ -z "$tools_array" ]; then
        fail_test "No tools found in response"
        return 1
    fi
    
    # Save discovered tools list
    echo "$tools_array" > "$TEMP_DIR/discovered_tools.txt"
    local discovered_count
    discovered_count=$(echo "$tools_array" | wc -l)
    
    log_info "Discovered $discovered_count tools"
    
    # This will FAIL because not all 122 tools are implemented yet
    if [ "$discovered_count" -eq 122 ]; then
        pass_test "All 122 expected tools discovered"
    else
        fail_test "Expected 122 tools, but found $discovered_count"
    fi
    
    return 0
}

verify_expected_tools() {
    log_info "Verifying all expected tools are present..."
    
    if [ ! -f "$TEMP_DIR/discovered_tools.txt" ]; then
        log_error "No discovered tools list found"
        return 1
    fi
    
    local missing_tools=()
    local found_tools=()
    
    for tool in "${EXPECTED_TOOLS[@]}"; do
        increment_test
        
        if grep -Fxq "$tool" "$TEMP_DIR/discovered_tools.txt"; then
            ((TOOLS_FOUND++))
            found_tools+=("$tool")
            pass_test "Tool found: $tool"
        else
            ((TOOLS_MISSING++))
            missing_tools+=("$tool")
            fail_test "Tool missing: $tool"
        fi
    done
    
    # Save results
    printf '%s\n' "${found_tools[@]}" > "$TEMP_DIR/found_tools.txt"
    printf '%s\n' "${missing_tools[@]}" > "$TEMP_DIR/missing_tools.txt"
    
    log_info "Tools verification complete: Found $TOOLS_FOUND, Missing $TOOLS_MISSING"
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        log_warning "Missing tools:"
        for tool in "${missing_tools[@]}"; do
            echo "  - $tool"
        done
    fi
    
    return 0
}

test_tool_schemas() {
    log_info "Testing tool schemas and descriptions..."
    
    if [ ! -f "$TEMP_DIR/tools_list_response.json" ]; then
        log_error "No tools list response found"
        return 1
    fi
    
    local tools_with_schemas=0
    local tools_without_schemas=0
    local tools_with_descriptions=0
    local tools_without_descriptions=0
    
    # Check each tool for required schema elements
    local tool_names
    tool_names=$(jq -r '.result.tools[]?.name // empty' "$TEMP_DIR/tools_list_response.json" 2>/dev/null || true)
    
    while IFS= read -r tool_name; do
        if [ -n "$tool_name" ]; then
            increment_test
            
            # Check for input schema
            local has_schema
            has_schema=$(jq -r ".result.tools[] | select(.name == \"$tool_name\") | has(\"inputSchema\")" "$TEMP_DIR/tools_list_response.json" 2>/dev/null || echo "false")
            
            if [ "$has_schema" = "true" ]; then
                ((tools_with_schemas++))
                pass_test "Tool $tool_name has input schema"
            else
                ((tools_without_schemas++))
                fail_test "Tool $tool_name missing input schema"
            fi
            
            # Check for description
            increment_test
            local description
            description=$(jq -r ".result.tools[] | select(.name == \"$tool_name\") | .description // empty" "$TEMP_DIR/tools_list_response.json" 2>/dev/null || true)
            
            if [ -n "$description" ] && [ "$description" != "null" ]; then
                ((tools_with_descriptions++))
                pass_test "Tool $tool_name has description"
            else
                ((tools_without_descriptions++))
                fail_test "Tool $tool_name missing description"
            fi
        fi
    done <<< "$tool_names"
    
    log_info "Schema verification: $tools_with_schemas with schemas, $tools_without_schemas without"
    log_info "Description verification: $tools_with_descriptions with descriptions, $tools_without_descriptions without"
    
    # This will FAIL because tool schemas aren't properly defined yet
    if [ $tools_without_schemas -eq 0 ] && [ $tools_without_descriptions -eq 0 ]; then
        pass_test "All tools have proper schemas and descriptions"
    else
        fail_test "Some tools missing schemas or descriptions"
    fi
    
    return 0
}

test_tool_callability() {
    log_info "Testing tool callability with sample requests..."
    
    # Test a subset of tools to verify they're callable
    local test_tools=("contact_list" "activity_list" "note_list" "task_list" "tag_list")
    
    for tool in "${test_tools[@]}"; do
        increment_test
        
        log_info "Testing callability of tool: $tool"
        
        local tool_call_request='{
            "jsonrpc": "2.0",
            "method": "tools/call",
            "params": {
                "name": "'$tool'",
                "arguments": {}
            },
            "id": '$((100 + TOOLS_CALLABLE + TOOLS_NOT_CALLABLE))'
        }'
        
        local response
        response=$(send_mcp_request "$tool_call_request" 20)
        
        if [ -z "$response" ]; then
            ((TOOLS_NOT_CALLABLE++))
            fail_test "Tool $tool: No response"
            continue
        fi
        
        # Check if response indicates success or proper error
        if echo "$response" | jq -e '.result' >/dev/null 2>&1; then
            ((TOOLS_CALLABLE++))
            pass_test "Tool $tool: Callable (returned result)"
        elif echo "$response" | jq -e '.error.code' >/dev/null 2>&1; then
            local error_code
            error_code=$(echo "$response" | jq -r '.error.code')
            
            # Some errors are acceptable (e.g., authentication errors in test mode)
            if [ "$error_code" = "-32602" ] || [ "$error_code" = "-32603" ]; then
                ((TOOLS_CALLABLE++))
                pass_test "Tool $tool: Callable (returned expected error)"
            else
                ((TOOLS_NOT_CALLABLE++))
                fail_test "Tool $tool: Unexpected error code $error_code"
            fi
        else
            ((TOOLS_NOT_CALLABLE++))
            fail_test "Tool $tool: Invalid response format"
        fi
    done
    
    log_info "Callability test complete: $TOOLS_CALLABLE callable, $TOOLS_NOT_CALLABLE not callable"
    
    return 0
}

test_crud_operation_completeness() {
    log_info "Testing CRUD operation completeness for entity types..."
    
    local crud_entities=(
        "contact" "activity" "call" "note" "task" "tag" "reminder"
        "journal_entry" "conversation" "conversation_message" "contact_field"
        "relationship" "company" "debt" "document" "photo" "gift"
    )
    
    local complete_entities=0
    local incomplete_entities=0
    
    for entity in "${crud_entities[@]}"; do
        increment_test
        
        local operations=("create" "get" "update" "delete" "list")
        local found_operations=0
        
        for operation in "${operations[@]}"; do
            local tool_name="${entity}_${operation}"
            
            if grep -Fxq "$tool_name" "$TEMP_DIR/discovered_tools.txt"; then
                ((found_operations++))
            fi
        done
        
        if [ $found_operations -eq 5 ]; then
            ((complete_entities++))
            pass_test "Entity $entity: Complete CRUD operations (5/5)"
        else
            ((incomplete_entities++))
            fail_test "Entity $entity: Incomplete CRUD operations ($found_operations/5)"
        fi
    done
    
    log_info "CRUD completeness: $complete_entities complete entities, $incomplete_entities incomplete"
    
    # This will FAIL because not all CRUD operations are implemented yet
    if [ $incomplete_entities -eq 0 ]; then
        pass_test "All entities have complete CRUD operations"
    else
        fail_test "Some entities missing CRUD operations"
    fi
    
    return 0
}

test_special_operations() {
    log_info "Testing special operations beyond CRUD..."
    
    local special_operations=(
        "contact_tag_add" "contact_tag_remove"
        "relationship_type_search" "relationship_type_group_search"
        "country_search" "currency_search"
        "audit_log_get" "audit_log_list"
    )
    
    local found_special=0
    local missing_special=0
    
    for operation in "${special_operations[@]}"; do
        increment_test
        
        if grep -Fxq "$operation" "$TEMP_DIR/discovered_tools.txt"; then
            ((found_special++))
            pass_test "Special operation found: $operation"
        else
            ((missing_special++))
            fail_test "Special operation missing: $operation"
        fi
    done
    
    log_info "Special operations: $found_special found, $missing_special missing"
    
    # This will FAIL because special operations aren't all implemented yet
    if [ $missing_special -eq 0 ]; then
        pass_test "All special operations available"
    else
        fail_test "Some special operations missing"
    fi
    
    return 0
}

generate_detailed_report() {
    log_info "Generating detailed tools discovery report..."
    
    local report_file="$TEMP_DIR/tools_discovery_report.json"
    
    cat > "$report_file" << EOF
{
    "testName": "$TEST_NAME",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "results": {
        "total_tests": $TESTS_TOTAL,
        "tests_passed": $TESTS_PASSED,
        "tests_failed": $TESTS_FAILED,
        "success_rate": $(echo "scale=2; $TESTS_PASSED * 100 / $TESTS_TOTAL" | bc -l)
    },
    "tools_analysis": {
        "expected_total": ${#EXPECTED_TOOLS[@]},
        "tools_found": $TOOLS_FOUND,
        "tools_missing": $TOOLS_MISSING,
        "coverage_rate": $(echo "scale=2; $TOOLS_FOUND * 100 / ${#EXPECTED_TOOLS[@]}" | bc -l),
        "tools_callable": $TOOLS_CALLABLE,
        "tools_not_callable": $TOOLS_NOT_CALLABLE
    },
    "discovered_tools": $(cat "$TEMP_DIR/discovered_tools.txt" 2>/dev/null | jq -R . | jq -s . || echo "[]"),
    "missing_tools": $(cat "$TEMP_DIR/missing_tools.txt" 2>/dev/null | jq -R . | jq -s . || echo "[]"),
    "environment": {
        "java_version": "$(java -version 2>&1 | head -n1)",
        "jar_file": "$MCP_SERVER_JAR",
        "temp_dir": "$TEMP_DIR"
    }
}
EOF
    
    log_info "Detailed report saved to: $report_file"
    
    # Copy to project docs
    local docs_dir="$PROJECT_ROOT/validation/docs"
    mkdir -p "$docs_dir"
    cp "$report_file" "$docs_dir/tools-discovery-test-$(date +%Y%m%d-%H%M%S).json"
    
    # Generate CSV summary for easy analysis
    local csv_file="$TEMP_DIR/tools_summary.csv"
    echo "Tool,Found,Callable,Category" > "$csv_file"
    
    for tool in "${EXPECTED_TOOLS[@]}"; do
        local found="No"
        local callable="Unknown"
        local category
        category=$(echo "$tool" | cut -d'_' -f1)
        
        if grep -Fxq "$tool" "$TEMP_DIR/discovered_tools.txt"; then
            found="Yes"
        fi
        
        echo "$tool,$found,$callable,$category" >> "$csv_file"
    done
    
    cp "$csv_file" "$docs_dir/tools-summary-$(date +%Y%m%d-%H%M%S).csv"
}

print_comprehensive_summary() {
    echo
    echo "=========================================================="
    echo "             ALL TOOLS DISCOVERY TEST SUMMARY"
    echo "=========================================================="
    echo
    echo "Test Results:"
    echo "  Total Tests:     $TESTS_TOTAL"
    echo "  Passed:          $TESTS_PASSED"
    echo "  Failed:          $TESTS_FAILED"
    echo "  Success Rate:    $(echo "scale=1; $TESTS_PASSED * 100 / $TESTS_TOTAL" | bc -l)%"
    echo
    echo "Tools Analysis:"
    echo "  Expected Total:  ${#EXPECTED_TOOLS[@]}"
    echo "  Tools Found:     $TOOLS_FOUND"
    echo "  Tools Missing:   $TOOLS_MISSING"
    echo "  Coverage Rate:   $(echo "scale=1; $TOOLS_FOUND * 100 / ${#EXPECTED_TOOLS[@]}" | bc -l)%"
    echo
    echo "Callability Test:"
    echo "  Tools Callable:     $TOOLS_CALLABLE"
    echo "  Tools Not Callable: $TOOLS_NOT_CALLABLE"
    echo
    
    if [ $TOOLS_MISSING -eq 0 ] && [ $TESTS_FAILED -eq 0 ]; then
        log_success "All 122 tools discovered and properly configured!"
        echo
        return 0
    else
        log_error "Tools discovery incomplete - implementation needed."
        echo
        echo "This is expected for TDD Phase 3.2 - these tests should FAIL initially."
        echo
        echo "Implementation tasks:"
        echo "1. Register all 122 tools in McpToolRegistry"
        echo "2. Implement missing entity service operations"
        echo "3. Add proper input schemas for all tools"
        echo "4. Implement special operations (search, add/remove, audit)"
        echo "5. Complete CRUD operations for all entity types"
        echo "6. Add tool descriptions and documentation"
        echo
        
        if [ $TOOLS_MISSING -gt 0 ]; then
            echo "Missing tools (showing first 10):"
            head -10 "$TEMP_DIR/missing_tools.txt" 2>/dev/null | while read -r tool; do
                echo "  - $tool"
            done
            if [ $TOOLS_MISSING -gt 10 ]; then
                echo "  ... and $(($TOOLS_MISSING - 10)) more"
            fi
            echo
        fi
        
        return 1
    fi
}

# Main execution
main() {
    log_info "Starting $TEST_NAME"
    echo
    
    if ! setup_test_environment; then
        log_error "Failed to setup test environment"
        exit 1
    fi
    
    # This will FAIL because tools aren't all registered yet
    if ! start_mcp_server; then
        log_error "Failed to start MCP server"
        exit 1
    fi
    
    # Run all test phases
    initialize_mcp_connection || true
    discover_all_tools || true
    verify_expected_tools || true
    test_tool_schemas || true
    test_tool_callability || true
    test_crud_operation_completeness || true
    test_special_operations || true
    
    generate_detailed_report
    print_comprehensive_summary
    
    # Return appropriate exit code
    if [ $TESTS_FAILED -eq 0 ] && [ $TOOLS_MISSING -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Execute main function
main "$@"