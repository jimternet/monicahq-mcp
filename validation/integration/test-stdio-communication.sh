#!/bin/bash

# T010: Shell script test for STDIO communication
# Tests basic STDIO message exchange with MCP server
# 
# This test MUST FAIL initially (RED phase of TDD).
# STDIO communication implementation isn't complete.

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
TEST_NAME="STDIO Communication Test"
TIMEOUT_SECONDS=30
MCP_SERVER_JAR="$PROJECT_ROOT/build/libs/monicahq-mcp-1.0.0.jar"
TEMP_DIR="/tmp/mcp-stdio-test-$$"
SERVER_PID=""

# Test counters
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0

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

# Set trap for cleanup
trap cleanup EXIT

setup_test_environment() {
    log_info "Setting up test environment for $TEST_NAME"
    
    # Create temporary directory
    mkdir -p "$TEMP_DIR"
    
    # Check if JAR file exists
    if [ ! -f "$MCP_SERVER_JAR" ]; then
        log_error "MCP server JAR not found: $MCP_SERVER_JAR"
        log_info "Please run: ./gradlew build"
        return 1
    fi
    
    # Set test environment variables
    export MONICA_API_URL="https://test.monica.example.com/api"
    export MONICA_API_TOKEN="test-token-12345"
    export MCP_STDIO_MODE="true"
    export SPRING_PROFILES_ACTIVE="test"
    
    log_success "Test environment setup complete"
    return 0
}

start_mcp_server() {
    log_info "Starting MCP server in STDIO mode..."
    
    # This will FAIL because STDIO mode isn't properly implemented
    java -jar "$MCP_SERVER_JAR" \
        -Dspring.profiles.active=test \
        -Dspring.main.banner-mode=off \
        -Dlogging.level.com.monicahq.mcp=DEBUG \
        -Dlogging.level.root=WARN \
        > "$TEMP_DIR/server_stdout.log" \
        2> "$TEMP_DIR/server_stderr.log" &
    
    SERVER_PID=$!
    
    log_info "MCP server started with PID: $SERVER_PID"
    
    # Wait for server to be ready
    local attempts=0
    local max_attempts=15
    
    while [ $attempts -lt $max_attempts ]; do
        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
            log_error "MCP server process died"
            cat "$TEMP_DIR/server_stderr.log"
            return 1
        fi
        
        # Check for ready message in stderr
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

send_json_message() {
    local message="$1"
    local expected_response_pattern="$2"
    local test_description="$3"
    
    increment_test
    
    log_info "Sending JSON message: $test_description"
    
    # Send message to server stdin and capture stdout
    local response
    response=$(echo "$message" | timeout 10s java -cp "$(cat "$TEMP_DIR/classpath.txt")" \
        com.monicahq.mcp.util.StdioTestClient 2>/dev/null || true)
    
    if [ -z "$response" ]; then
        fail_test "No response received for: $test_description"
        return 1
    fi
    
    log_info "Received response: $response"
    
    # Validate JSON structure
    if ! echo "$response" | jq . >/dev/null 2>&1; then
        fail_test "Invalid JSON response for: $test_description"
        return 1
    fi
    
    # Check expected pattern if provided
    if [ -n "$expected_response_pattern" ]; then
        if echo "$response" | grep -q "$expected_response_pattern"; then
            pass_test "$test_description - Response pattern matched"
        else
            fail_test "$test_description - Response pattern not found: $expected_response_pattern"
            return 1
        fi
    else
        pass_test "$test_description - Valid JSON response received"
    fi
    
    return 0
}

test_basic_stdio_communication() {
    log_info "Testing basic STDIO communication..."
    
    # Test 1: Initialize request
    increment_test
    local init_request='{
        "jsonrpc": "2.0",
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "stdio-test-client",
                "version": "1.0.0"
            }
        },
        "id": 1
    }'
    
    # This will FAIL because STDIO JSON-RPC handling isn't implemented
    if ! send_json_message "$init_request" '"result"' "Initialize request"; then
        return 1
    fi
    
    # Test 2: Tools list request
    increment_test
    local tools_request='{
        "jsonrpc": "2.0",
        "method": "tools/list",
        "params": {},
        "id": 2
    }'
    
    # This will FAIL because tools/list STDIO endpoint isn't implemented
    if ! send_json_message "$tools_request" '"tools"' "Tools list request"; then
        return 1
    fi
    
    # Test 3: Tool call request
    increment_test
    local tool_call_request='{
        "jsonrpc": "2.0",
        "method": "tools/call",
        "params": {
            "name": "contact_create",
            "arguments": {
                "firstName": "STDIO",
                "lastName": "Test",
                "genderId": 1,
                "isBirthdateKnown": false,
                "isDeceased": false,
                "isDeceasedDateKnown": false
            }
        },
        "id": 3
    }'
    
    # This will FAIL because tool call STDIO handling isn't implemented
    if ! send_json_message "$tool_call_request" '"result"' "Tool call request"; then
        return 1
    fi
    
    log_success "Basic STDIO communication tests completed"
    return 0
}

test_stdio_error_handling() {
    log_info "Testing STDIO error handling..."
    
    # Test 1: Invalid JSON
    increment_test
    local invalid_json='{ invalid json }'
    
    # This will FAIL because STDIO error handling isn't implemented
    if ! send_json_message "$invalid_json" '"error"' "Invalid JSON handling"; then
        log_warning "Expected error response for invalid JSON"
    fi
    
    # Test 2: Missing method
    increment_test
    local missing_method='{
        "jsonrpc": "2.0",
        "params": {},
        "id": 4
    }'
    
    # This will FAIL because method validation isn't implemented
    if ! send_json_message "$missing_method" '"error"' "Missing method handling"; then
        log_warning "Expected error response for missing method"
    fi
    
    # Test 3: Unknown method
    increment_test
    local unknown_method='{
        "jsonrpc": "2.0",
        "method": "unknown/method",
        "params": {},
        "id": 5
    }'
    
    # This will FAIL because unknown method handling isn't implemented
    if ! send_json_message "$unknown_method" '"error"' "Unknown method handling"; then
        log_warning "Expected error response for unknown method"
    fi
    
    log_success "STDIO error handling tests completed"
    return 0
}

test_stdio_protocol_compliance() {
    log_info "Testing STDIO protocol compliance..."
    
    # Test 1: JSON-RPC 2.0 version validation
    increment_test
    local wrong_version='{
        "jsonrpc": "1.0",
        "method": "tools/list",
        "params": {},
        "id": 6
    }'
    
    # This will FAIL because version validation isn't implemented
    if ! send_json_message "$wrong_version" '"error"' "JSON-RPC version validation"; then
        log_warning "Expected error for incorrect JSON-RPC version"
    fi
    
    # Test 2: Request ID handling
    increment_test
    local string_id_request='{
        "jsonrpc": "2.0",
        "method": "tools/list",
        "params": {},
        "id": "string-id-test"
    }'
    
    # This will FAIL because ID handling isn't implemented
    if ! send_json_message "$string_id_request" '"string-id-test"' "String ID handling"; then
        log_warning "Expected response with matching string ID"
    fi
    
    # Test 3: Notification handling (no ID)
    increment_test
    local notification='{
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
    }'
    
    # This will FAIL because notification handling isn't implemented
    local response
    response=$(echo "$notification" | timeout 5s java -cp "$(cat "$TEMP_DIR/classpath.txt")" \
        com.monicahq.mcp.util.StdioTestClient 2>/dev/null || true)
    
    if [ -z "$response" ]; then
        pass_test "Notification handling - No response expected"
    else
        log_warning "Notification should not generate response"
    fi
    
    log_success "STDIO protocol compliance tests completed"
    return 0
}

test_stdio_performance() {
    log_info "Testing STDIO performance..."
    
    increment_test
    local start_time=$(date +%s%N)
    
    # Send multiple rapid requests
    for i in {1..10}; do
        local request='{
            "jsonrpc": "2.0",
            "method": "tools/list",
            "params": {},
            "id": '$(($i + 100))'
        }'
        
        # This will FAIL because STDIO performance isn't optimized
        if ! send_json_message "$request" '"tools"' "Performance test request $i" >/dev/null; then
            fail_test "Performance test failed at request $i"
            return 1
        fi
    done
    
    local end_time=$(date +%s%N)
    local duration_ms=$(((end_time - start_time) / 1000000))
    
    log_info "Processed 10 requests in ${duration_ms}ms"
    
    if [ $duration_ms -lt 5000 ]; then
        pass_test "STDIO performance test - ${duration_ms}ms for 10 requests"
    else
        fail_test "STDIO performance test - Too slow: ${duration_ms}ms for 10 requests"
        return 1
    fi
    
    return 0
}

verify_stdout_cleanliness() {
    log_info "Verifying stdout cleanliness..."
    
    increment_test
    
    # Check that server stdout only contains JSON-RPC responses
    if [ -f "$TEMP_DIR/server_stdout.log" ]; then
        local stdout_content
        stdout_content=$(cat "$TEMP_DIR/server_stdout.log")
        
        # Should not contain Spring Boot startup messages
        if echo "$stdout_content" | grep -q "Spring Boot"; then
            fail_test "STDOUT contaminated with Spring Boot messages"
            return 1
        fi
        
        # Should not contain logging statements
        if echo "$stdout_content" | grep -qE "(INFO|DEBUG|WARN|ERROR)"; then
            fail_test "STDOUT contaminated with log messages"
            return 1
        fi
        
        # All lines should be valid JSON (if any)
        if [ -n "$stdout_content" ]; then
            local line_count=0
            local valid_json_lines=0
            
            while IFS= read -r line; do
                if [ -n "$line" ]; then
                    ((line_count++))
                    if echo "$line" | jq . >/dev/null 2>&1; then
                        ((valid_json_lines++))
                    fi
                fi
            done <<< "$stdout_content"
            
            if [ $line_count -eq $valid_json_lines ]; then
                pass_test "STDOUT cleanliness - All output lines are valid JSON"
            else
                fail_test "STDOUT cleanliness - Found non-JSON output"
                return 1
            fi
        else
            pass_test "STDOUT cleanliness - No contaminating output"
        fi
    else
        log_warning "No stdout log file found"
    fi
    
    return 0
}

generate_test_report() {
    log_info "Generating test report..."
    
    local report_file="$TEMP_DIR/stdio_communication_test_report.json"
    
    cat > "$report_file" << EOF
{
    "testName": "$TEST_NAME",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "results": {
        "total": $TESTS_TOTAL,
        "passed": $TESTS_PASSED,
        "failed": $TESTS_FAILED,
        "success_rate": $(echo "scale=2; $TESTS_PASSED * 100 / $TESTS_TOTAL" | bc -l)
    },
    "environment": {
        "java_version": "$(java -version 2>&1 | head -n1)",
        "jar_file": "$MCP_SERVER_JAR",
        "temp_dir": "$TEMP_DIR"
    },
    "logs": {
        "server_stdout": "$(cat "$TEMP_DIR/server_stdout.log" 2>/dev/null || echo 'Not available')",
        "server_stderr": "$(cat "$TEMP_DIR/server_stderr.log" 2>/dev/null || echo 'Not available')"
    }
}
EOF
    
    log_info "Test report saved to: $report_file"
    
    # Copy report to project validation docs
    local docs_dir="$PROJECT_ROOT/validation/docs"
    mkdir -p "$docs_dir"
    cp "$report_file" "$docs_dir/stdio-communication-test-$(date +%Y%m%d-%H%M%S).json"
}

print_summary() {
    echo
    echo "=============================================="
    echo "           STDIO COMMUNICATION TEST SUMMARY"
    echo "=============================================="
    echo
    echo "Total Tests:  $TESTS_TOTAL"
    echo "Passed:       $TESTS_PASSED"
    echo "Failed:       $TESTS_FAILED"
    echo
    
    if [ $TESTS_FAILED -eq 0 ]; then
        log_success "All tests passed! STDIO communication is working correctly."
        echo
        return 0
    else
        log_error "Some tests failed. STDIO communication needs implementation."
        echo
        echo "This is expected for TDD Phase 3.2 - these tests should FAIL initially."
        echo "Implementation tasks:"
        echo "1. Implement McpStdioServer main class"
        echo "2. Add JSON-RPC 2.0 message handling"
        echo "3. Implement tools/list and tools/call endpoints"
        echo "4. Add proper error handling for invalid requests"
        echo "5. Ensure stdout cleanliness (only JSON-RPC responses)"
        echo
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
    
    # This will FAIL because the MCP server STDIO mode isn't implemented
    if ! start_mcp_server; then
        log_error "Failed to start MCP server"
        exit 1
    fi
    
    # Run all test suites
    test_basic_stdio_communication || true
    test_stdio_error_handling || true
    test_stdio_protocol_compliance || true
    test_stdio_performance || true
    verify_stdout_cleanliness || true
    
    generate_test_report
    print_summary
    
    # Return appropriate exit code
    if [ $TESTS_FAILED -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Execute main function
main "$@"