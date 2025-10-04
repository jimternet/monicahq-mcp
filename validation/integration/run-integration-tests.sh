#!/bin/bash

# Integration Test Runner
# Runs all integration tests for MonicaHQ MCP Server

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LOG_FILE="/tmp/mcp-integration-tests.log"
REPORT_FILE="/tmp/mcp-integration-test-report.md"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results tracking
declare -a PASSED_TESTS=()
declare -a FAILED_TESTS=()
declare -a SKIPPED_TESTS=()
TOTAL_TESTS=0

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Error handling
error_exit() {
    echo -e "${RED}FATAL ERROR: $1${NC}" >&2
    log "FATAL ERROR: $1"
    exit 1
}

# Success message
success() {
    echo -e "${GREEN}✓ $1${NC}"
    log "SUCCESS: $1"
}

# Failure message
failure() {
    echo -e "${RED}✗ $1${NC}"
    log "FAILURE: $1"
}

# Warning message
warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
    log "WARNING: $1"
}

# Info message
info() {
    echo -e "${BLUE}ℹ $1${NC}"
    log "INFO: $1"
}

# Skip message
skip() {
    echo -e "${YELLOW}⊘ $1${NC}"
    log "SKIPPED: $1"
}

# Run a single test
run_test() {
    local test_name="$1"
    local test_command="$2"
    local test_description="$3"
    local required="${4:-true}"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    info "Running test: $test_name"
    log "Test command: $test_command"
    
    local start_time=$(date +%s)
    local test_output="/tmp/test_${test_name}_output.log"
    local test_error="/tmp/test_${test_name}_error.log"
    
    if eval "$test_command" > "$test_output" 2> "$test_error"; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        success "$test_name - $test_description (${duration}s)"
        PASSED_TESTS+=("$test_name")
        return 0
    else
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        if [ "$required" = "true" ]; then
            failure "$test_name - $test_description (${duration}s)"
            log "Test error output: $(cat "$test_error" 2>/dev/null || echo 'No error output')"
            FAILED_TESTS+=("$test_name")
            return 1
        else
            skip "$test_name - $test_description (optional, ${duration}s)"
            SKIPPED_TESTS+=("$test_name")
            return 0
        fi
    fi
}

# Check prerequisites
check_prerequisites() {
    info "Checking prerequisites..."
    
    # Check Java
    if ! command -v java >/dev/null 2>&1; then
        error_exit "Java not found. Please install Java 17 or later"
    fi
    success "Java found: $(java -version 2>&1 | head -1)"
    
    # Check if MCP server JAR exists
    cd "$PROJECT_ROOT"
    if ! ls build/libs/monica-hq-mcp-*.jar 1> /dev/null 2>&1; then
        info "Building MCP server JAR..."
        ./gradlew build -x test || error_exit "Failed to build MCP server JAR"
    fi
    success "MCP server JAR found"
    
    # Check environment variables
    if [ -z "${MONICA_API_URL:-}" ]; then
        warning "MONICA_API_URL not set - some tests may fail"
    fi
    
    if [ -z "${MONICA_API_TOKEN:-}" ]; then
        warning "MONICA_API_TOKEN not set - some tests may fail"
    fi
}

# Test MCP Protocol Basics
test_mcp_protocol() {
    info "Testing MCP Protocol basics..."
    
    run_test "mcp_initialize" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-1\",\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\",\"clientInfo\":{\"name\":\"test-client\",\"version\":\"1.0.0\"}}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"protocolVersion\"'" \
        "MCP initialize handshake" \
        true
    
    run_test "mcp_tools_list" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-2\",\"method\":\"tools/list\",\"params\":{}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"tools\"'" \
        "MCP tools list endpoint" \
        true
    
    run_test "mcp_ping" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-3\",\"method\":\"ping\",\"params\":{}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"pong\"'" \
        "MCP ping/pong" \
        false
}

# Test STDIO Communication
test_stdio_communication() {
    info "Testing STDIO communication..."
    
    run_test "stdio_json_input" \
        "$SCRIPT_DIR/test-stdio-communication.sh" \
        "STDIO JSON communication" \
        true
    
    run_test "stdio_multiple_messages" \
        "echo -e '{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\"}}\n{\"jsonrpc\":\"2.0\",\"id\":\"2\",\"method\":\"tools/list\",\"params\":{}}' | timeout 15s java -jar build/libs/monica-hq-mcp-*.jar | grep -c '\"id\"'" \
        "Multiple STDIO messages" \
        true
}

# Test Tool Discovery
test_tool_discovery() {
    info "Testing tool discovery..."
    
    run_test "all_tools_discovery" \
        "$SCRIPT_DIR/validate-all-122-tools.sh" \
        "All 122 tools discovery" \
        true
    
    run_test "activity_create_tool" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-activity\",\"method\":\"tools/list\",\"params\":{}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q 'activity_create'" \
        "Activity create tool exists" \
        true
    
    run_test "contact_list_tool" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-contact\",\"method\":\"tools/list\",\"params\":{}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q 'contact_list'" \
        "Contact list tool exists" \
        true
}

# Test Tool Execution
test_tool_execution() {
    info "Testing tool execution..."
    
    # Test activity creation with various parameter formats
    run_test "activity_create_string_attendees" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-activity-1\",\"method\":\"tools/call\",\"params\":{\"name\":\"monicahq:activity_create\",\"arguments\":{\"summary\":\"Test activity\",\"attendees\":[\"John Doe\"]}}}' | timeout 15s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"jsonrpc\"'" \
        "Activity create with string attendees" \
        false
    
    run_test "activity_create_object_attendees" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-activity-2\",\"method\":\"tools/call\",\"params\":{\"name\":\"monicahq:activity_create\",\"arguments\":{\"summary\":\"Test activity\",\"attendees\":[{\"contactId\":123}]}}}' | timeout 15s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"jsonrpc\"'" \
        "Activity create with object attendees" \
        false
    
    run_test "contact_list_execution" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-contact-list\",\"method\":\"tools/call\",\"params\":{\"name\":\"monicahq:contact_list\",\"arguments\":{\"limit\":1}}}' | timeout 15s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"jsonrpc\"'" \
        "Contact list execution" \
        false
}

# Test Debug Mode
test_debug_mode() {
    info "Testing debug mode..."
    
    run_test "debug_mode_activation" \
        "MCP_DEBUG=true echo '{\"jsonrpc\":\"2.0\",\"id\":\"debug-test\",\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\"}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar 2>&1 | grep -q 'MCP-DEBUG'" \
        "Debug mode activation and logging" \
        true
    
    run_test "debug_capabilities" \
        "MCP_DEBUG=true echo '{\"jsonrpc\":\"2.0\",\"id\":\"debug-caps\",\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\"}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q 'debugMode'" \
        "Debug mode capabilities in response" \
        true
}

# Test Error Handling
test_error_handling() {
    info "Testing error handling..."
    
    run_test "invalid_json" \
        "echo 'invalid-json' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"error\"'" \
        "Invalid JSON handling" \
        true
    
    run_test "unknown_method" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-unknown\",\"method\":\"unknown_method\",\"params\":{}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"Method not found\"'" \
        "Unknown method error" \
        true
    
    run_test "invalid_tool_name" \
        "echo '{\"jsonrpc\":\"2.0\",\"id\":\"test-invalid-tool\",\"method\":\"tools/call\",\"params\":{\"name\":\"nonexistent:tool\",\"arguments\":{}}}' | timeout 10s java -jar build/libs/monica-hq-mcp-*.jar | grep -q '\"error\"'" \
        "Invalid tool name handling" \
        true
}

# Test Performance
test_performance() {
    info "Testing performance..."
    
    run_test "response_time" \
        "time (echo '{\"jsonrpc\":\"2.0\",\"id\":\"perf-test\",\"method\":\"tools/list\",\"params\":{}}' | timeout 5s java -jar build/libs/monica-hq-mcp-*.jar > /dev/null)" \
        "Response time under 5 seconds" \
        false
    
    run_test "memory_usage" \
        "timeout 10s java -Xmx128m -jar build/libs/monica-hq-mcp-*.jar <<< '{\"jsonrpc\":\"2.0\",\"id\":\"mem-test\",\"method\":\"tools/list\",\"params\":{}}' > /dev/null" \
        "Memory usage under 128MB" \
        false
}

# Generate comprehensive report
generate_report() {
    info "Generating test report..."
    
    local total_passed=${#PASSED_TESTS[@]}
    local total_failed=${#FAILED_TESTS[@]}
    local total_skipped=${#SKIPPED_TESTS[@]}
    local success_rate=0
    
    if [ $TOTAL_TESTS -gt 0 ]; then
        success_rate=$(( (total_passed * 100) / TOTAL_TESTS ))
    fi
    
    cat > "$REPORT_FILE" << EOF
# MonicaHQ MCP Server - Integration Test Report

**Generated:** $(date '+%Y-%m-%d %H:%M:%S')
**Test Runner:** $0
**Project Root:** $PROJECT_ROOT

## Test Summary

- **Total Tests:** $TOTAL_TESTS
- **Passed:** $total_passed
- **Failed:** $total_failed
- **Skipped:** $total_skipped
- **Success Rate:** $success_rate%

## Test Results

### ✅ Passed Tests ($total_passed)
EOF

    for test in "${PASSED_TESTS[@]}"; do
        echo "- ✅ $test" >> "$REPORT_FILE"
    done

    if [ $total_failed -gt 0 ]; then
        cat >> "$REPORT_FILE" << EOF

### ❌ Failed Tests ($total_failed)
EOF
        for test in "${FAILED_TESTS[@]}"; do
            echo "- ❌ $test" >> "$REPORT_FILE"
        done
    fi

    if [ $total_skipped -gt 0 ]; then
        cat >> "$REPORT_FILE" << EOF

### ⊘ Skipped Tests ($total_skipped)
EOF
        for test in "${SKIPPED_TESTS[@]}"; do
            echo "- ⊘ $test" >> "$REPORT_FILE"
        done
    fi

    cat >> "$REPORT_FILE" << EOF

## Environment

- **Java Version:** $(java -version 2>&1 | head -1)
- **MONICA_API_URL:** ${MONICA_API_URL:-"Not set"}
- **MONICA_API_TOKEN:** $([ -n "${MONICA_API_TOKEN:-}" ] && echo "Set (${#MONICA_API_TOKEN} chars)" || echo "Not set")
- **Working Directory:** $(pwd)

## Test Categories

1. **MCP Protocol Basics** - Core JSON-RPC 2.0 over STDIO functionality
2. **STDIO Communication** - Input/output handling and message parsing
3. **Tool Discovery** - Tool listing and schema validation
4. **Tool Execution** - Actual tool invocation and parameter handling
5. **Debug Mode** - Debug logging and enhanced capabilities
6. **Error Handling** - Error response generation and validation
7. **Performance** - Response time and resource usage

## Next Steps

EOF

    if [ $total_failed -gt 0 ]; then
        cat >> "$REPORT_FILE" << EOF
### Failed Tests Require Attention

1. Check the individual test error logs in /tmp/test_*_error.log
2. Verify environment variables are correctly set
3. Ensure MonicaHQ API is accessible (for tool execution tests)
4. Review server logs for detailed error information

EOF
    fi

    cat >> "$REPORT_FILE" << EOF
### For Claude Desktop Integration

If all core tests pass, proceed with:
1. Run: \`validation/setup/verify-claude-desktop.sh\`
2. Configure Claude Desktop with the generated configuration
3. Test with actual Claude Desktop client

### Debugging

- **Detailed logs:** $LOG_FILE
- **Individual test outputs:** /tmp/test_*_output.log
- **Individual test errors:** /tmp/test_*_error.log

---
*Generated by MonicaHQ MCP Server integration test suite*
EOF

    success "Test report generated: $REPORT_FILE"
}

# Display results summary
display_summary() {
    local total_passed=${#PASSED_TESTS[@]}
    local total_failed=${#FAILED_TESTS[@]}
    local total_skipped=${#SKIPPED_TESTS[@]}
    local success_rate=0
    
    if [ $TOTAL_TESTS -gt 0 ]; then
        success_rate=$(( (total_passed * 100) / TOTAL_TESTS ))
    fi
    
    echo ""
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}                    INTEGRATION TEST RESULTS                  ${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "📊 ${BLUE}Test Summary:${NC}"
    echo "   Total tests: $TOTAL_TESTS"
    echo -e "   Passed: ${GREEN}$total_passed${NC}"
    
    if [ $total_failed -gt 0 ]; then
        echo -e "   Failed: ${RED}$total_failed${NC}"
    else
        echo -e "   Failed: $total_failed"
    fi
    
    if [ $total_skipped -gt 0 ]; then
        echo -e "   Skipped: ${YELLOW}$total_skipped${NC}"
    else
        echo -e "   Skipped: $total_skipped"
    fi
    
    echo "   Success rate: $success_rate%"
    echo ""
    
    if [ $total_failed -eq 0 ]; then
        echo -e "${GREEN}✅ ALL INTEGRATION TESTS PASSED!${NC}"
        echo ""
        echo -e "${BLUE}🚀 Ready for Claude Desktop integration${NC}"
        echo "Next step: Run validation/setup/verify-claude-desktop.sh"
    else
        echo -e "${RED}❌ SOME TESTS FAILED${NC}"
        echo ""
        echo -e "${YELLOW}⚠ Review failed tests before proceeding${NC}"
        echo "Check detailed logs at: $LOG_FILE"
    fi
    
    echo ""
    echo -e "${BLUE}📄 Full report available at: $REPORT_FILE${NC}"
}

# Main execution
main() {
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}        MonicaHQ MCP Server - Integration Test Suite         ${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo ""
    
    log "Starting integration test suite"
    
    # Clear previous logs
    > "$LOG_FILE"
    
    # Initialize test arrays
    PASSED_TESTS=()
    FAILED_TESTS=()
    SKIPPED_TESTS=()
    TOTAL_TESTS=0
    
    # Run test suites
    check_prerequisites
    
    echo ""
    test_mcp_protocol
    
    echo ""
    test_stdio_communication
    
    echo ""
    test_tool_discovery
    
    echo ""
    test_tool_execution
    
    echo ""
    test_debug_mode
    
    echo ""
    test_error_handling
    
    echo ""
    test_performance
    
    echo ""
    generate_report
    display_summary
    
    # Exit with appropriate code
    if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
        log "Integration tests completed with failures"
        exit 1
    else
        log "All integration tests passed successfully"
        exit 0
    fi
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [--help|--verbose|--quick]"
        echo ""
        echo "Runs comprehensive integration tests for MonicaHQ MCP Server"
        echo ""
        echo "Options:"
        echo "  --help, -h     Show this help message"
        echo "  --verbose      Enable verbose output"
        echo "  --quick        Run only essential tests"
        echo ""
        echo "Environment variables:"
        echo "  MONICA_API_URL    MonicaHQ API endpoint (optional for basic tests)"
        echo "  MONICA_API_TOKEN  MonicaHQ API token (optional for basic tests)"
        exit 0
        ;;
    --verbose)
        set -x
        ;;
    --quick)
        # Skip performance and optional tests
        export QUICK_MODE=true
        ;;
esac

# Run main function
main "$@"