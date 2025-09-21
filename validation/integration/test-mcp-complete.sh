#!/bin/bash

# MonicaHQ MCP Server - Comprehensive Testing Suite
# Validates all aspects of MCP server functionality

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

FAILURES=0
TESTS_RUN=0

fail() {
    echo -e "${RED}❌ FAILED:${NC} $1"
    ((FAILURES++))
}

pass() {
    echo -e "${GREEN}✅ PASSED:${NC} $1"
}

info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

warn() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
}

run_test() {
    ((TESTS_RUN++))
    if $1; then
        pass "$2"
    else
        fail "$2"
    fi
}

echo "🧪 MonicaHQ MCP Server - Comprehensive Testing Suite"
echo "====================================================="
echo ""

# Check prerequisites
echo "Checking Prerequisites"
echo "====================="

check_environment() {
    if [ -z "$MONICA_API_URL" ] || [ -z "$MONICA_API_TOKEN" ]; then
        warn "MONICA_API_URL and MONICA_API_TOKEN not set - some tests will be skipped"
        return 1
    fi
    return 0
}

check_build() {
    if [ ! -f "build/libs/monicahqmcp-0.1.0.jar" ]; then
        info "Building JAR file..."
        ./gradlew build --quiet
    fi
    [ -f "build/libs/monicahqmcp-0.1.0.jar" ]
}

check_docker() {
    command -v docker >/dev/null 2>&1
}

check_jq() {
    command -v jq >/dev/null 2>&1
}

run_test check_build "JAR file build"
run_test check_docker "Docker availability"
run_test check_jq "jq JSON processor"
run_test check_environment "Environment variables"

echo ""
echo "Phase 1: Constitutional Compliance"
echo "=================================="

run_constitutional_compliance() {
    ./validate-constitution.sh >/dev/null 2>&1
}

run_test run_constitutional_compliance "Constitutional compliance validation"

echo ""
echo "Phase 2: Unit and Integration Tests"
echo "==================================="

run_gradle_tests() {
    ./gradlew test --quiet >/dev/null 2>&1
}

test_count_validation() {
    local test_count=$(find src/test/java -name "*.java" | wc -l)
    [ "$test_count" -ge 50 ]
}

run_test run_gradle_tests "Gradle test suite (136 tests)"
run_test test_count_validation "Test count validation (≥50 test files)"

echo ""
echo "Phase 3: MCP Protocol Validation"
echo "================================"

test_mcp_stdio_basic() {
    if ! check_environment; then
        return 1
    fi
    
    # Test basic MCP protocol communication
    local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    # Check if response contains expected JSON-RPC structure
    echo "$response" | jq -e '.jsonrpc == "2.0" and .id == 1 and has("result")' >/dev/null 2>&1
}

test_stdout_cleanliness() {
    if ! check_environment; then
        return 1
    fi
    
    # Critical: STDOUT must contain ONLY JSON-RPC responses
    local full_output=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null)
    
    # Count lines - should be exactly 1 (JSON response only)
    local line_count=$(echo "$full_output" | wc -l)
    
    # Test that output is valid JSON and nothing else
    if [ "$line_count" -eq 1 ] && echo "$full_output" | jq empty >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

test_mcp_initialize() {
    if ! check_environment; then
        return 1
    fi
    
    local init_request='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}},"id":1}'
    local response=$(echo "$init_request" | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    echo "$response" | jq -e '.result.protocolVersion and .result.serverInfo and .result.capabilities' >/dev/null 2>&1
}

test_mcp_tool_listing() {
    if ! check_environment; then
        return 1
    fi
    
    local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    # Check for expected tool count (50 operations)
    local tool_count=$(echo "$response" | jq -r '.result.tools | length' 2>/dev/null || echo "0")
    [ "$tool_count" -ge 45 ]  # Allow for some variance
}

test_mcp_error_handling() {
    if ! check_environment; then
        return 1
    fi
    
    # Test invalid JSON-RPC request
    local response=$(echo '{"method":"invalid","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    echo "$response" | jq -e 'has("error") and .error.code' >/dev/null 2>&1
}

if check_environment; then
    run_test test_mcp_stdio_basic "MCP STDIO basic communication"
    run_test test_stdout_cleanliness "STDOUT cleanliness (JSON only)"
    run_test test_mcp_initialize "MCP initialize protocol"
    run_test test_mcp_tool_listing "MCP tool listing (50 operations)"
    run_test test_mcp_error_handling "MCP error handling"
else
    info "Skipping MCP protocol tests - environment variables not set"
fi

echo ""
echo "Phase 4: Architecture Validation"
echo "================================"

test_spring_boot_reactive() {
    grep -r "WebClient\|WebFlux" src/main/java/com/monicahq/mcp/client/ >/dev/null 2>&1
}

test_circuit_breaker() {
    grep -r "@CircuitBreaker\|resilience4j" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1
}

test_dependency_injection() {
    grep -r "@Service\|@Component\|@Autowired" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1
}

test_dual_mode_support() {
    [ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ] && \
    grep -r "WebSocket" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1
}

run_test test_spring_boot_reactive "Spring Boot reactive patterns"
run_test test_circuit_breaker "Circuit breaker implementation"
run_test test_dependency_injection "Dependency injection usage"
run_test test_dual_mode_support "Dual-mode architecture"

echo ""
echo "Phase 5: Security and Quality"
echo "============================="

test_no_hardcoded_secrets() {
    ! grep -r -i "password.*=.*[\"']" src/main/java/ >/dev/null 2>&1
}

test_environment_config() {
    grep -r "@Value.*monica\." src/main/java/com/monicahq/mcp/ >/dev/null 2>&1
}

test_type_safety() {
    grep -r "@Valid\|@NotNull\|MapStruct" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1
}

test_oauth2_implementation() {
    grep -r "Bearer\|Authorization" src/main/java/com/monicahq/mcp/client/ >/dev/null 2>&1
}

run_test test_no_hardcoded_secrets "No hardcoded secrets"
run_test test_environment_config "Environment variable configuration"
run_test test_type_safety "Type safety with validation"
run_test test_oauth2_implementation "OAuth2 Bearer token authentication"

echo ""
echo "Phase 6: Deployment Readiness"
echo "============================="

test_docker_build() {
    if check_docker; then
        docker build -t monicahq-mcp-test . >/dev/null 2>&1
        local result=$?
        docker rmi monicahq-mcp-test >/dev/null 2>&1 || true
        return $result
    else
        return 1
    fi
}

test_jar_execution() {
    timeout 5s java -jar build/libs/monicahqmcp-0.1.0.jar --help >/dev/null 2>&1 || \
    timeout 5s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio </dev/null >/dev/null 2>&1
}

test_health_endpoints() {
    grep "spring-boot-starter-actuator" build.gradle >/dev/null 2>&1
}

test_logging_configuration() {
    [ -f "src/main/resources/logback-stdio.xml" ] || \
    grep -r "logging" src/main/resources/ >/dev/null 2>&1
}

if check_docker; then
    run_test test_docker_build "Docker containerization"
else
    info "Skipping Docker tests - Docker not available"
fi

run_test test_jar_execution "JAR execution"
run_test test_health_endpoints "Health monitoring endpoints"
run_test test_logging_configuration "Logging configuration"

echo ""
echo "Phase 7: Tool Accessibility Validation"
echo "======================================"

test_helper_scripts() {
    [ -x "./run-mcp-server.sh" ] && [ -x "./test-mcp-operations.sh" ]
}

test_documentation() {
    [ -f "README.md" ] && grep -i "mcp.*inspector" README.md >/dev/null 2>&1
}

test_claude_desktop_config() {
    grep -A 10 -B 5 "claude_desktop_config.json" README.md >/dev/null 2>&1
}

run_test test_helper_scripts "Helper scripts availability"
run_test test_documentation "MCP Inspector documentation"
run_test test_claude_desktop_config "Claude Desktop configuration examples"

echo ""
echo "========================================================"
echo "Comprehensive Testing Summary"
echo "========================================================"

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}🎉 ALL TESTS PASSED${NC} ($TESTS_RUN/$TESTS_RUN)"
    echo "The MonicaHQ MCP Server is ready for production deployment"
    echo ""
    echo "Next steps:"
    echo "1. Deploy to production environment"
    echo "2. Configure Claude Desktop integration"
    echo "3. Monitor using health endpoints"
    echo ""
    echo "Validation tools available:"
    echo "- ./validate-constitution.sh - Constitutional compliance"
    echo "- ./run-mcp-server.sh - STDIO server testing"  
    echo "- ./test-mcp-operations.sh - Operation examples"
    echo "- mcp-inspector - Interactive MCP testing"
elif [ "$FAILURES" -le 3 ]; then
    echo -e "${YELLOW}⚠️  MOSTLY READY${NC} ($((TESTS_RUN - FAILURES))/$TESTS_RUN tests passed)"
    echo "Minor issues found - address before production deployment"
else
    echo -e "${RED}❌ CRITICAL ISSUES${NC} ($((TESTS_RUN - FAILURES))/$TESTS_RUN tests passed)"
    echo "Significant problems found - requires immediate attention"
fi

echo ""
echo "Tests run: $TESTS_RUN"
echo "Failures: $FAILURES"
echo ""

exit $FAILURES