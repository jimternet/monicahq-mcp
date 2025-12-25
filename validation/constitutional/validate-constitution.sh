#!/bin/bash

# MonicaHQ MCP Server - Constitutional Compliance Validator
# Validates adherence to Constitution v1.2.0 principles

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

echo "🏛️  MonicaHQ MCP Server Constitutional Compliance Validator"
echo "Constitution Version: 1.2.0"
echo "========================================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

VIOLATIONS=0
WARNINGS=0

violation() {
    echo -e "${RED}❌ VIOLATION:${NC} $1"
    ((VIOLATIONS++))
}

warning() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
    ((WARNINGS++))
}

success() {
    echo -e "${GREEN}✅ PASS:${NC} $1"
}

info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

echo "Principle I: MCP Protocol First"
echo "==============================="

# Check JSON-RPC 2.0 implementation
if grep -r "jsonrpc.*2\.0" src/main/java/com/monicahq/mcp/controller/ >/dev/null 2>&1; then
    success "JSON-RPC 2.0 protocol implementation found"
else
    violation "JSON-RPC 2.0 protocol implementation not found"
fi

# Check tool categorization
if grep -r "\[Contact\]" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
    success "Tool categorization implemented"
else
    warning "Tool categorization may not be fully implemented"
fi

# Check Claude Desktop STDIO support
if [ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]; then
    success "STDIO mode for Claude Desktop implemented"
else
    violation "STDIO mode for Claude Desktop not found"
fi

# Check STDIO logging configuration (critical for MCP protocol)
if [ -f "src/main/resources/logback-stdio.xml" ] && \
   grep -q "System.err" src/main/resources/logback-stdio.xml >/dev/null 2>&1; then
    success "STDIO logging properly configured (STDERR only)"
else
    violation "STDIO logging configuration missing - STDOUT contamination risk"
fi

# Check for STDOUT contamination risks
if grep -r "System\.out\.print" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
    violation "System.out.print* calls found - will contaminate STDOUT in STDIO mode"
else
    success "No STDOUT contamination detected"
fi

echo ""
echo "Principle II: Test-Driven Development (NON-NEGOTIABLE)"
echo "======================================================"

# Run tests and check coverage
info "Running test suite..."
if ./gradlew test --quiet >/dev/null 2>&1; then
    TEST_RESULTS=$(./gradlew test 2>&1 | grep -E "BUILD SUCCESSFUL|tests completed" || true)
    if [ -n "$TEST_RESULTS" ]; then
        success "All tests passing"
    else
        violation "Tests are failing"
    fi
else
    violation "Test execution failed"
fi

# Count test files
CONTRACT_TESTS=$(find src/test/java/com/monicahq/mcp/contract -name "*.java" | wc -l)
INTEGRATION_TESTS=$(find src/test/java/com/monicahq/mcp/integration -name "*.java" | wc -l)
TOTAL_TESTS=$((CONTRACT_TESTS + INTEGRATION_TESTS))

info "Test inventory: $CONTRACT_TESTS contract tests, $INTEGRATION_TESTS integration tests"

if [ "$TOTAL_TESTS" -ge 50 ]; then
    success "Comprehensive test coverage ($TOTAL_TESTS tests)"
else
    warning "Test coverage may be insufficient ($TOTAL_TESTS tests)"
fi

echo ""
echo "Principle III: Spring Boot Architecture Excellence"
echo "=================================================="

# Check Spring Boot 3.x
if grep "spring-boot.*3\." build.gradle >/dev/null 2>&1; then
    success "Spring Boot 3.x dependency found"
else
    violation "Spring Boot 3.x not found in dependencies"
fi

# Check WebFlux for external API calls
if grep -r "WebClient" src/main/java/com/monicahq/mcp/client/ >/dev/null 2>&1; then
    success "WebFlux used for external API clients"
else
    violation "WebFlux not found for external API clients"
fi

# Check dual-mode support
if grep -r "stdio.*web" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1 || \
   ([ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ] && grep -r "WebSocket" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1); then
    success "Dual-mode architecture (STDIO + Web Server) implemented"
else
    warning "Dual-mode architecture may not be fully implemented"
fi

# Check circuit breaker patterns
if grep -r "CircuitBreaker\|Resilience4j" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
    success "Circuit breaker patterns implemented"
else
    violation "Circuit breaker patterns not found"
fi

echo ""
echo "Principle IV: Production-Ready Deployment"
echo "========================================="

# Check Docker support
if [ -f "Dockerfile" ]; then
    success "Docker containerization supported"
else
    violation "Dockerfile not found"
fi

# Check environment variable configuration
if grep -r "@Value.*monica\." src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
    success "Environment variable configuration implemented"
else
    violation "Environment variable configuration not found"
fi

# Check health endpoints (Web Server mode)
if grep -r "actuator\|health" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1 || \
   grep "spring-boot-starter-actuator" build.gradle >/dev/null 2>&1; then
    success "Health checks and monitoring capabilities included"
else
    warning "Health checks may not be implemented"
fi

echo ""
echo "Principle V: Type Safety and Code Generation"
echo "============================================"

# Check MapStruct usage
if grep -r "MapStruct\|@Mapper" src/main/java/com/monicahq/mcp/mapper/ >/dev/null 2>&1; then
    success "MapStruct used for type-safe mapping"
else
    violation "MapStruct not found for type-safe mapping"
fi

# Check Lombok usage
if grep -r "@Data\|@Getter\|@Setter\|@Builder" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
    success "Lombok used for code generation"
else
    violation "Lombok not found for boilerplate reduction"
fi

# Check strong typing with validation
if grep -r "@Valid\|@NotNull\|@Size" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
    success "Strong typing with validation implemented"
else
    warning "Validation annotations may not be comprehensive"
fi

echo ""
echo "API Integration Standards"
echo "========================"

# Check OAuth2 Bearer token authentication
if grep -r "Bearer\|Authorization.*token" src/main/java/com/monicahq/mcp/client/ >/dev/null 2>&1; then
    success "OAuth2 Bearer token authentication implemented"
else
    violation "OAuth2 Bearer token authentication not found"
fi

# Check error handling and retries
if grep -r "@Retry\|retry" src/main/java/com/monicahq/mcp/client/ >/dev/null 2>&1; then
    success "Retry mechanisms implemented"
else
    warning "Retry mechanisms may not be implemented"
fi

echo ""
echo "Principle VI: MCP Response Content Visibility"
echo "============================================="

# Check if services format content field
SERVICE_FILES=$(find src/main/java/com/monicahq/mcp/service -name "*.java" 2>/dev/null | wc -l)
if [ "$SERVICE_FILES" -gt 0 ]; then
    # Check for content field formatting in services
    if grep -r "content.*=.*format\|formatContent\|contentFormatter" src/main/java/com/monicahq/mcp/service/ >/dev/null 2>&1; then
        success "Content field formatting found in services"
    else
        violation "Services not formatting content field for Claude Desktop visibility"
    fi
    
    # Check for ContentFormatter utility
    if [ -f "src/main/java/com/monicahq/mcp/util/ContentFormatter.java" ]; then
        success "ContentFormatter utility exists"
    else
        warning "ContentFormatter utility not found - may impact consistency"
    fi
    
    # Check if tests verify content field
    if grep -r "content.*field\|getContent()\|\.content" src/test/java/ >/dev/null 2>&1; then
        success "Tests verify content field formatting"
    else
        violation "Tests do not verify content field formatting"
    fi
else
    info "Service files not yet created - skipping content visibility checks"
fi

echo ""
echo "Code Quality Requirements"
echo "========================"

# Check JavaDoc documentation
JAVADOC_FILES=$(find src/main/java -name "*.java" -exec grep -l "/\*\*" {} \; | wc -l)
TOTAL_JAVA_FILES=$(find src/main/java -name "*.java" | wc -l)

if [ "$JAVADOC_FILES" -gt $((TOTAL_JAVA_FILES / 2)) ]; then
    success "Adequate JavaDoc documentation found"
else
    warning "JavaDoc documentation may be insufficient ($JAVADOC_FILES/$TOTAL_JAVA_FILES files)"
fi

# Check for hardcoded secrets
if grep -r -i "password\|secret\|key.*=" src/main/java/ | grep -v -E "\.properties|@Value|System\.getenv" >/dev/null 2>&1; then
    violation "Potential hardcoded secrets found"
else
    success "No hardcoded secrets detected"
fi

echo ""
echo "========================================================"
echo "Constitutional Compliance Summary"
echo "========================================================"

if [ "$VIOLATIONS" -eq 0 ] && [ "$WARNINGS" -eq 0 ]; then
    echo -e "${GREEN}🎉 FULLY COMPLIANT${NC} - No violations or warnings"
    echo "The MonicaHQ MCP Server fully adheres to Constitution v1.2.0"
elif [ "$VIOLATIONS" -eq 0 ]; then
    echo -e "${YELLOW}⚠️  MOSTLY COMPLIANT${NC} - $WARNINGS warnings found"
    echo "Address warnings to achieve full constitutional compliance"
else
    echo -e "${RED}❌ NON-COMPLIANT${NC} - $VIOLATIONS violations, $WARNINGS warnings"
    echo "Constitutional violations must be resolved before deployment"
fi

echo ""
echo "Violations: $VIOLATIONS"
echo "Warnings: $WARNINGS"
echo ""

if [ "$VIOLATIONS" -gt 0 ]; then
    echo "Next steps:"
    echo "1. Address all constitutional violations"
    echo "2. Re-run validation: ./validate-constitution.sh"
    echo "3. Update constitution if architectural decisions require it"
    exit 1
else
    echo "Constitution validation complete ✅"
    exit 0
fi