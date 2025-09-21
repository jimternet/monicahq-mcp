#!/bin/bash

# Constitutional Compliance Test Cases
# Validates adherence to MonicaHQ MCP Server Constitution v1.2.0

set -e

# Test configuration
CONSTITUTION_FILE=".specify/memory/constitution.md"
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_RESULTS=()

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    TEST_RESULTS+=("PASS: $1")
}

log_failure() {
    echo -e "${RED}[FAIL]${NC} $1"
    TEST_RESULTS+=("FAIL: $1")
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Test Case 1: Constitution Document Exists and is Valid
test_constitution_document() {
    log_info "Testing constitution document existence and validity..."
    
    if [[ ! -f "$CONSTITUTION_FILE" ]]; then
        log_failure "Constitution document not found at $CONSTITUTION_FILE"
        return 1
    fi
    
    # Check version is populated (not template)
    if grep -q "Version.*1.2.0" "$CONSTITUTION_FILE" && ! grep -q "\[VERSION\]" "$CONSTITUTION_FILE"; then
        log_success "Constitution document exists with valid version 1.2.0"
    else
        log_failure "Constitution document version not properly set or still contains template placeholders"
        return 1
    fi
    
    # Check all 6 core principles exist
    local principles=("MCP Protocol First" "Test-Driven Development" "Spring Boot Architecture Excellence" "Production-Ready Deployment" "Type Safety" "MCP Response Content Visibility")
    for principle in "${principles[@]}"; do
        if grep -q "$principle" "$CONSTITUTION_FILE"; then
            log_success "Core principle found: $principle"
        else
            log_failure "Missing core principle: $principle"
            return 1
        fi
    done
}

# Test Case 2: MCP Protocol First Compliance
test_mcp_protocol_first() {
    log_info "Testing MCP Protocol First principle compliance..."
    
    # Check for JSON-RPC 2.0 usage
    if find src -name "*.java" -exec grep -l "jsonrpc.*2.0" {} \; | grep -q .; then
        log_success "JSON-RPC 2.0 protocol implementation found"
    else
        log_failure "JSON-RPC 2.0 protocol implementation not found"
        return 1
    fi
    
    # Check for STDIO mode implementation
    if [[ -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        log_success "MCP STDIO server implementation exists"
    else
        log_failure "MCP STDIO server implementation not found"
        return 1
    fi
    
    # Check STDOUT cleanliness configuration
    if [[ -f "src/main/resources/logback-stdio.xml" ]] && grep -q "System.err" "src/main/resources/logback-stdio.xml"; then
        log_success "STDIO logging configuration prevents STDOUT contamination"
    else
        log_failure "STDIO logging configuration not properly set for STDOUT cleanliness"
        return 1
    fi
}

# Test Case 3: Test-Driven Development Compliance
test_tdd_compliance() {
    log_info "Testing TDD compliance (100% test coverage requirement)..."
    
    # Check if test directory exists
    if [[ ! -d "src/test" ]]; then
        log_failure "Test directory src/test not found"
        return 1
    fi
    
    # Run tests and check coverage (if available)
    if [[ -f "gradlew" ]]; then
        log_info "Running test suite to verify TDD compliance..."
        if ./gradlew test --quiet; then
            log_success "All tests pass - TDD compliance verified"
        else
            log_failure "Tests are failing - violates TDD principle"
            return 1
        fi
    else
        log_warning "Gradle wrapper not found - cannot verify test execution"
    fi
    
    # Check for contract tests
    if [[ -d "src/test/java/com/monicahq/mcp/contract" ]]; then
        log_success "Contract tests directory exists"
    else
        log_failure "Contract tests directory not found - violates TDD structure"
        return 1
    fi
}

# Test Case 4: Spring Boot Architecture Excellence
test_spring_architecture() {
    log_info "Testing Spring Boot Architecture Excellence compliance..."
    
    # Check for Spring Boot 3.x usage
    if grep -q "org.springframework.boot:spring-boot-starter" build.gradle 2>/dev/null || \
       grep -q "spring-boot-starter" pom.xml 2>/dev/null; then
        log_success "Spring Boot starter dependencies found"
    else
        log_failure "Spring Boot dependencies not found in build configuration"
        return 1
    fi
    
    # Check for WebFlux reactive implementation
    if find src -name "*.java" -exec grep -l "WebFlux\|Mono\|Flux" {} \; | grep -q .; then
        log_success "WebFlux reactive patterns found in codebase"
    else
        log_failure "WebFlux reactive patterns not found - violates architecture principle"
        return 1
    fi
    
    # Check for proper component structure
    local components=("controller" "service" "client" "config")
    for component in "${components[@]}"; do
        if [[ -d "src/main/java/com/monicahq/mcp/$component" ]]; then
            log_success "Spring Boot component structure found: $component"
        else
            log_failure "Missing component directory: $component"
            return 1
        fi
    done
}

# Test Case 5: Production-Ready Deployment
test_production_ready() {
    log_info "Testing Production-Ready Deployment compliance..."
    
    # Check for Docker support
    if [[ -f "Dockerfile" ]]; then
        log_success "Dockerfile exists for containerization"
    else
        log_failure "Dockerfile not found - violates production readiness"
        return 1
    fi
    
    # Check for environment variable configuration
    if grep -q "MONICA_API" README.md 2>/dev/null || \
       find src -name "*.java" -exec grep -l "Environment\|@Value" {} \; | grep -q .; then
        log_success "Environment variable configuration found"
    else
        log_failure "Environment variable configuration not found"
        return 1
    fi
    
    # Check for health check capabilities
    if find src -name "*.java" -exec grep -l "actuator\|health" {} \; | grep -q . || \
       grep -q "actuator" build.gradle 2>/dev/null; then
        log_success "Health check capabilities found"
    else
        log_warning "Health check capabilities not clearly identified"
    fi
}

# Test Case 6: Type Safety and Code Generation
test_type_safety() {
    log_info "Testing Type Safety and Code Generation compliance..."
    
    # Check for MapStruct usage
    if grep -q "mapstruct" build.gradle 2>/dev/null || \
       find src -name "*.java" -exec grep -l "MapStruct\|@Mapper" {} \; | grep -q .; then
        log_success "MapStruct type-safe mapping found"
    else
        log_failure "MapStruct usage not found - violates type safety principle"
        return 1
    fi
    
    # Check for Lombok usage
    if grep -q "lombok" build.gradle 2>/dev/null || \
       find src -name "*.java" -exec grep -l "@Data\|@Getter\|@Setter" {} \; | grep -q .; then
        log_success "Lombok code generation found"
    else
        log_failure "Lombok usage not found - violates code generation principle"
        return 1
    fi
    
    # Check for DTO structure
    if [[ -d "src/main/java/com/monicahq/mcp/dto" ]]; then
        log_success "DTO package structure exists for type safety"
    else
        log_failure "DTO package structure not found"
        return 1
    fi
}

# Test Case 7: MCP Response Content Visibility
test_content_visibility() {
    log_info "Testing MCP Response Content Visibility compliance..."
    
    # Check for ContentFormatter utility
    if [[ -f "src/main/java/com/monicahq/mcp/util/ContentFormatter.java" ]]; then
        log_success "ContentFormatter utility exists"
    else
        log_failure "ContentFormatter utility not found - violates content visibility principle"
        return 1
    fi
    
    # Check if services format content field
    if find src/main/java/com/monicahq/mcp/service -name "*.java" -exec grep -l "formatContent\|ContentFormatter\|content.*=.*format" {} \; 2>/dev/null | grep -q .; then
        log_success "Services implement content field formatting"
    else
        log_failure "Services do not format content field - violates content visibility principle"
        return 1
    fi
    
    # Check if tests verify content field
    if find src/test -name "*.java" -exec grep -l "content\|getContent()\|\.content" {} \; 2>/dev/null | grep -q .; then
        log_success "Tests verify content field formatting"
    else
        log_failure "Tests do not verify content field - violates testing requirements"
        return 1
    fi
}

# Main execution
main() {
    log_info "Starting Constitutional Compliance Test Suite"
    log_info "Constitution Version: 1.2.0"
    log_info "Project Root: $PROJECT_ROOT"
    echo "=================================="
    
    cd "$PROJECT_ROOT"
    
    # Execute all test cases
    local failed_tests=0
    
    test_constitution_document || ((failed_tests++))
    echo ""
    test_mcp_protocol_first || ((failed_tests++))
    echo ""
    test_tdd_compliance || ((failed_tests++))
    echo ""
    test_spring_architecture || ((failed_tests++))
    echo ""
    test_production_ready || ((failed_tests++))
    echo ""
    test_type_safety || ((failed_tests++))
    echo ""
    test_content_visibility || ((failed_tests++))
    
    echo "=================================="
    log_info "Constitutional Compliance Test Results:"
    
    # Print all results
    for result in "${TEST_RESULTS[@]}"; do
        if [[ $result == PASS:* ]]; then
            echo -e "${GREEN}✓${NC} ${result#PASS: }"
        else
            echo -e "${RED}✗${NC} ${result#FAIL: }"
        fi
    done
    
    echo ""
    if [[ $failed_tests -eq 0 ]]; then
        log_success "All constitutional compliance tests passed!"
        log_info "The MonicaHQ MCP Server adheres to all 6 core principles"
        return 0
    else
        log_failure "$failed_tests constitutional compliance test(s) failed"
        log_info "Review the constitution at $CONSTITUTION_FILE"
        log_info "Address violations before proceeding with implementation"
        return 1
    fi
}

# Execute if run directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi