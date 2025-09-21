#!/bin/bash

# Constitutional Validation Test: Spring Boot Architecture Excellence
# Tests compliance with Spring Boot Architecture Excellence principle from Constitution v1.1.0
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="Spring Boot Architecture Excellence"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }

main() {
    log_info "Testing: $TEST_NAME"
    cd "$PROJECT_ROOT"
    
    local failures=0
    
    # Test 1: Spring Boot 3.x version compliance
    log_info "Checking Spring Boot 3.x usage..."
    if [[ -f "build.gradle" ]]; then
        if ! grep -q "org.springframework.boot.*3\." build.gradle; then
            log_fail "Spring Boot 3.x version not found in build.gradle"
            ((failures++))
        else
            log_pass "Spring Boot 3.x dependency found"
        fi
    else
        log_fail "build.gradle not found - cannot verify Spring Boot version"
        ((failures++))
    fi
    
    # Test 2: WebFlux reactive patterns for I/O operations
    log_info "Checking WebFlux reactive implementation..."
    if ! find src -name "*.java" -exec grep -l "WebFlux\|Mono\|Flux\|@RestController" {} \; | grep -q .; then
        log_fail "WebFlux reactive patterns not found - required for external API calls"
        ((failures++))
    else
        log_pass "WebFlux reactive patterns found"
    fi
    
    # Test 3: Dual-mode architecture (STDIO + Web Server)
    log_info "Checking dual-mode architecture support..."
    # STDIO mode
    if [[ ! -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        log_fail "STDIO mode server not implemented"
        ((failures++))
    else
        log_pass "STDIO mode server implementation found"
    fi
    
    # Web server capability
    if ! find src -name "*.java" -exec grep -l "WebFlux\|WebMvcConfigurer\|@RestController" {} \; | grep -q .; then
        log_fail "Web server mode capabilities not found"
        ((failures++))
    else
        log_pass "Web server mode capabilities found"
    fi
    
    # Test 4: Dependency injection and component scanning
    log_info "Checking Spring dependency injection patterns..."
    if ! find src -name "*.java" -exec grep -l "@Component\|@Service\|@Repository\|@Controller\|@RestController" {} \; | grep -q .; then
        log_fail "Spring component annotations not found"
        ((failures++))
    else
        log_pass "Spring component patterns found"
    fi
    
    # Test 5: Circuit breaker patterns with Resilience4j
    log_info "Checking circuit breaker implementation..."
    if [[ -f "build.gradle" ]]; then
        if ! grep -q "resilience4j" build.gradle; then
            log_fail "Resilience4j dependency not found - required for circuit breaker patterns"
            ((failures++))
        else
            log_pass "Resilience4j dependency found"
        fi
    fi
    
    if ! find src -name "*.java" -exec grep -l "@CircuitBreaker\|CircuitBreaker\|resilience4j" {} \; | grep -q .; then
        log_fail "Circuit breaker pattern implementation not found"
        ((failures++))
    else
        log_pass "Circuit breaker patterns found in code"
    fi
    
    # Test 6: Proper component structure
    log_info "Checking Spring Boot component architecture..."
    local required_packages=("config" "controller" "service" "client" "dto" "mapper")
    for package in "${required_packages[@]}"; do
        if [[ ! -d "src/main/java/com/monicahq/mcp/$package" ]]; then
            log_fail "Missing required package: $package"
            ((failures++))
        else
            log_pass "Package structure found: $package"
        fi
    done
    
    # Test 7: Configuration externalization
    log_info "Checking configuration externalization..."
    if [[ ! -f "src/main/resources/application.yml" ]] && [[ ! -f "src/main/resources/application.properties" ]]; then
        log_fail "Spring Boot configuration files not found"
        ((failures++))
    else
        log_pass "Spring Boot configuration files found"
    fi
    
    echo "=================================="
    if [[ $failures -eq 0 ]]; then
        log_pass "$TEST_NAME: All tests passed"
        return 0
    else
        log_fail "$TEST_NAME: $failures test(s) failed"
        return 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi