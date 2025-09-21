#!/bin/bash

# Unit Tests: Constitutional Validation Functions
# Tests individual validation functions used in constitutional compliance
# Follows Constitution v1.1.0 requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="Constitutional Validators Unit Tests"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }

# Test tracking
TESTS_RUN=0
FAILURES=0

run_test() {
    ((TESTS_RUN++))
    if $1; then
        log_pass "$2"
    else
        log_fail "$2"
        ((FAILURES++))
    fi
}

main() {
    log_info "Testing: $TEST_NAME"
    cd "$PROJECT_ROOT"
    
    # Unit Test 1: JSON-RPC 2.0 Detection Function
    test_jsonrpc_detection() {
        # Test that we can detect JSON-RPC 2.0 usage
        if find src -name "*.java" -exec grep -l '"jsonrpc".*"2\.0"' {} \; | grep -q .; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 2: STDOUT Contamination Detection Function
    test_stdout_contamination_detection() {
        # Test that we can detect System.out.print calls
        if ! grep -r "System\.out\.print" src/main/java/com/monicahq/mcp/ >/dev/null 2>&1; then
            return 0  # No contamination found (good)
        else
            return 1  # Contamination found (bad)
        fi
    }
    
    # Unit Test 3: STDERR Logging Configuration Validation
    test_stderr_logging_validation() {
        # Test that STDERR logging is properly configured
        if [[ -f "src/main/resources/logback-stdio.xml" ]] && \
           grep -q "System.err" "src/main/resources/logback-stdio.xml"; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 4: Spring Boot Version Detection Function
    test_spring_boot_version_detection() {
        # Test that we can detect Spring Boot 3.x usage
        if [[ -f "build.gradle" ]] && grep -q "org.springframework.boot.*3\." build.gradle; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 5: WebFlux Usage Detection Function
    test_webflux_detection() {
        # Test that we can detect WebFlux usage
        if find src -name "*.java" -exec grep -l "WebClient\|WebFlux\|Mono\|Flux" {} \; | grep -q .; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 6: MapStruct Usage Detection Function
    test_mapstruct_detection() {
        # Test that we can detect MapStruct usage
        if find src -name "*.java" -exec grep -l "@Mapper\|MapStruct" {} \; | grep -q .; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 7: Lombok Usage Detection Function
    test_lombok_detection() {
        # Test that we can detect Lombok usage
        if find src -name "*.java" -exec grep -l "@Data\|@Getter\|@Setter" {} \; | grep -q .; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 8: Docker Configuration Validation Function
    test_docker_config_validation() {
        # Test that Docker configuration exists
        if [[ -f "Dockerfile" ]]; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 9: Environment Variable Detection Function
    test_env_var_detection() {
        # Test that environment variables are properly configured
        if grep -r "MONICA_API" README.md src/ >/dev/null 2>&1; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 10: Test Count Validation Function
    test_test_count_validation() {
        # Test that we have adequate test coverage
        local contract_tests=$(find src/test/java/com/monicahq/mcp/contract -name "*.java" 2>/dev/null | wc -l)
        local integration_tests=$(find src/test/java/com/monicahq/mcp/integration -name "*.java" 2>/dev/null | wc -l)
        local total_tests=$((contract_tests + integration_tests))
        
        if [[ $total_tests -ge 40 ]]; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 11: Circuit Breaker Detection Function
    test_circuit_breaker_detection() {
        # Test that circuit breaker patterns are implemented
        if find src -name "*.java" -exec grep -l "@CircuitBreaker\|resilience4j" {} \; | grep -q .; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 12: OAuth2 Implementation Detection Function
    test_oauth2_detection() {
        # Test that OAuth2 Bearer token authentication is implemented
        if find src -name "*.java" -exec grep -l "Bearer\|Authorization.*token" {} \; | grep -q .; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 13: Validation Script Existence Function
    test_validation_script_existence() {
        # Test that validation scripts exist
        if [[ -f "validate-constitution.sh" ]] && [[ -f "test-mcp-complete.sh" ]]; then
            return 0
        else
            return 1
        fi
    }
    
    # Unit Test 14: Constitutional Test Structure Validation
    test_constitutional_test_structure() {
        # Test that constitutional test structure exists
        local constitutional_tests=(
            "tests/constitutional/test_mcp_protocol_first.sh"
            "tests/constitutional/test_tdd_compliance.sh"
            "tests/constitutional/test_spring_architecture.sh"
            "tests/constitutional/test_production_ready.sh"
            "tests/constitutional/test_type_safety.sh"
        )
        
        for test_file in "${constitutional_tests[@]}"; do
            if [[ ! -f "$test_file" ]]; then
                return 1
            fi
        done
        return 0
    }
    
    # Unit Test 15: Constitution Document Validation Function
    test_constitution_document_validation() {
        # Test that constitution document is properly populated
        if [[ -f ".specify/memory/constitution.md" ]] && \
           grep -q "Version.*1.1.0" ".specify/memory/constitution.md" && \
           ! grep -q "\[VERSION\]" ".specify/memory/constitution.md"; then
            return 0
        else
            return 1
        fi
    }
    
    # Execute all unit tests
    log_info "Running constitutional validator unit tests..."
    echo "========================================"
    
    run_test test_jsonrpc_detection "JSON-RPC 2.0 detection function"
    run_test test_stdout_contamination_detection "STDOUT contamination detection function"
    run_test test_stderr_logging_validation "STDERR logging validation function"
    run_test test_spring_boot_version_detection "Spring Boot version detection function"
    run_test test_webflux_detection "WebFlux usage detection function"
    run_test test_mapstruct_detection "MapStruct usage detection function"
    run_test test_lombok_detection "Lombok usage detection function"
    run_test test_docker_config_validation "Docker configuration validation function"
    run_test test_env_var_detection "Environment variable detection function"
    run_test test_test_count_validation "Test count validation function"
    run_test test_circuit_breaker_detection "Circuit breaker detection function"
    run_test test_oauth2_detection "OAuth2 implementation detection function"
    run_test test_validation_script_existence "Validation script existence function"
    run_test test_constitutional_test_structure "Constitutional test structure validation"
    run_test test_constitution_document_validation "Constitution document validation function"
    
    echo "========================================"
    if [[ $FAILURES -eq 0 ]]; then
        log_pass "$TEST_NAME: All unit tests passed ($TESTS_RUN/$TESTS_RUN)"
        log_info "All constitutional validation functions are working correctly"
        return 0
    else
        log_fail "$TEST_NAME: $FAILURES unit test(s) failed ($((TESTS_RUN - FAILURES))/$TESTS_RUN passed)"
        log_info "Constitutional validation functions need attention"
        return 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi