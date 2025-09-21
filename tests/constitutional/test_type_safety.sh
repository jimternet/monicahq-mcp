#!/bin/bash

# Constitutional Validation Test: Type Safety and Code Generation
# Tests compliance with Type Safety and Code Generation principle from Constitution v1.1.0
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="Type Safety and Code Generation"

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
    
    # Test 1: MapStruct type-safe mapping implementation
    log_info "Checking MapStruct implementation..."
    if [[ -f "build.gradle" ]]; then
        if ! grep -q "mapstruct" build.gradle; then
            log_fail "MapStruct dependency not found in build.gradle"
            ((failures++))
        else
            log_pass "MapStruct dependency found"
        fi
    else
        log_fail "build.gradle not found - cannot verify MapStruct dependency"
        ((failures++))
    fi
    
    # Check for MapStruct usage in code
    if ! find src -name "*.java" -exec grep -l "@Mapper\|MapStruct" {} \; | grep -q .; then
        log_fail "MapStruct @Mapper annotations not found in source code"
        ((failures++))
    else
        log_pass "MapStruct mapper implementations found"
    fi
    
    # Test 2: Lombok code generation
    log_info "Checking Lombok implementation..."
    if [[ -f "build.gradle" ]]; then
        if ! grep -q "lombok" build.gradle; then
            log_fail "Lombok dependency not found in build.gradle"
            ((failures++))
        else
            log_pass "Lombok dependency found"
        fi
    fi
    
    # Check for Lombok annotations
    if ! find src -name "*.java" -exec grep -l "@Data\|@Getter\|@Setter\|@Builder\|@AllArgsConstructor\|@NoArgsConstructor" {} \; | grep -q .; then
        log_fail "Lombok annotations not found in source code"
        ((failures++))
    else
        log_pass "Lombok annotations found in source code"
    fi
    
    # Test 3: DTO structure and type safety
    log_info "Checking DTO structure..."
    if [[ ! -d "src/main/java/com/monicahq/mcp/dto" ]]; then
        log_fail "DTO package not found - type safety structure missing"
        ((failures++))
    else
        local dto_files=$(find "src/main/java/com/monicahq/mcp/dto" -name "*.java" | wc -l)
        if [[ $dto_files -gt 0 ]]; then
            log_pass "DTO package found with $dto_files DTO classes"
        else
            log_fail "DTO package exists but contains no DTO classes"
            ((failures++))
        fi
    fi
    
    # Test 4: Mapper package structure
    log_info "Checking mapper structure..."
    if [[ ! -d "src/main/java/com/monicahq/mcp/mapper" ]]; then
        log_fail "Mapper package not found - type-safe mapping structure missing"
        ((failures++))
    else
        local mapper_files=$(find "src/main/java/com/monicahq/mcp/mapper" -name "*.java" | wc -l)
        if [[ $mapper_files -gt 0 ]]; then
            log_pass "Mapper package found with $mapper_files mapper classes"
        else
            log_fail "Mapper package exists but contains no mapper classes"
            ((failures++))
        fi
    fi
    
    # Test 5: Strong typing in API interactions
    log_info "Checking API interaction typing..."
    # Look for generic Object usage which indicates weak typing
    if find src -name "*.java" -exec grep -l "Object\[\]\|Map<String, Object>" {} \; | grep -v test | grep -q .; then
        log_fail "Weak typing detected - using generic Object types in API interactions"
        ((failures++))
    else
        log_pass "No weak typing patterns detected"
    fi
    
    # Test 6: Validation annotations
    log_info "Checking validation implementation..."
    if ! find src -name "*.java" -exec grep -l "@Valid\|@NotNull\|@NotEmpty\|@Size\|@Pattern" {} \; | grep -q .; then
        log_fail "Bean validation annotations not found - validation not implemented"
        ((failures++))
    else
        log_pass "Bean validation annotations found"
    fi
    
    # Test 7: Type-safe configuration
    log_info "Checking type-safe configuration..."
    if ! find src -name "*.java" -exec grep -l "@ConfigurationProperties\|@Value" {} \; | grep -q .; then
        log_fail "Type-safe configuration not implemented"
        ((failures++))
    else
        log_pass "Type-safe configuration found"
    fi
    
    # Test 8: Generic type usage
    log_info "Checking generic type usage..."
    # Look for raw types which indicate type safety violations
    if find src -name "*.java" -exec grep -l "List [a-z]\|Map [a-z]\|Set [a-z]" {} \; | grep -v test | grep -q .; then
        log_fail "Raw types detected - violates type safety principle"
        ((failures++))
    else
        log_pass "No raw types detected - proper generic usage"
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