#!/bin/bash

# Constitutional Validation Test: Production-Ready Deployment
# Tests compliance with Production-Ready Deployment principle from Constitution v1.1.0
# MUST FAIL initially per TDD requirements

set -e

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_NAME="Production-Ready Deployment"

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
    
    # Test 1: Docker containerization support
    log_info "Checking Docker containerization..."
    if [[ ! -f "Dockerfile" ]]; then
        log_fail "Dockerfile not found - Docker containerization not implemented"
        ((failures++))
    else
        log_pass "Dockerfile found for containerization"
        
        # Check Dockerfile quality
        if ! grep -q "FROM.*openjdk\|FROM.*eclipse-temurin" Dockerfile; then
            log_fail "Dockerfile does not use appropriate Java base image"
            ((failures++))
        else
            log_pass "Dockerfile uses appropriate Java base image"
        fi
    fi
    
    # Test 2: Environment variable configuration
    log_info "Checking environment variable configuration..."
    local required_env_vars=("MONICA_API_URL" "MONICA_API_TOKEN")
    local env_config_found=false
    
    for env_var in "${required_env_vars[@]}"; do
        if grep -r "$env_var" src/ README.md Dockerfile 2>/dev/null | grep -q .; then
            log_pass "Environment variable $env_var configured"
            env_config_found=true
        else
            log_fail "Environment variable $env_var not configured"
            ((failures++))
        fi
    done
    
    # Test 3: Health checks and monitoring
    log_info "Checking health check implementation..."
    if [[ -f "build.gradle" ]] && grep -q "spring-boot-starter-actuator" build.gradle; then
        log_pass "Spring Boot Actuator found for health checks"
    else
        log_fail "Spring Boot Actuator not configured - health checks missing"
        ((failures++))
    fi
    
    # Test 4: Logging configuration for production
    log_info "Checking production logging configuration..."
    if [[ -f "src/main/resources/logback-spring.xml" ]] || [[ -f "src/main/resources/logback.xml" ]]; then
        log_pass "Logback configuration found"
    else
        log_fail "Production logging configuration not found"
        ((failures++))
    fi
    
    # Test 5: Claude Desktop integration readiness
    log_info "Checking Claude Desktop integration..."
    if [[ ! -f "src/main/java/com/monicahq/mcp/McpStdioServer.java" ]]; then
        log_fail "Claude Desktop STDIO server not implemented"
        ((failures++))
    else
        log_pass "Claude Desktop STDIO server found"
    fi
    
    # Check for claude_desktop_config.json template or documentation
    if [[ -f "claude_desktop_config.json" ]] || grep -q "claude_desktop_config" README.md 2>/dev/null; then
        log_pass "Claude Desktop configuration documented"
    else
        log_fail "Claude Desktop configuration not documented"
        ((failures++))
    fi
    
    # Test 6: Production deployment scripts
    log_info "Checking deployment automation..."
    if [[ -f "docker-compose.yml" ]] || [[ -f ".github/workflows/deploy.yml" ]] || [[ -f "deploy.sh" ]]; then
        log_pass "Deployment automation found"
    else
        log_fail "Deployment automation not implemented"
        ((failures++))
    fi
    
    # Test 7: Security configuration
    log_info "Checking security best practices..."
    # Check for secrets management
    if grep -r "password\|secret\|key" src/ | grep -v "\.class" | grep -v "test" | grep "=" | grep -q .; then
        log_fail "Potential hardcoded secrets found in source code"
        ((failures++))
    else
        log_pass "No hardcoded secrets detected in source code"
    fi
    
    # Test 8: Performance and resource configuration
    log_info "Checking resource configuration..."
    if [[ -f "src/main/resources/application.yml" ]] || [[ -f "src/main/resources/application.properties" ]]; then
        if grep -q "server.port\|management.endpoints\|logging.level" src/main/resources/application.* 2>/dev/null; then
            log_pass "Application configuration found"
        else
            log_fail "Application configuration incomplete"
            ((failures++))
        fi
    else
        log_fail "Application configuration files not found"
        ((failures++))
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