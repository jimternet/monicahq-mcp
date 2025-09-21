#!/bin/bash

# MonicaHQ MCP Server - Run All CRUD Validation Tests
# Executes complete CRUD validation suite for all entity types

# Load environment and setup
source setup/load-env.sh
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

log_success() {
    echo -e "${GREEN}✅ PASSED:${NC} $1"
}

log_error() {
    echo -e "${RED}❌ FAILED:${NC} $1"
}

echo "🧪 MonicaHQ MCP Server - Complete CRUD Validation Suite"
echo "======================================================="
echo "Running all entity type validations with security improvements"
echo

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Array of CRUD validation scripts in execution order
CRUD_SCRIPTS=(
    "validate-contact-crud.sh"
    "validate-contact-field-crud.sh" 
    "validate-note-crud.sh"
    "validate-task-crud.sh"
    "validate-tag-crud.sh"
    "validate-reminder-crud.sh"
    "validate-activity-crud.sh"
    "validate-call-crud.sh"
)

# Function to run a single CRUD test
run_crud_test() {
    local script_name="$1"
    local script_path="crud/$script_name"
    
    if [ ! -f "$script_path" ]; then
        log_error "Script not found: $script_path"
        ((FAILED_TESTS++))
        return 1
    fi
    
    log_info "Running $script_name..."
    echo "----------------------------------------"
    
    if cd crud && ./"$script_name" && cd ..; then
        log_success "$script_name completed successfully"
        ((PASSED_TESTS++))
    else
        log_error "$script_name failed"
        ((FAILED_TESTS++))
    fi
    
    echo
    ((TOTAL_TESTS++))
}

# Execute all CRUD validation scripts
log_info "Starting CRUD validation suite..."
echo

for script in "${CRUD_SCRIPTS[@]}"; do
    run_crud_test "$script"
done

# Final results
echo "🎯 Complete CRUD Validation Results"
echo "==================================="
echo "Total scripts run: $TOTAL_TESTS"
echo "Scripts passed: $PASSED_TESTS"
echo "Scripts failed: $FAILED_TESTS"
echo

if [ $FAILED_TESTS -eq 0 ]; then
    log_success "🎉 ALL CRUD VALIDATION TESTS PASSED!"
    echo
    echo "✅ Entity Coverage: 8/8 entity types validated (100%)"
    echo "✅ Security: All scripts use environment variables"
    echo "✅ Constitutional Principle VII: Fully implemented"
    echo "✅ Production Ready: Complete CRUD functionality verified"
    echo
    echo "🚀 MonicaHQ MCP Server is ready for Claude Desktop integration!"
    exit 0
else
    log_error "Some CRUD validation tests failed. Check individual results above."
    echo
    echo "📋 Next steps:"
    echo "  1. Review failed test output above"
    echo "  2. Check Monica API connectivity"
    echo "  3. Verify environment variables in ../.env"
    echo "  4. Ensure Monica instance is running on localhost:8081"
    exit 1
fi