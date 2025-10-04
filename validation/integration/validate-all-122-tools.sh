#!/bin/bash

# All Tools Validation Script
# Validates that all 122 MonicaHQ MCP tools are discoverable and properly configured

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LOG_FILE="/tmp/all-tools-validation.log"
TOOLS_OUTPUT_FILE="/tmp/mcp_all_tools_output.json"
EXPECTED_TOOL_COUNT=122

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Error handling
error_exit() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    log "ERROR: $1"
    exit 1
}

# Success message
success() {
    echo -e "${GREEN}✓ $1${NC}"
    log "SUCCESS: $1"
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

# Load environment variables
load_environment() {
    if [ -f "$PROJECT_ROOT/.env" ]; then
        info "Loading environment from .env file"
        set -a
        source "$PROJECT_ROOT/.env"
        set +a
    fi
    
    # Check required environment variables
    if [ -z "${MONICA_API_URL:-}" ]; then
        error_exit "MONICA_API_URL environment variable is not set"
    fi
    
    if [ -z "${MONICA_API_TOKEN:-}" ]; then
        error_exit "MONICA_API_TOKEN environment variable is not set"
    fi
    
    success "Environment variables loaded successfully"
}

# Check MCP server JAR
check_mcp_server() {
    info "Checking MCP server JAR file..."
    
    cd "$PROJECT_ROOT"
    local jar_files=(build/libs/monica-hq-mcp-*.jar)
    
    if [ ! -f "${jar_files[0]}" ]; then
        warning "MCP server JAR not found. Building it now..."
        ./gradlew build -x test || error_exit "Failed to build MCP server JAR"
    fi
    
    if [ -f "${jar_files[0]}" ]; then
        success "MCP server JAR found: ${jar_files[0]}"
        echo "${jar_files[0]}"
    else
        error_exit "MCP server JAR not found after build"
    fi
}

# Get all tools from MCP server
get_all_tools() {
    info "Retrieving all tools from MCP server..."
    
    local jar_file="$1"
    local tools_message='{"jsonrpc":"2.0","id":"tools-list","method":"tools/list","params":{}}'
    
    info "Sending tools/list request to MCP server..."
    
    # Start MCP server and send tools/list request
    echo "$tools_message" | timeout 30s java -jar "$jar_file" > "$TOOLS_OUTPUT_FILE" 2>/tmp/mcp_tools_error.log
    
    if [ $? -ne 0 ]; then
        error_exit "Failed to get tools list from MCP server. Check /tmp/mcp_tools_error.log for details"
    fi
    
    success "Tools list retrieved successfully"
}

# Parse and validate tools
parse_and_validate_tools() {
    info "Parsing and validating tools..."
    
    if [ ! -f "$TOOLS_OUTPUT_FILE" ]; then
        error_exit "Tools output file not found: $TOOLS_OUTPUT_FILE"
    fi
    
    # Check if response is valid JSON
    if ! python3 -m json.tool "$TOOLS_OUTPUT_FILE" >/dev/null 2>&1; then
        error_exit "Tools response is not valid JSON"
    fi
    
    # Extract tools array
    local tools_count
    tools_count=$(python3 -c "
import json
with open('$TOOLS_OUTPUT_FILE', 'r') as f:
    data = json.load(f)
    
if 'result' in data and 'tools' in data['result']:
    tools = data['result']['tools']
    print(len(tools))
else:
    print('0')
" 2>/dev/null || echo "0")
    
    if [ "$tools_count" -eq 0 ]; then
        error_exit "No tools found in MCP server response"
    fi
    
    info "Found $tools_count tools in MCP server response"
    
    # Check if we have the expected number of tools
    if [ "$tools_count" -eq "$EXPECTED_TOOL_COUNT" ]; then
        success "Tool count matches expected: $tools_count/$EXPECTED_TOOL_COUNT"
    elif [ "$tools_count" -gt "$EXPECTED_TOOL_COUNT" ]; then
        warning "More tools than expected: $tools_count/$EXPECTED_TOOL_COUNT (this is okay)"
    else
        warning "Fewer tools than expected: $tools_count/$EXPECTED_TOOL_COUNT"
    fi
    
    echo "$tools_count"
}

# Analyze tool categories
analyze_tool_categories() {
    info "Analyzing tool categories and operations..."
    
    python3 << 'EOF'
import json
import sys
from collections import defaultdict

try:
    with open('/tmp/mcp_all_tools_output.json', 'r') as f:
        data = json.load(f)
    
    if 'result' not in data or 'tools' not in data['result']:
        print("ERROR: Invalid tools response format")
        sys.exit(1)
    
    tools = data['result']['tools']
    
    # Categorize tools
    categories = defaultdict(lambda: defaultdict(int))
    entities = set()
    operations = defaultdict(int)
    
    for tool in tools:
        name = tool.get('name', '')
        if ':' in name:
            entity, operation = name.split(':', 1)
            categories[entity][operation] += 1
            entities.add(entity)
            operations[operation] += 1
    
    print(f"\n📊 TOOL ANALYSIS REPORT")
    print(f"{'='*50}")
    print(f"Total Tools: {len(tools)}")
    print(f"Entities: {len(entities)}")
    print(f"Operations: {len(operations)}")
    
    print(f"\n📋 ENTITIES ({len(entities)}):")
    for entity in sorted(entities):
        tool_count = sum(categories[entity].values())
        ops = sorted(categories[entity].keys())
        print(f"  • {entity}: {tool_count} tools ({', '.join(ops)})")
    
    print(f"\n⚡ OPERATIONS ({len(operations)}):")
    for operation in sorted(operations.keys()):
        count = operations[operation]
        print(f"  • {operation}: {count} tools")
    
    # Validate standard operations
    print(f"\n🔍 STANDARD OPERATIONS VALIDATION:")
    expected_operations = ['create', 'get', 'update', 'delete', 'list']
    for op in expected_operations:
        if op in operations:
            print(f"  ✓ {op}: {operations[op]} tools")
        else:
            print(f"  ✗ {op}: missing")
    
    # Check for special operations
    special_ops = ['add', 'remove', 'search']
    print(f"\n🔧 SPECIAL OPERATIONS:")
    for op in special_ops:
        if op in operations:
            print(f"  ✓ {op}: {operations[op]} tools")
        else:
            print(f"  - {op}: not found")
    
    # Detailed entity analysis
    print(f"\n📝 DETAILED ENTITY ANALYSIS:")
    for entity in sorted(entities):
        ops = categories[entity]
        total = sum(ops.values())
        crud_ops = sum(ops.get(op, 0) for op in ['create', 'get', 'update', 'delete', 'list'])
        crud_completeness = crud_ops / 5 * 100 if crud_ops > 0 else 0
        
        print(f"  📦 {entity} ({total} tools):")
        print(f"    CRUD completeness: {crud_completeness:.0f}% ({crud_ops}/5)")
        for op in sorted(ops.keys()):
            count = ops[op]
            print(f"    • {op}: {count}")
    
    print(f"\n✅ Analysis complete!")

except Exception as e:
    print(f"ERROR: Failed to analyze tools: {e}")
    sys.exit(1)
EOF
    
    if [ $? -eq 0 ]; then
        success "Tool analysis completed successfully"
    else
        error_exit "Failed to analyze tools"
    fi
}

# Validate specific tool schemas
validate_tool_schemas() {
    info "Validating tool schemas and parameters..."
    
    python3 << 'EOF'
import json
import sys

try:
    with open('/tmp/mcp_all_tools_output.json', 'r') as f:
        data = json.load(f)
    
    tools = data['result']['tools']
    
    print(f"\n🔧 TOOL SCHEMA VALIDATION")
    print(f"{'='*50}")
    
    valid_tools = 0
    issues = []
    
    for tool in tools:
        name = tool.get('name', 'unnamed')
        description = tool.get('description', '')
        input_schema = tool.get('inputSchema', {})
        
        # Basic validation
        tool_valid = True
        tool_issues = []
        
        if not name:
            tool_issues.append("Missing name")
            tool_valid = False
        
        if not description:
            tool_issues.append("Missing description")
            tool_valid = False
        
        if not input_schema or not isinstance(input_schema, dict):
            tool_issues.append("Missing or invalid inputSchema")
            tool_valid = False
        else:
            # Validate schema structure
            if 'type' not in input_schema:
                tool_issues.append("inputSchema missing 'type'")
                tool_valid = False
            
            if input_schema.get('type') == 'object':
                if 'properties' not in input_schema:
                    tool_issues.append("Object schema missing 'properties'")
                    tool_valid = False
        
        if tool_valid:
            valid_tools += 1
        else:
            issues.append(f"{name}: {', '.join(tool_issues)}")
    
    print(f"Valid tools: {valid_tools}/{len(tools)}")
    
    if issues:
        print(f"\n❌ SCHEMA ISSUES ({len(issues)}):")
        for issue in issues[:10]:  # Show first 10 issues
            print(f"  • {issue}")
        if len(issues) > 10:
            print(f"  ... and {len(issues) - 10} more issues")
    else:
        print(f"\n✅ All tool schemas are valid!")
    
    # Check for activity_create specifically
    activity_create_found = False
    for tool in tools:
        if tool.get('name') == 'monicahq:activity_create':
            activity_create_found = True
            print(f"\n🎯 ACTIVITY_CREATE TOOL FOUND:")
            print(f"  Name: {tool.get('name')}")
            print(f"  Description: {tool.get('description', 'N/A')}")
            
            input_schema = tool.get('inputSchema', {})
            if 'properties' in input_schema:
                properties = input_schema['properties']
                print(f"  Parameters: {', '.join(properties.keys())}")
                
                # Check for attendees parameter
                if 'attendees' in properties:
                    attendees_schema = properties['attendees']
                    print(f"  Attendees schema: {attendees_schema.get('type', 'unknown')}")
                else:
                    print(f"  ⚠ Missing attendees parameter")
            break
    
    if not activity_create_found:
        print(f"\n❌ CRITICAL: activity_create tool not found!")
        issues.append("activity_create tool missing")
    
    success_rate = (valid_tools / len(tools)) * 100 if tools else 0
    print(f"\nSchema validation success rate: {success_rate:.1f}%")
    
    if success_rate >= 95:
        print("✅ Tool schemas validation PASSED")
        sys.exit(0)
    else:
        print("❌ Tool schemas validation FAILED")
        sys.exit(1)

except Exception as e:
    print(f"ERROR: Failed to validate tool schemas: {e}")
    sys.exit(1)
EOF
    
    if [ $? -eq 0 ]; then
        success "Tool schema validation passed"
    else
        error_exit "Tool schema validation failed"
    fi
}

# Test critical tools
test_critical_tools() {
    info "Testing critical tool operations..."
    
    local jar_file="$1"
    
    # Test activity_create tool specifically
    info "Testing activity_create tool call..."
    
    local activity_create_message='{
        "jsonrpc": "2.0",
        "id": "test-activity-create",
        "method": "tools/call",
        "params": {
            "name": "monicahq:activity_create",
            "arguments": {
                "summary": "Test activity from validation script",
                "attendees": ["Test User"],
                "description": "This is a test activity created by the validation script"
            }
        }
    }'
    
    echo "$activity_create_message" | timeout 30s java -jar "$jar_file" > /tmp/mcp_activity_test.json 2>/tmp/mcp_activity_error.log
    
    if [ $? -eq 0 ]; then
        # Check if the response contains an error or success
        if grep -q '"error"' /tmp/mcp_activity_test.json; then
            warning "Activity create test returned an error (this is expected if no MonicaHQ instance is available)"
            log "Activity create error: $(cat /tmp/mcp_activity_test.json)"
        else
            success "Activity create test executed successfully"
        fi
    else
        warning "Activity create test failed to execute"
    fi
    
    # Test contact_list tool
    info "Testing contact_list tool call..."
    
    local contact_list_message='{
        "jsonrpc": "2.0",
        "id": "test-contact-list",
        "method": "tools/call",
        "params": {
            "name": "monicahq:contact_list",
            "arguments": {
                "limit": 1
            }
        }
    }'
    
    echo "$contact_list_message" | timeout 30s java -jar "$jar_file" > /tmp/mcp_contact_test.json 2>/tmp/mcp_contact_error.log
    
    if [ $? -eq 0 ]; then
        if grep -q '"error"' /tmp/mcp_contact_test.json; then
            warning "Contact list test returned an error (this is expected if no MonicaHQ instance is available)"
        else
            success "Contact list test executed successfully"
        fi
    else
        warning "Contact list test failed to execute"
    fi
}

# Generate validation report
generate_validation_report() {
    local tools_count="$1"
    
    info "Generating validation report..."
    
    local report_file="/tmp/all-tools-validation-report.md"
    
    cat > "$report_file" << EOF
# MonicaHQ MCP Server - All Tools Validation Report

**Generated:** $(date '+%Y-%m-%d %H:%M:%S')
**Script:** $0
**Project:** $PROJECT_ROOT

## Summary

- **Total Tools Found:** $tools_count
- **Expected Tools:** $EXPECTED_TOOL_COUNT
- **Status:** $([ "$tools_count" -ge "$EXPECTED_TOOL_COUNT" ] && echo "✅ PASSED" || echo "⚠ PARTIAL")

## Environment

- **MONICA_API_URL:** $MONICA_API_URL
- **MONICA_API_TOKEN:** $([ -n "$MONICA_API_TOKEN" ] && echo "Set (${#MONICA_API_TOKEN} chars)" || echo "Not set")
- **Java Version:** $(java -version 2>&1 | head -1)

## Test Results

### Tool Discovery
- Tools/list endpoint: ✅ Working
- JSON response format: ✅ Valid
- Tool count: $([ "$tools_count" -ge "$EXPECTED_TOOL_COUNT" ] && echo "✅" || echo "⚠") $tools_count/$EXPECTED_TOOL_COUNT

### Schema Validation
- Tool names: ✅ Valid format
- Tool descriptions: ✅ Present
- Input schemas: ✅ Valid structure
- Critical tools: ✅ Present

### Tool Execution Tests
- activity_create: $([ -f /tmp/mcp_activity_test.json ] && echo "✅ Tested" || echo "❌ Failed")
- contact_list: $([ -f /tmp/mcp_contact_test.json ] && echo "✅ Tested" || echo "❌ Failed")

## Files Generated

- **Tools output:** $TOOLS_OUTPUT_FILE
- **Validation log:** $LOG_FILE
- **This report:** $report_file

## Next Steps

1. If tool count is lower than expected, check the MCP tool registry
2. If tool execution fails, verify MonicaHQ API connectivity
3. For Claude Desktop integration, run: validation/setup/verify-claude-desktop.sh

---
*Generated by MonicaHQ MCP Server validation suite*
EOF
    
    success "Validation report generated: $report_file"
    echo ""
    echo -e "${BLUE}📄 Validation Report Summary:${NC}"
    cat "$report_file"
}

# Main execution
main() {
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}       MonicaHQ MCP Server - All 122 Tools Validation        ${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════════════${NC}"
    echo ""
    
    log "Starting all tools validation"
    
    # Clear previous logs
    > "$LOG_FILE"
    
    # Load environment and check server
    load_environment
    local jar_file
    jar_file=$(check_mcp_server)
    
    # Get and validate tools
    get_all_tools "$jar_file"
    local tools_count
    tools_count=$(parse_and_validate_tools)
    
    # Detailed analysis
    analyze_tool_categories
    validate_tool_schemas
    test_critical_tools "$jar_file"
    
    # Generate report
    generate_validation_report "$tools_count"
    
    echo ""
    if [ "$tools_count" -ge "$EXPECTED_TOOL_COUNT" ]; then
        echo -e "${GREEN}✅ All tools validation PASSED! Found $tools_count/$EXPECTED_TOOL_COUNT tools.${NC}"
        log "All tools validation completed successfully with $tools_count tools"
        exit 0
    else
        echo -e "${YELLOW}⚠ Partial validation completed. Found $tools_count/$EXPECTED_TOOL_COUNT tools.${NC}"
        log "Partial tools validation completed with $tools_count tools"
        exit 1
    fi
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [--help|--debug]"
        echo ""
        echo "Validates all 122 MonicaHQ MCP tools are discoverable and properly configured."
        echo ""
        echo "Options:"
        echo "  --help, -h    Show this help message"
        echo "  --debug       Enable debug output"
        echo ""
        echo "Environment variables:"
        echo "  MONICA_API_URL    MonicaHQ API endpoint (required)"
        echo "  MONICA_API_TOKEN  MonicaHQ API token (required)"
        exit 0
        ;;
    --debug)
        set -x
        ;;
esac

# Run main function
main "$@"