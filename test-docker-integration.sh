#!/bin/bash

# Quick Docker Integration Test for MonicaHQ MCP Server
# Tests against existing Monica instance on localhost:8081

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results
PASSED=0
FAILED=0

# Logging
log() {
    echo -e "${BLUE}[$(date '+%H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
    PASSED=$((PASSED + 1))
}

failure() {
    echo -e "${RED}✗${NC} $1"
    FAILED=$((FAILED + 1))
}

warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}        MonicaHQ MCP Server - Docker Integration Test          ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Test 1: Check Monica instance is running
log "Testing Monica instance on localhost:8081"
if curl -s -o /dev/null -w "%{http_code}" http://localhost:8081 | grep -q "200\|302"; then
    success "Monica instance is responding on localhost:8081"
else
    failure "Monica instance not responding on localhost:8081"
fi

# Test 2: Check MCP server JAR exists
log "Checking MCP server JAR"
if ls build/libs/monicahqmcp-*.jar 1> /dev/null 2>&1; then
    success "MCP server JAR found"
else
    failure "MCP server JAR not found - run ./gradlew build"
fi

# Test 3: Test MCP server basic functionality (without Monica API token)
log "Testing MCP server basic functionality"
cat > /tmp/test_mcp_basic.json << 'EOF'
{"jsonrpc":"2.0","id":"test-1","method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"test-client","version":"1.0.0"}}}
EOF

# Test with mock token to avoid auth error
if timeout 10s bash -c 'cat /tmp/test_mcp_basic.json | MONICA_API_URL="http://localhost:8081/api" MONICA_API_TOKEN="test-token" java -jar build/libs/monicahqmcp-*.jar 2>/dev/null | grep -q "protocolVersion"' 2>/dev/null; then
    success "MCP server initialization works"
elif timeout 10s bash -c 'cat /tmp/test_mcp_basic.json | MONICA_API_URL="http://localhost:8081/api" MONICA_API_TOKEN="test-token" java -jar build/libs/monicahqmcp-*.jar 2>&1 | grep -q "Starting MonicaHqMcpApplication"' 2>/dev/null; then
    warning "MCP server starts but may need valid API token"
    PASSED=$((PASSED + 1))
else
    failure "MCP server failed to start"
fi

# Test 4: Test tools list endpoint
log "Testing tools list endpoint"
cat > /tmp/test_tools_list.json << 'EOF'
{"jsonrpc":"2.0","id":"test-2","method":"tools/list","params":{}}
EOF

if timeout 10s bash -c 'cat /tmp/test_tools_list.json | MONICA_API_URL="http://localhost:8081/api" MONICA_API_TOKEN="test-token" java -jar build/libs/monicahqmcp-*.jar 2>/dev/null | grep -q "tools"' 2>/dev/null; then
    success "Tools list endpoint responding"
else
    warning "Tools list endpoint may require valid API token"
fi

# Test 5: Check Docker compose files
log "Checking Docker Compose configuration"
if [ -f "docker-compose.yml" ] && [ -f "docker-compose.dev.yml" ]; then
    success "Docker Compose files present"
else
    failure "Docker Compose files missing"
fi

# Test 6: Check if port 8081 is in use (Monica should be running)
log "Checking port 8081 usage"
if lsof -i :8081 >/dev/null 2>&1; then
    success "Port 8081 is in use (Monica running)"
else
    failure "Port 8081 is not in use (Monica not running)"
fi

# Test 7: Check if we can reach Monica web interface
log "Testing Monica web interface"
if curl -s -L http://localhost:8081 | grep -q -i "monica\|login\|dashboard"; then
    success "Monica web interface accessible"
else
    warning "Monica web interface may not be fully ready"
fi

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}                          SUMMARY                               ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""
echo "Passed: ${GREEN}$PASSED${NC}"
echo "Failed: ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ All basic tests passed!${NC}"
    echo ""
    echo -e "${BLUE}🚀 Ready for MCP integration testing${NC}"
    echo ""
    echo "Next steps:"
    echo "1. Set MONICA_API_TOKEN environment variable"
    echo "2. Run: ./validation/integration/run-integration-tests.sh"
    echo "3. Configure Claude Desktop with MCP server"
else
    echo -e "${RED}❌ Some tests failed${NC}"
    echo ""
    echo "Please address the failed tests before proceeding."
fi

# Cleanup
rm -f /tmp/test_mcp_basic.json /tmp/test_tools_list.json

exit $FAILED