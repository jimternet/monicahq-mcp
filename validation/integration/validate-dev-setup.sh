#!/bin/bash

# MonicaHQ MCP Server - Development Environment Validation Script
# This script sets up a local Monica instance and validates all MCP tools

set -e

echo "🧪 MonicaHQ MCP Server - Development Environment Setup"
echo "====================================================="

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

log_warning() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
}

log_error() {
    echo -e "${RED}❌ FAILED:${NC} $1"
}

# Step 1: Check prerequisites
echo
echo "Step 1: Checking Prerequisites"
echo "=============================="

if ! command -v docker &> /dev/null; then
    log_error "Docker is not installed"
    exit 1
fi
log_success "Docker is available"

if ! command -v docker-compose &> /dev/null; then
    log_error "Docker Compose is not installed"
    exit 1
fi
log_success "Docker Compose is available"

if ! command -v curl &> /dev/null; then
    log_error "curl is not installed"
    exit 1
fi
log_success "curl is available"

if ! command -v jq &> /dev/null; then
    log_warning "jq is not installed - some tests will be limited"
fi

# Step 2: Build and start services
echo
echo "Step 2: Starting Development Environment"
echo "======================================="

log_info "Building MCP server..."
./gradlew build

log_info "Starting services with docker-compose..."
docker-compose -f docker-compose.dev.yml up -d

log_info "Waiting for services to be ready..."
sleep 30

# Step 3: Wait for Monica to be ready
echo
echo "Step 3: Waiting for Monica API"
echo "=============================="

log_info "Waiting for Monica to be ready (this may take a few minutes on first run)..."
MONICA_READY=false
ATTEMPTS=0
MAX_ATTEMPTS=60

while [ $ATTEMPTS -lt $MAX_ATTEMPTS ]; do
    if curl -f -s http://localhost:8081/api > /dev/null 2>&1; then
        MONICA_READY=true
        break
    fi
    sleep 10
    ATTEMPTS=$((ATTEMPTS + 1))
    log_info "Attempt $ATTEMPTS/$MAX_ATTEMPTS - Monica still starting..."
done

if [ "$MONICA_READY" = false ]; then
    log_error "Monica API did not become ready in time"
    echo "Check logs with: docker-compose -f docker-compose.dev.yml logs monica"
    exit 1
fi

log_success "Monica API is ready at http://localhost:8081"

# Step 4: Wait for MCP server to be ready
echo
echo "Step 4: Waiting for MCP Server"
echo "============================="

log_info "Waiting for MCP server to be ready..."
MCP_READY=false
ATTEMPTS=0
MAX_ATTEMPTS=30

while [ $ATTEMPTS -lt $MAX_ATTEMPTS ]; do
    if curl -f -s http://localhost:8080/actuator/health > /dev/null 2>&1; then
        MCP_READY=true
        break
    fi
    sleep 5
    ATTEMPTS=$((ATTEMPTS + 1))
    log_info "Attempt $ATTEMPTS/$MAX_ATTEMPTS - MCP server still starting..."
done

if [ "$MCP_READY" = false ]; then
    log_error "MCP server did not become ready in time"
    echo "Check logs with: docker-compose -f docker-compose.dev.yml logs monicahq-mcp"
    exit 1
fi

log_success "MCP server is ready at http://localhost:8080"

# Step 5: Set up Monica API token
echo
echo "Step 5: Setting up Monica API Token"
echo "==================================="

log_warning "You need to manually create an API token in Monica:"
echo "1. Open http://localhost:8081 in your browser"
echo "2. Create an account or log in"
echo "3. Go to Settings > API"
echo "4. Create a new API token"
echo "5. Copy the token"
echo
echo "Then run the validation tests with:"
echo "  MONICA_API_TOKEN=your-token-here ./validate-tools.sh"

echo
echo "Development environment is ready!"
echo "================================"
echo "- Monica Web UI:     http://localhost:8081"
echo "- MCP Server:        http://localhost:8080"
echo "- MCP Inspector:     http://localhost:3000"
echo
echo "To stop the environment:"
echo "  docker-compose -f docker-compose.dev.yml down"
echo
echo "To view logs:"
echo "  docker-compose -f docker-compose.dev.yml logs -f [service-name]"