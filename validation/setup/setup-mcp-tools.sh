#!/bin/bash

# MonicaHQ MCP Server - Tool Setup and Configuration
# Sets up MCP Inspector and validation tools for comprehensive testing

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() {
    echo -e "${BLUE}ℹ️  INFO:${NC} $1"
}

success() {
    echo -e "${GREEN}✅ SUCCESS:${NC} $1"
}

warn() {
    echo -e "${YELLOW}⚠️  WARNING:${NC} $1"
}

error() {
    echo -e "${RED}❌ ERROR:${NC} $1"
}

echo "🔧 MonicaHQ MCP Server - Tool Setup"
echo "==================================="
echo ""

# Check prerequisites
info "Checking prerequisites..."

# Check Node.js/npm
if ! command -v npm >/dev/null 2>&1; then
    error "npm is required but not installed. Please install Node.js and npm first."
    exit 1
fi

# Check Java
if ! command -v java >/dev/null 2>&1; then
    error "Java is required but not installed. Please install Java 17+ first."
    exit 1
fi

# Check if project is built
if [ ! -f "build/libs/monicahqmcp-0.1.0.jar" ]; then
    info "Building MonicaHQ MCP Server..."
    ./gradlew build
fi

echo ""
echo "Installing MCP Inspector"
echo "======================="

# Install MCP Inspector globally
if command -v mcp-inspector >/dev/null 2>&1; then
    success "MCP Inspector already installed"
else
    info "Installing MCP Inspector globally..."
    if npm install -g @anthropic/mcp-inspector >/dev/null 2>&1; then
        success "MCP Inspector installed successfully"
    else
        warn "Failed to install MCP Inspector globally. Trying local installation..."
        npm install @anthropic/mcp-inspector
        echo 'alias mcp-inspector="npx mcp-inspector"' >> ~/.bashrc
        success "MCP Inspector installed locally"
    fi
fi

echo ""
echo "Setting up Command Aliases"
echo "=========================="

# Create alias configuration file
cat > mcp-aliases.sh << 'EOF'
#!/bin/bash
# MonicaHQ MCP Server - Command Aliases
# Source this file to get convenient testing commands

# Core testing commands
alias mcp-test='./test-mcp-complete.sh'
alias mcp-constitution='./validate-constitution.sh'
alias mcp-build='./gradlew build'
alias mcp-run='./run-mcp-server.sh'

# MCP Inspector shortcuts
alias mcp-inspect-jar='mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio'
alias mcp-inspect-docker='mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp'

# Development shortcuts
alias mcp-logs='tail -f logs/monicahq-mcp.log'
alias mcp-health='curl -s http://localhost:8080/actuator/health | jq'
alias mcp-tools='echo '\''{"jsonrpc":"2.0","method":"tools/list","id":1}'\'' | ./run-mcp-server.sh'

# Quick validation
alias mcp-validate='./gradlew test && ./validate-constitution.sh'
alias mcp-quick-test='echo '\''{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'\'' | ./run-mcp-server.sh'

echo "🔧 MonicaHQ MCP aliases loaded!"
echo "Available commands:"
echo "  mcp-test          - Run comprehensive test suite"
echo "  mcp-constitution  - Validate constitutional compliance"
echo "  mcp-inspect-jar   - Test with MCP Inspector (JAR)"
echo "  mcp-inspect-docker- Test with MCP Inspector (Docker)"
echo "  mcp-validate      - Quick validation (tests + constitution)"
echo "  mcp-quick-test    - Quick MCP protocol test"
echo "  mcp-tools         - List available MCP tools"
echo "  mcp-health        - Check health endpoints"
EOF

chmod +x mcp-aliases.sh
success "Command aliases created in mcp-aliases.sh"

echo ""
echo "Creating Test Configuration Files"
echo "================================="

# Create MCP Inspector configuration
cat > mcp-inspector-config.json << EOF
{
  "name": "MonicaHQ MCP Server",
  "description": "MCP server for MonicaHQ CRM integration",
  "server": {
    "command": "java",
    "args": ["-jar", "build/libs/monicahqmcp-0.1.0.jar", "--stdio"],
    "env": {
      "MONICA_API_URL": "\${MONICA_API_URL}",
      "MONICA_API_TOKEN": "\${MONICA_API_TOKEN}"
    }
  },
  "expectedTools": [
    "contact_list",
    "contact_create",
    "contact_get", 
    "note_create",
    "task_list"
  ],
  "testCases": [
    {
      "name": "List Contacts",
      "method": "tools/call",
      "params": {
        "name": "contact_list",
        "arguments": {}
      }
    },
    {
      "name": "Create Contact",
      "method": "tools/call", 
      "params": {
        "name": "contact_create",
        "arguments": {
          "firstName": "Test",
          "lastName": "User",
          "genderId": "3",
          "isBirthdateKnown": false,
          "isDeceased": false,
          "isDeceasedDateKnown": false
        }
      }
    }
  ]
}
EOF

success "MCP Inspector configuration created"

# Create Claude Desktop configuration template
cat > claude-desktop-config-template.json << 'EOF'
{
  "mcpServers": {
    "monicahq": {
      "command": "java",
      "args": [
        "-jar", 
        "/absolute/path/to/monicahqmcp-0.1.0.jar", 
        "--stdio"
      ],
      "env": {
        "MONICA_API_URL": "https://your-monica-instance.com/api",
        "MONICA_API_TOKEN": "your-oauth2-bearer-token"
      }
    }
  }
}
EOF

success "Claude Desktop configuration template created"

# Create environment template
cat > .env.template << 'EOF'
# MonicaHQ MCP Server Environment Configuration
# Copy this file to .env and fill in your values

# Required: MonicaHQ API Configuration
MONICA_API_URL=https://your-monica-instance.com/api
MONICA_API_TOKEN=your-oauth2-bearer-token

# Optional: Server Configuration
LOG_LEVEL=INFO
LOG_LEVEL_MCP=DEBUG
MONICA_API_TIMEOUT=30s
MONICA_API_MAX_RETRIES=3

# Optional: Circuit Breaker Configuration  
CIRCUIT_BREAKER_FAILURE_RATE=50
CIRCUIT_BREAKER_WAIT_DURATION=10s

# Optional: Production Settings
JAVA_OPTS=-Xmx512m -Xms256m
MCP_WEBSOCKET_MAX_SESSIONS=10
MCP_WEBSOCKET_PING_INTERVAL=30s
EOF

success "Environment template created"

echo ""
echo "Creating Quick Start Guide"
echo "========================="

cat > TESTING-GUIDE.md << 'EOF'
# MonicaHQ MCP Server - Testing Guide

## Quick Start

1. **Set up environment variables:**
   ```bash
   cp .env.template .env
   # Edit .env with your MonicaHQ details
   source .env
   ```

2. **Run comprehensive validation:**
   ```bash
   ./test-mcp-complete.sh
   ```

3. **Load convenient aliases:**
   ```bash
   source mcp-aliases.sh
   ```

## Testing Tools

### Constitutional Compliance
```bash
./validate-constitution.sh
```
Validates adherence to Constitution v1.1.0 principles.

### Comprehensive Testing
```bash
./test-mcp-complete.sh
```
Runs all validation phases:
- Constitutional compliance
- Unit/integration tests  
- MCP protocol validation
- Architecture validation
- Security and quality checks
- Deployment readiness

### MCP Inspector (Interactive)
```bash
mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio
```
Interactive web interface for testing MCP operations.

### Quick Protocol Test
```bash
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | ./run-mcp-server.sh
```

## Claude Desktop Integration

1. **Copy configuration template:**
   ```bash
   cp claude-desktop-config-template.json ~/.config/claude-desktop/config.json
   ```

2. **Update paths and credentials in the config file**

3. **Restart Claude Desktop**

## Troubleshooting

### Common Issues
- **Environment variables not set**: Check `.env` file
- **Build failures**: Run `./gradlew clean build`
- **Test failures**: Check constitutional compliance first
- **MCP connection issues**: Verify JSON-RPC 2.0 format

### Debug Mode
```bash
LOG_LEVEL=DEBUG ./run-mcp-server.sh
```

### Health Checks (Web Server Mode)
```bash
curl http://localhost:8080/actuator/health
```

## Available Aliases (after `source mcp-aliases.sh`)

- `mcp-test` - Comprehensive testing
- `mcp-constitution` - Constitutional validation  
- `mcp-inspect-jar` - MCP Inspector with JAR
- `mcp-validate` - Quick validation
- `mcp-tools` - List available tools
- `mcp-health` - Health check

## Production Deployment

1. **Validate completely:**
   ```bash
   ./test-mcp-complete.sh
   ```

2. **Build Docker image:**
   ```bash
   docker build -t monicahq-mcp .
   ```

3. **Deploy with Docker Compose:**
   ```bash
   docker-compose up -d
   ```

## Need Help?

- Check constitutional compliance: `./validate-constitution.sh`
- Run comprehensive tests: `./test-mcp-complete.sh`
- Review README.md for detailed documentation
- Use MCP Inspector for interactive debugging
EOF

success "Testing guide created (TESTING-GUIDE.md)"

echo ""
echo "========================================================"
echo "Setup Complete! 🎉"
echo "========================================================"
echo ""
echo "Tools installed and configured:"
echo "✅ MCP Inspector (global/local)"
echo "✅ Command aliases (mcp-aliases.sh)"
echo "✅ Test configuration files"
echo "✅ Claude Desktop config template"
echo "✅ Environment template"
echo "✅ Testing guide"
echo ""
echo "Next steps:"
echo "1. Copy .env.template to .env and configure your MonicaHQ details"
echo "2. Source aliases: source mcp-aliases.sh"
echo "3. Run validation: mcp-test"
echo "4. Test interactively: mcp-inspect-jar"
echo ""
echo "For Claude Desktop integration:"
echo "1. Copy claude-desktop-config-template.json to Claude Desktop config"
echo "2. Update paths and credentials"
echo "3. Restart Claude Desktop"
echo ""
echo "Documentation: TESTING-GUIDE.md"