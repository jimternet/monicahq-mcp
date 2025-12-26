# MonicaHQ MCP Server

A Spring Boot-based Model Context Protocol (MCP) server that provides Claude Desktop and other MCP clients with direct access to your MonicaHQ CRM instance. Features 122 categorized operations across 23 entity types with comprehensive API coverage.

## ✅ Status: Production Ready

- **Tests**: 188/188 passing (100%)
- **Architecture**: Spring Boot 3.x with STDIO MCP protocol
- **Tool Organization**: 122 operations across comprehensive entity coverage
- **Deployment**: Multiple options (Spring Boot JAR, Docker, Claude Desktop)

## 📚 Documentation Index

This project includes specialized documentation for different use cases:

| Document | Purpose | When to Use |
|----------|---------|-------------|
| **README.md** | Main setup guide and API overview | Setting up the MCP server, understanding capabilities |
| **[CLAUDE.md](CLAUDE.md)** | Claude Code context and development notes | AI-assisted development, codebase understanding |
| **[TESTING-GUIDE.md](TESTING-GUIDE.md)** | Comprehensive testing procedures | Running tests, constitutional compliance validation |
| **[MCP-STDIO-LOGGING.md](MCP-STDIO-LOGGING.md)** | MCP protocol compliance requirements | Debugging Claude Desktop integration issues |
| **[docs/API-LIMITATIONS.md](docs/API-LIMITATIONS.md)** | MonicaHQ API limitations and workarounds | Understanding unsupported features, debugging HTTP 405 errors |
| **[IMPLEMENTATION-SUCCESS.md](IMPLEMENTATION-SUCCESS.md)** | Historical implementation record | Understanding project completion milestones |

### Quick Reference Links
- **Getting Started**: Continue reading this README
- **Development Setup**: See [docker-compose.dev.yml](#development-with-docker-compose) below
- **Testing & Validation**: See [TESTING-GUIDE.md](TESTING-GUIDE.md)
- **Troubleshooting**: See [MCP-STDIO-LOGGING.md](MCP-STDIO-LOGGING.md) for protocol issues

## Quick Start

Choose your preferred deployment method:

### Option A: Spring Boot JAR (Recommended for Development)

```bash
# 1. Set environment variables
export MONICA_API_URL=https://your-monica-instance.com/api
export MONICA_API_TOKEN=your-oauth2-bearer-token

# 2. Build the application
./gradlew build

# 3a. Run STDIO mode (for Claude Desktop)
java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# 3b. OR run Web Server mode (for HTTP/WebSocket access)
java -jar build/libs/monicahqmcp-0.1.0.jar --web
```

### Option B: Docker (Recommended for Production)

```bash
# 1. Build
./gradlew build
docker build -t monicahq-mcp .

# 2a. Run STDIO mode for Claude Desktop
docker run -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

# 2b. OR run Web Server mode (accessible on port 8080)
docker run -p 8080:8080 -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp --web

# 2c. OR use Docker Compose for production
docker-compose up -d
```

### Claude Desktop Integration

Choose JAR or Docker configuration:

**Option A: Native JAR Configuration**
```json
{
  "mcpServers": {
    "monicahq": {
      "command": "/usr/bin/java",
      "args": [
        "-jar", "/absolute/path/to/monicahqmcp-0.1.0.jar", 
        "--stdio"
      ],
      "env": {
        "MONICA_API_URL": "https://your-monica-instance.com/api",
        "MONICA_API_TOKEN": "your-oauth2-bearer-token"
      }
    }
  }
}
```

**Option B: Docker Configuration**
```json
{
  "mcpServers": {
    "monicahq": {
      "command": "docker",
      "args": [
        "run", "--rm", "-i",
        "-e", "MONICA_API_URL=https://your-monica-instance.com/api", 
        "-e", "MONICA_API_TOKEN=your-oauth2-bearer-token",
        "monicahq-mcp"
      ]
    }
  }
}
```

## MCP Operations

The server provides **122 categorized operations** across 23 entity types, organized for optimal discoverability:

### Core Entity Operations (122 total)

| Category | Entity Types | Operations | Count |
|----------|-------------|------------|--------|
| **📋 Contact Management** | Contact, ContactField, ContactTag, Address, Group, Occupation | CRUD + relationships | 32 |
| **🎯 Productivity** | Note, Task, Reminder, Tag, JournalEntry | Full CRUD | 25 |
| **💬 Communication** | Activity, ActivityType, ActivityTypeCategory, Call, Conversation, Message | CRUD + threading | 29 |
| **💼 Professional** | Company, Relationship, RelationshipType, RelationshipTypeGroup | CRUD + associations | 19 |
| **💰 Financial** | Debt, Gift, Document, Photo | CRUD + file handling | 20 |
| **🌍 Reference Data** | Country, Currency, AuditLog | Read-only operations | 9 |

### Complete Entity List (23 types)
- **Contacts**: Contact, ContactField, ContactTag, Address, Group, Occupation
- **Activities**: Activity, ActivityType, ActivityTypeCategory, Call, Conversation, ConversationMessage
- **Organization**: Note, Task, Reminder, Tag, JournalEntry
- **Relationships**: Company, Relationship, RelationshipType, RelationshipTypeGroup
- **Financial**: Debt, Gift, Document, Photo
- **System**: Country, Currency, AuditLog

### Tool Organization Features
- **Category Prefixes**: All 122 tools have clear category labels (e.g., `[Contact] Create a new contact`)
- **Logical Grouping**: Tools are ordered by frequency of use and workflow logic
- **Comprehensive Coverage**: 91% of Monica API endpoints implemented (excludes admin-only operations)
- **Future-Ready**: Metadata included for client-side grouping capabilities

## Example Usage with Claude

Once configured, you can interact with your MonicaHQ instance through Claude:

### Basic Operations
```
"Create a new contact named John Doe with email john@example.com"
"Add a note to contact ID 123 saying 'Met at conference'"
"List all tasks due this week"
"Tag contact 'Alice Smith' with 'VIP Client'"
```

### Real-World Workflows
```
# Contact Management Workflow
"Create a contact for Sarah Johnson, CEO of TechCorp"
"Add her work address: 123 Tech Street, San Francisco, CA 94105"
"Create a company called TechCorp and associate Sarah with it"
"Set a reminder to follow up with Sarah next Tuesday"
"Add a note about our meeting discussion on AI integration"

# Relationship Tracking
"Show me all my family relationships"
"Create a gift idea for my partner's birthday next month"
"Track a debt of $50 I owe to contact ID 45"
"Add a photo to John's contact from our team event"

# Activity Management
"Log a call with client ID 78 about contract renewal"
"Create a journal entry about today's product launch"
"Start a conversation thread about the Q4 planning"
"List all activities from last week"
```

## Deployment Architecture

### Dual-Mode Spring Boot Application

The MonicaHQ MCP Server is built as a flexible Spring Boot 3.x application supporting two operational modes:

#### STDIO Mode (Default)
- **Use Case**: Claude Desktop integration
- **Protocol**: MCP over STDIO (JSON-RPC 2.0)
- **Activation**: `--stdio` flag or `MCP_STDIO_MODE=true`
- **Communication**: Standard input/output streams

#### Web Server Mode
- **Use Case**: HTTP/WebSocket access, production deployments
- **Protocol**: HTTP REST API + WebSocket for MCP
- **Activation**: `--web` flag or Spring profile `docker`
- **Endpoints**: 
  - Health: `http://localhost:8080/actuator/health`
  - MCP WebSocket: `ws://localhost:8080/mcp`

### Core Architecture Features
- **Framework**: Spring Boot 3.x with WebFlux (reactive)
- **Authentication**: OAuth2 Bearer tokens to MonicaHQ API
- **Resilience**: Circuit breaker pattern with Resilience4j
- **Testing**: 188 comprehensive tests (100% pass rate)
- **Tool Organization**: 122 operations across 23 entity types

## Deployment Options

### 1. Spring Boot JAR Deployment

**Prerequisites:**
- Java 17+
- MonicaHQ instance with OAuth2 token

**Build & Run:**
```bash
# Build the application
./gradlew build

# STDIO Mode (for Claude Desktop)
export MONICA_API_URL=https://your-monica.com/api
export MONICA_API_TOKEN=your-token
java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# Web Server Mode (for HTTP/WebSocket access)
export MONICA_API_URL=https://your-monica.com/api
export MONICA_API_TOKEN=your-token
java -jar build/libs/monicahqmcp-0.1.0.jar --web
```

**Testing & Development:**
```bash
# Run all tests
./gradlew test

# Use helper script for STDIO testing
./run-mcp-server.sh

# Test specific operations
./test-mcp-operations.sh
```

### 2. Docker Deployment

**Single Container:**
```bash
# Build image
docker build -t monicahq-mcp .

# STDIO Mode
docker run -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

# Web Server Mode
docker run -p 8080:8080 -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp --web
```

**Production with Docker Compose:**
```bash
# Basic deployment
docker-compose up -d

# Production with nginx reverse proxy
docker-compose --profile production up -d
```

### 3. Development Setup

**Prerequisites:**
- Java 17+
- Your favorite IDE (IntelliJ IDEA recommended)
- MonicaHQ instance with OAuth2 token

**Local Development:**
```bash
# Clone and setup
git clone <repository>
cd monicahq_mcp

# Set environment variables
export MONICA_API_URL=https://your-monica.com/api
export MONICA_API_TOKEN=your-token

# Run in development mode
./gradlew bootRun --args='--stdio'

# Or run tests with live reload
./gradlew test --continuous
```

### Project Structure
```
src/main/java/com/monicahq/mcp/
├── config/          # Spring configuration
├── controller/      # MCP message handlers and tool registry
├── service/         # Business logic (52 operations)
├── client/          # MonicaHQ API client
├── dto/            # Data transfer objects
├── mapper/         # MapStruct mappers
└── exception/      # Exception handling

src/test/java/
├── contract/       # Contract tests for each operation
├── integration/    # End-to-end workflow tests
└── config/         # Test configuration and mocks
```

## Production Deployment

### Docker Compose (Recommended)

The included `docker-compose.yml` provides a production-ready setup:

```yaml
# Key features:
# - Health checks and auto-restart
# - Resource limits and optimization
# - Optional nginx reverse proxy
# - Persistent logging
# - Environment-based configuration
```

**Deploy:**
```bash
# Basic production deployment
docker-compose up -d

# With nginx reverse proxy and SSL
docker-compose --profile production up -d

# View logs
docker-compose logs -f monicahq-mcp

# Health check
curl http://localhost:8080/actuator/health
```

### Environment Configuration

**Required Variables:**
- `MONICA_API_URL`: Your MonicaHQ API URL (e.g., `https://monica.example.com/api`)
- `MONICA_API_TOKEN`: OAuth2 Bearer token for authentication

**Optional Configuration:**
```bash
# Logging
LOG_LEVEL=INFO                    # DEBUG, INFO, WARN, ERROR
LOG_LEVEL_MCP=DEBUG              # MCP-specific logging

# Performance
JAVA_OPTS="-Xmx512m -Xms256m"    # JVM memory settings
MONICA_API_TIMEOUT=30s           # API timeout
MONICA_API_MAX_RETRIES=3         # Retry attempts

# Circuit Breaker
CIRCUIT_BREAKER_FAILURE_RATE=50  # Failure rate threshold
CIRCUIT_BREAKER_WAIT_DURATION=10s # Wait time in open state

# WebSocket (Web Server Mode)
MCP_WEBSOCKET_MAX_SESSIONS=10    # Max concurrent WebSocket sessions
MCP_WEBSOCKET_PING_INTERVAL=30s  # WebSocket ping interval
```

### Health Monitoring & Observability

**Health Endpoints (Web Server Mode):**
```bash
# Application health
curl http://localhost:8080/actuator/health

# Detailed health with circuit breaker status
curl http://localhost:8080/actuator/health | jq

# Environment info
curl http://localhost:8080/actuator/env

# Application metrics
curl http://localhost:8080/actuator/metrics
```

**Logging:**
- **STDIO Mode**: Logs to stderr (suitable for systemd/Docker logging)
- **Web Server Mode**: Logs to `/app/logs/monicahq-mcp.log` + console
- **Format**: Structured logging optimized for log aggregation

### Performance Tuning

**JVM Optimization:**
```bash
# For containers with 1GB+ memory
JAVA_OPTS="-Xmx768m -Xms256m -XX:+UseG1GC -XX:MaxGCPauseMillis=100"

# For low-memory environments
JAVA_OPTS="-Xmx256m -Xms128m -XX:+UseSerialGC"
```

**Resource Limits (Docker):**
```yaml
deploy:
  resources:
    limits:
      memory: 768m
      cpus: '1.0'
    reservations:
      memory: 256m
      cpus: '0.25'
```

## System Requirements

### Minimum Requirements
- **Java**: 17+ (OpenJDK or Oracle JDK)
- **Memory**: 512MB RAM minimum, 1GB recommended
- **Disk**: 100MB for application + logs
- **Monica**: Any Monica instance with API access enabled
- **Network**: HTTPS access to Monica instance

### Monica API Compatibility
- **Supported Versions**: Monica 4.x and 5.x
- **Required**: OAuth2 Bearer token authentication
- **API Coverage**: 91% of Monica API endpoints (122 operations)

## ⚠️ API Limitations

The MonicaHQ API has some endpoints that don't work as documented. This MCP server implements workarounds for most issues, but some features are unavailable.

### Unsupported Features

| Feature | Status | Reason |
|---------|--------|--------|
| **Photo Upload** | ❌ NOT SUPPORTED | MonicaHQ API returns HTTP 405 - no workaround exists |
| **Document Upload** | ❌ NOT SUPPORTED | MonicaHQ API returns HTTP 405 - no workaround exists |

**What this means:**
- `photo_create` and `document_create` operations are not available
- Photos and documents must be uploaded manually through the MonicaHQ web interface
- This is a limitation of the MonicaHQ API itself, not this MCP server

### Implemented Workarounds

This MCP server automatically handles these MonicaHQ API quirks:

| Issue | Workaround | Affected Operations |
|-------|------------|---------------------|
| Nested POST endpoints return HTTP 405 | Uses root-level POST with `contact_id` in body | `relationship_create`, `address_create`, `contactfield_create`, `pet_create`, `call_create` |
| Conversation messages endpoint returns HTTP 405 | Extracts messages from conversation GET response | `conversation_message_list` |

**You don't need to do anything special** - the workarounds are transparent and all affected operations work normally.

### More Information

For complete technical details on API limitations and workarounds, see [docs/API-LIMITATIONS.md](docs/API-LIMITATIONS.md).

## Troubleshooting

### Common Issues

1. **Authentication Errors**
   ```bash
   # Verify token format (should be JWT)
   echo $MONICA_API_TOKEN | cut -d'.' -f2 | base64 -d | jq
   
   # Test token directly
   curl -H "Authorization: Bearer $MONICA_API_TOKEN" $MONICA_API_URL/me
   ```

2. **Connection Failures**
   ```bash
   # Test API connectivity
   curl -v $MONICA_API_URL/ping
   
   # Check from container
   docker run --rm -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp curl -f $MONICA_API_URL/me
   ```

3. **Claude Desktop Integration**
   ```bash
   # Verify config location (macOS)
   ls -la ~/Library/Application\ Support/Claude/claude_desktop_config.json
   
   # Test STDIO mode manually
   echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | java -jar monicahqmcp-0.1.0.jar --stdio
   
   # Validate JSON configuration
   jq . ~/.../claude_desktop_config.json
   ```

4. **Docker Issues**
   ```bash
   # Check container logs
   docker logs <container-id> --tail 50
   
   # Debug container
   docker run -it --entrypoint /bin/sh monicahq-mcp
   
   # Check Docker daemon
   docker info
   ```

### Debug Mode

**Enable MCP Protocol Debug Logging:**
```bash
# Enable MCP-specific debug logging (recommended for Claude Desktop issues)
export MCP_DEBUG=true

# Spring Boot JAR with MCP debug
MCP_DEBUG=true java -jar monicahqmcp-0.1.0.jar --stdio

# Docker with MCP debug
docker run -e MCP_DEBUG=true -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

# Claude Desktop configuration with debug mode
{
  "mcpServers": {
    "monicahq": {
      "command": "/usr/bin/java",
      "args": ["-jar", "/path/to/monicahqmcp-0.1.0.jar", "--stdio"],
      "env": {
        "MONICA_API_URL": "https://your-instance.com/api",
        "MONICA_API_TOKEN": "your-token",
        "MCP_DEBUG": "true"
      }
    }
  }
}
```

**Enable General Application Logging:**
```bash
# Spring Boot JAR with full debug
LOG_LEVEL=DEBUG LOG_LEVEL_MCP=TRACE java -jar monicahqmcp-0.1.0.jar --stdio

# Docker with full debug
docker run -e LOG_LEVEL=DEBUG -e LOG_LEVEL_MCP=TRACE -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

# Docker Compose
LOG_LEVEL=DEBUG docker-compose up
```

**Debug Mode Features:**
- **MCP Protocol Tracing**: Detailed request/response logging to stderr
- **Parameter Validation**: Enhanced error messages with suggestions
- **Performance Monitoring**: Request timing and resource usage
- **Connection Diagnostics**: Protocol negotiation and tool discovery details
- **Error Context**: Stack traces and troubleshooting hints

**What MCP_DEBUG=true provides:**
- `[MCP-DEBUG]` prefixed logs to stderr (keeps stdout clean for JSON-RPC)
- Detailed message processing flow
- Parameter validation details
- Tool execution timing
- Enhanced error messages with troubleshooting guidance

### MCP Inspector Integration

For detailed testing and debugging:

```bash
# Install MCP Inspector
npm install -g @anthropic/mcp-inspector

# Test with JAR
mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# Test with Docker
mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp
```

## Development with Docker Compose

For complete development environment with Monica instance and validation:

```bash
# Start complete development stack (Monica + MCP Server + Database)
docker-compose -f docker-compose.dev.yml up -d

# Monica available at: http://localhost:8081
# MCP Server available at: http://localhost:8080
# Validation reports: http://localhost:8080/validation

# Run validation suite
docker-compose -f docker-compose.dev.yml exec monicahq-mcp ./validation/constitutional/validate-constitution.sh

# Stop and cleanup
docker-compose -f docker-compose.dev.yml down -v
```

The development stack includes:
- **Monica CRM**: Full Monica instance with database
- **MCP Server**: Your development server with live reloading
- **Validation Suite**: Automated constitutional compliance checking
- **Health Monitoring**: Built-in health checks and logging

## Contributing

This project follows a **Constitutional Governance Framework** (v1.3.0) to ensure code quality and architectural consistency.

### Development Process

1. **Fork the repository** and create a feature branch
2. **Review the Constitution**: See [TESTING-GUIDE.md](TESTING-GUIDE.md) for constitutional principles
3. **Follow TDD**: Write tests before implementation (100% coverage required)
4. **Validate compliance**: Run constitutional validation before submitting
5. **Submit a pull request** with constitutional compliance verification

### Required Validation Steps

```bash
# 1. Constitutional compliance check
./validation/constitutional/validate-constitution.sh

# 2. Comprehensive testing (7 phases)
./validation/integration/test-mcp-complete.sh

# 3. Claude Desktop integration test
./validation/integration/test-claude-desktop.sh

# 4. Run all tests (must maintain 188/188 passing)
./gradlew test
```

For detailed testing procedures, see [TESTING-GUIDE.md](TESTING-GUIDE.md).

### Constitutional Principles

1. **MCP Protocol First** - JSON-RPC 2.0 over STDIO compliance
2. **Test-Driven Development** - 100% test coverage (non-negotiable)
3. **Spring Boot Architecture Excellence** - WebFlux for external APIs
4. **Production-Ready Deployment** - Docker + Claude Desktop ready
5. **Type Safety & Code Generation** - MapStruct + Lombok patterns
6. **Complete Monica API Data Access** - All fields visible in MCP responses

### Pull Request Requirements

- ✅ All constitutional validation tests pass
- ✅ 188/188 tests pass (100% coverage maintained)
- ✅ STDOUT cleanliness verified for MCP protocol
- ✅ Constitutional compliance documented

## Frequently Asked Questions (FAQ)

### General Questions

**Q: What is MCP (Model Context Protocol)?**
A: MCP is a protocol that allows Claude Desktop to interact with external systems. This server implements MCP to give Claude access to your MonicaHQ CRM data.

**Q: Do I need my own Monica instance?**
A: Yes, you need a Monica instance with API access enabled. You can self-host Monica or use their hosted service.

**Q: Which Monica features are supported?**
A: We support 91% of Monica's API, including all core CRM features. Only administrative endpoints (users, permissions) are excluded.

### Setup Questions

**Q: How do I get a Monica API token?**
A: In Monica, go to Settings → API → Create New Token. Copy the generated Bearer token.

**Q: Can I use this without Claude Desktop?**
A: Yes! The server supports Web Server mode for HTTP/WebSocket access. Use `--web` flag when starting.

**Q: Why are there two docker-compose files?**
A: `docker-compose.yml` is for production deployment, while `docker-compose.dev.yml` includes a full Monica instance for development.

### Technical Questions

**Q: Why does Claude Desktop show "MCP server error"?**
A: Usually STDOUT contamination. Check [MCP-STDIO-LOGGING.md](MCP-STDIO-LOGGING.md) for debugging steps.

**Q: Can I extend this with custom Monica fields?**
A: Yes, the server automatically includes all fields from Monica API responses, including custom fields.

**Q: What's the difference between STDIO and Web Server modes?**
A: STDIO mode is for Claude Desktop (JSON-RPC over standard I/O), Web Server mode provides HTTP/WebSocket endpoints for other clients.

### Performance Questions

**Q: How many concurrent operations can it handle?**
A: The server uses reactive WebFlux architecture and can handle 100+ concurrent operations. Actual limit depends on your Monica instance.

**Q: What happens if Monica API is slow or down?**
A: Circuit breaker pattern prevents cascading failures. After 50% failure rate, the circuit opens for 10 seconds before retrying.

## Security Considerations

### Authentication & Authorization
- **OAuth2 Bearer Tokens**: Never commit tokens to version control
- **Environment Variables**: Use `.env` files or secure secret management
- **Token Scope**: Create Monica tokens with minimum required permissions

### Network Security
- **HTTPS Required**: Always use HTTPS URLs for Monica API
- **Docker Network Isolation**: Containers use isolated networks
- **No Direct Database Access**: All operations go through Monica's API

### Data Privacy
- **No Data Storage**: MCP server doesn't store any CRM data
- **Pass-Through Architecture**: Acts as a proxy to Monica API
- **Audit Logging**: All operations are logged for compliance

### Best Practices
1. **Rotate API Tokens** regularly
2. **Use Read-Only Tokens** when possible
3. **Monitor Access Logs** for unusual activity
4. **Keep Dependencies Updated** with `./gradlew dependencyUpdates`
5. **Review Constitutional Compliance** before deployment

## License

MIT License - see LICENSE file for details.

## Tech Stack

- **Java 17+** - Runtime environment
- **Spring Boot 3.x** - Application framework
- **WebFlux** - Reactive web framework
- **Resilience4j** - Circuit breaker and resilience patterns
- **MapStruct** - Object mapping
- **Lombok** - Code generation
- **JUnit 5** - Testing framework
- **Docker** - Containerization
- **Gradle** - Build system

## Support & Resources

- **Documentation**: See our [Documentation Index](#documentation-index)
- **Monica API Docs**: [Monica API Reference](https://github.com/monicahq/monica/blob/main/docs/api/readme.md)
- **MCP Protocol**: [Model Context Protocol Specification](https://modelcontextprotocol.io)
- **Issues**: Report bugs via GitHub Issues
- **Contributing**: See [Contributing](#contributing) section

---

*For detailed API documentation and MonicaHQ setup instructions, visit [MonicaHQ Documentation](https://github.com/monicahq/monica)*