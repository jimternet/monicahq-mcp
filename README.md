# MonicaHQ MCP Server

A Spring Boot-based Model Context Protocol (MCP) server that provides Claude Desktop and other MCP clients with direct access to your MonicaHQ CRM instance. Features 50 categorized operations across 12 entity types with enhanced tool organization.

## ✅ Status: Production Ready

- **Tests**: 136/136 passing (100%)
- **Architecture**: Dual-mode Spring Boot 3.x (STDIO + Web Server)
- **Tool Organization**: 50 operations in 3 logical categories
- **Deployment**: Multiple options (Spring Boot JAR, Docker, Claude Desktop)

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

The server provides **50 categorized operations** across 12 entity types, organized for optimal discoverability:

### 📋 Contact Management (12 operations)
| Entity | Operations | Description |
|--------|------------|-------------|
| **Contact** | `[Contact]` create, get, update, delete, list | Core contact management |
| **ContactField** | `[Contact Field]` create, get, update, delete, list | Custom contact fields |
| **ContactTag** | `[Contact Tag]` add, remove | Tag-contact relationships |

### 🎯 Productivity & Organization (20 operations)
| Entity | Operations | Description |
|--------|------------|-------------|
| **Note** | `[Note]` create, get, update, delete, list | Notes and observations |
| **Task** | `[Task]` create, get, update, delete, list | Task management |
| **Reminder** | `[Reminder]` create, get, update, delete, list | Reminders and alerts |
| **Tag** | `[Tag]` create, get, update, delete, list | Organizational tags |

### 💬 Activity & Communication (18 operations)
| Entity | Operations | Description |
|--------|------------|-------------|
| **Activity** | `[Activity]` create, get, update, delete, list | Activities and interactions |
| **Call** | `[Call]` create, get, update, delete, list | Phone call records |
| **Conversation** | `[Conversation]` create, get, update, list | Conversation threads |
| **ConversationMessage** | `[Message]` create, get, update, list | Individual messages |

### Tool Organization Features
- **Category Prefixes**: All tools have clear category labels (e.g., `[Contact] Create a new contact`)
- **Logical Grouping**: Tools are ordered by frequency of use and workflow logic
- **Future-Ready**: Metadata included for client-side grouping capabilities

## Example Usage with Claude

Once configured, you can interact with your MonicaHQ instance through Claude:

```
"Create a new contact named John Doe with email john@example.com"

"Add a note to contact ID 123 saying 'Met at conference'"

"List all tasks due this week"

"Tag contact 'Alice Smith' with 'VIP Client'"
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
- **Testing**: 136 comprehensive tests (100% pass rate)
- **Tool Organization**: 50 operations in 3 logical categories

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

**Enable Detailed Logging:**
```bash
# Spring Boot JAR
LOG_LEVEL=DEBUG LOG_LEVEL_MCP=TRACE java -jar monicahqmcp-0.1.0.jar --stdio

# Docker
docker run -e LOG_LEVEL=DEBUG -e LOG_LEVEL_MCP=TRACE -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

# Docker Compose
LOG_LEVEL=DEBUG docker-compose up
```

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

## Contributing

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Ensure all tests pass: `./gradlew test`
5. Submit a pull request

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

---

*For detailed API documentation and MonicaHQ setup instructions, visit [MonicaHQ Documentation](https://github.com/monicahq/monica)*