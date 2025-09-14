# MonicaHQ MCP Server - Quick Start Guide

**Version**: 0.1.0  
**Date**: 2025-09-13

## Overview
This guide provides step-by-step instructions to get the MonicaHQ MCP server running locally and perform basic operations.

## Prerequisites

1. **Java 17+** installed
   ```bash
   java -version  # Should show 17 or higher
   ```

2. **MonicaHQ Instance** accessible
   - URL: Your MonicaHQ instance URL (e.g., https://app.monicahq.com)  
   - OAuth2 Bearer Token: Generated from MonicaHQ Settings → API
   - Authentication: OAuth2 Bearer token in Authorization header

3. **MCP Client** (for testing)
   - Any MCP-compatible client or the included test client

## Installation

### Option 1: Using JAR (Recommended)
```bash
# Download the latest release
wget https://github.com/yourusername/monicahq-mcp/releases/latest/download/monicahq-mcp-server.jar

# Set environment variables
export MONICA_API_URL=https://your-monica-instance.com/api
export MONICA_API_TOKEN=your-oauth2-bearer-token-here

# Run the server
java -jar monicahq-mcp-server.jar
```

### Option 2: Building from Source
```bash
# Clone the repository
git clone https://github.com/yourusername/monicahq-mcp.git
cd monicahq-mcp

# Build with Gradle
./gradlew clean build

# Run
java -jar build/libs/monicahq-mcp-server-0.1.0.jar
```

## Configuration

Create `application.yml` in the same directory as the JAR:

```yaml
server:
  port: 8080

monica:
  api:
    url: ${MONICA_API_URL}
    token: ${MONICA_API_TOKEN}
    timeout: 30s
    max-retries: 3

mcp:
  websocket:
    path: /mcp
    max-sessions: 10

logging:
  level:
    com.noofinc.monicahqmcp: DEBUG
```

## Basic Usage

### 1. Verify Server Health
```bash
curl http://localhost:8080/actuator/health
```

Expected response:
```json
{
  "status": "UP",
  "components": {
    "monicaConnection": {
      "status": "UP",
      "details": {
        "instance": "https://your-monica-instance.com"
      }
    }
  }
}
```

### 2. Connect MCP Client
Connect your MCP client to:
```
ws://localhost:8080/mcp
```

### 3. Create Your First Contact

Send MCP request:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "contact_create",
    "arguments": {
      "firstName": "John",
      "lastName": "Doe",
      "genderId": 1,
      "isBirthdateKnown": false,
      "isDeceased": false,
      "isDeceasedDateKnown": false,
      "email": "john.doe@example.com",
      "phone": "+1-555-0123"
    }
  },
  "id": 1
}
```

Expected response:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Contact created successfully"
      }
    ],
    "data": {
      "id": 12345,
      "firstName": "John",
      "lastName": "Doe",
      "genderId": 1,
      "isBirthdateKnown": false,
      "isDeceased": false,
      "isDeceasedDateKnown": false,
      "email": "john.doe@example.com",
      "phone": "+1-555-0123",
      "createdAt": "2025-09-13T10:30:00Z"
    }
  },
  "id": 1
}
```

### 4. Add a Note to the Contact

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "note_create",
    "arguments": {
      "contactId": 12345,
      "body": "Met at the tech conference. Interested in our API integration services.",
      "isFavorite": true
    }
  },
  "id": 2
}
```

### 5. List All Contacts

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "contact_list",
    "arguments": {
      "page": 1,
      "limit": 10
    }
  },
  "id": 3
}
```

## Common Operations

### Working with Tasks
```json
// Create a task
{
  "method": "tools/call",
  "params": {
    "name": "task_create",
    "arguments": {
      "title": "Follow up with John",
      "description": "Discuss API requirements",
      "contactId": 12345,
      "dueDate": "2025-09-20"
    }
  }
}

// Mark task as complete
{
  "method": "tools/call",
  "params": {
    "name": "task_update",
    "arguments": {
      "id": 67890,
      "completed": true
    }
  }
}
```

### Managing Tags
```json
// Create a tag
{
  "method": "tools/call",
  "params": {
    "name": "tag_create",
    "arguments": {
      "name": "VIP",
      "color": "#FF0000"
    }
  }
}

// Add tag to contact
{
  "method": "tools/call",
  "params": {
    "name": "contacttag_add",
    "arguments": {
      "contactId": 12345,
      "tagId": 99
    }
  }
}
```

## Testing Script

Save as `test-mcp-server.sh`:

```bash
#!/bin/bash

# Configuration
MCP_URL="ws://localhost:8080/mcp"
CONTACT_ID=""

echo "Testing MonicaHQ MCP Server..."

# Function to send MCP request
send_request() {
    local method=$1
    local args=$2
    echo "Testing: $method"
    # Use websocat or similar tool
    echo "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"$method\",\"arguments\":$args},\"id\":1}" | \
        websocat -t -1 $MCP_URL
}

# Test 1: Create contact
echo "1. Creating contact..."
RESPONSE=$(send_request "contact_create" '{"firstName":"Test","lastName":"User","genderId":1,"isBirthdateKnown":false,"isDeceased":false,"isDeceasedDateKnown":false,"email":"test@example.com"}')
CONTACT_ID=$(echo $RESPONSE | jq -r '.result.data.id')
echo "Created contact ID: $CONTACT_ID"

# Test 2: Add note
echo "2. Adding note..."
send_request "note_create" "{\"contactId\":$CONTACT_ID,\"body\":\"Test note\"}"

# Test 3: Create task
echo "3. Creating task..."
send_request "task_create" "{\"title\":\"Test task\",\"contactId\":$CONTACT_ID}"

# Test 4: List contacts
echo "4. Listing contacts..."
send_request "contact_list" '{"limit":5}'

echo "All tests completed!"
```

## Troubleshooting

### Connection Refused
- Check server is running: `ps aux | grep java`
- Verify port is available: `lsof -i :8080`
- Check firewall settings

### Authentication Failed
- Verify OAuth2 Bearer token is correct
- Check token has necessary permissions in MonicaHQ  
- Ensure token is properly formatted: "Authorization: Bearer TOKEN"
- Ensure environment variables are set

### Rate Limiting
- Server automatically handles rate limits with exponential backoff
- Check logs for rate limit warnings
- Adjust `monica.api.max-retries` if needed

### WebSocket Connection Drops
- Check `mcp.websocket.max-sessions` configuration
- Monitor server logs for errors
- Ensure client sends periodic ping messages

## Performance Validation

Run the included performance test:
```bash
./gradlew test --tests PerformanceTest
```

Expected results:
- Create contact: < 500ms
- List contacts: < 300ms
- Concurrent operations: 10+ req/s

## Next Steps

1. **Explore All Operations**: Review the [MCP Protocol Contract](contracts/mcp-protocol.json) for all 52 available operations
2. **Set Up Monitoring**: Configure Prometheus metrics collection
3. **Customize Configuration**: Adjust settings in `application.yml`
4. **Build Automations**: Create scripts using the MCP interface
5. **Deploy Production**: Use Docker for production deployment

## Support

- Documentation: [Full Documentation](../README.md)
- Issues: [GitHub Issues](https://github.com/yourusername/monicahq-mcp/issues)
- MCP Protocol: [MCP Specification](https://modelcontextprotocol.io)
- MonicaHQ API: [Monica API Docs](https://www.monicahq.com/api)

---
*Quick Start Guide v0.1.0 - MonicaHQ MCP Server*