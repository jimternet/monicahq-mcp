# MonicaHQ MCP Server - Claude Desktop Integration

This guide shows you how to connect the MonicaHQ MCP server to Claude Desktop to access your MonicaHQ CRM data directly from Claude.

## Prerequisites

1. **Claude Desktop** installed on your machine
2. **Java 17+** installed
3. **MonicaHQ instance** with API access
4. **OAuth2 Bearer Token** from your MonicaHQ instance

## Step 1: Get Your MonicaHQ API Token

1. Log into your MonicaHQ instance
2. Go to **Settings** → **API**
3. Generate a new **Personal Access Token**
4. Copy the token - you'll need it for configuration

## Step 2: Set Up Environment Variables

Create a `.env` file in the project root:

```bash
# MonicaHQ Configuration
MONICA_API_URL=https://your-monica-instance.com/api
MONICA_API_TOKEN=your-oauth2-bearer-token-here

# MCP Server Configuration  
MCP_PORT=8080
MCP_HOST=localhost
```

Or set them directly in your shell:

```bash
export MONICA_API_URL=https://your-monica-instance.com/api
export MONICA_API_TOKEN=your-oauth2-bearer-token-here
```

## Step 3: Build and Start the MCP Server

```bash
# Clone and build the project
git clone <your-repo-url>
cd monicahq_mcp

# Build the server
./gradlew clean build

# Start the server
./gradlew bootRun
```

The server will start on `http://localhost:8080` with WebSocket endpoint at `ws://localhost:8080/mcp`.

## Step 4: Configure Claude Desktop

### Method 1: Using claude_desktop_config.json

1. **Find your Claude Desktop config directory:**
   - **macOS**: `~/Library/Application Support/Claude/`
   - **Windows**: `%APPDATA%\Claude\`
   - **Linux**: `~/.config/Claude/`

2. **Create or edit `claude_desktop_config.json`:**

```json
{
  "mcpServers": {
    "monicahq": {
      "command": "java",
      "args": [
        "-jar",
        "/path/to/your/build/libs/monicahq-mcp-server-0.1.0.jar"
      ],
      "env": {
        "MONICA_API_URL": "https://your-monica-instance.com/api",
        "MONICA_API_TOKEN": "your-oauth2-bearer-token-here"
      }
    }
  }
}
```

### Method 2: Using WebSocket Connection (Alternative)

If you prefer to run the server separately, configure Claude Desktop to connect via WebSocket:

```json
{
  "mcpServers": {
    "monicahq": {
      "command": "npx",
      "args": ["@anthropic-ai/mcp-client", "ws://localhost:8080/mcp"],
      "env": {}
    }
  }
}
```

## Step 5: Restart Claude Desktop

1. **Quit Claude Desktop completely**
2. **Restart Claude Desktop**
3. **Look for the MonicaHQ tools** in the Claude interface

## Step 6: Verify Connection

Once Claude Desktop is running, you should see MonicaHQ tools available. Try asking Claude:

> "What contacts do I have in MonicaHQ?"

or 

> "Create a new contact named John Smith in MonicaHQ"

## Available Operations

The MCP server provides 52+ operations across 12 entity types:

### **Contact Operations**
- Create, read, update, delete contacts
- List contacts with pagination and search
- Add custom fields to contacts

### **Activity Operations**
- Track activities with contacts
- Record meetings, calls, and interactions
- Associate activities with multiple attendees

### **Communication Operations**
- Log phone calls with details
- Track conversation history
- Add conversation messages

### **Task Management**
- Create and manage tasks
- Set due dates and priorities
- Mark tasks as complete

### **Notes & Journaling**
- Add personal notes to contacts
- Create journal entries
- Favorite important notes

### **Tags & Organization**
- Create and manage tags
- Associate tags with contacts
- Filter contacts by tags

### **Reminders**
- Set up contact reminders
- Configure recurring reminders
- Track reminder history

## Example Usage in Claude Desktop

Once configured, you can ask Claude things like:

**Creating Contacts:**
> "Add a new contact: Sarah Johnson, email sarah@example.com, phone +1-555-123-4567"

**Managing Tasks:**
> "Create a task to follow up with John about the project proposal, due next Friday"

**Adding Notes:**
> "Add a note to contact ID 123: 'Discussed partnership opportunities during coffee meeting'"

**Searching:**
> "Show me all contacts tagged as 'VIP'"

**Organizing:**
> "List all my upcoming reminders for this week"

## Troubleshooting

### Connection Issues

1. **Server not starting:**
   ```bash
   # Check Java version
   java -version
   
   # Check if port 8080 is available
   lsof -i :8080
   
   # Check server logs
   tail -f logs/application.log
   ```

2. **Claude Desktop not seeing tools:**
   - Verify `claude_desktop_config.json` is in the correct location
   - Check JSON syntax is valid
   - Restart Claude Desktop completely
   - Check Claude Desktop logs for errors

3. **API Authentication errors:**
   - Verify your MonicaHQ token is valid
   - Check token permissions in MonicaHQ settings
   - Ensure `MONICA_API_URL` is correct (should end with `/api`)

### Health Check

You can verify the server is working by visiting:
- **Health Check**: `http://localhost:8080/health`
- **Detailed Health**: `http://localhost:8080/health/detailed`

Example healthy response:
```json
{
  "status": "UP",
  "components": {
    "monicaHQ": "UP",
    "webSocket": "UP", 
    "toolRegistry": "UP"
  }
}
```

## Advanced Configuration

### Custom Port

If port 8080 is in use, change it in `application.yml`:

```yaml
server:
  port: 9090  # Use different port

mcp:
  websocket:
    path: /mcp
```

Then update your `.env`:
```bash
MCP_PORT=9090
```

### Production Deployment

For production use, consider:

1. **Run as a service:**
   ```bash
   # Create systemd service file
   sudo nano /etc/systemd/system/monicahq-mcp.service
   ```

2. **Use environment-specific configs:**
   - `application-dev.yml`
   - `application-prod.yml`

3. **Enable SSL/TLS** for WebSocket connections

### Multiple MonicaHQ Instances

You can configure multiple instances:

```json
{
  "mcpServers": {
    "monicahq-personal": {
      "command": "java",
      "args": ["-jar", "/path/to/server.jar"],
      "env": {
        "MONICA_API_URL": "https://personal.monicahq.com/api",
        "MONICA_API_TOKEN": "personal-token"
      }
    },
    "monicahq-work": {
      "command": "java", 
      "args": ["-jar", "/path/to/server.jar", "--server.port=8081"],
      "env": {
        "MONICA_API_URL": "https://work.monicahq.com/api",
        "MONICA_API_TOKEN": "work-token"
      }
    }
  }
}
```

## Support & Resources

- **Server Logs**: Check `logs/application.log` for debugging
- **Claude Desktop Logs**: Check Claude Desktop console for MCP connection issues  
- **MonicaHQ API Docs**: https://www.monicahq.com/api
- **MCP Protocol**: https://modelcontextprotocol.io

---

🎉 **You're all set!** Claude Desktop can now interact with your MonicaHQ CRM data through the MCP server.