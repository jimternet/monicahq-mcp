# Claude Desktop Setup Guide for MonicaHQ MCP Server

This guide walks you through setting up the MonicaHQ MCP Server with Claude Desktop for seamless CRM integration.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Quick Setup](#quick-setup)
- [Manual Setup](#manual-setup)
- [Configuration Options](#configuration-options)
- [Verification](#verification)
- [Usage Examples](#usage-examples)
- [Troubleshooting](#troubleshooting)
- [Advanced Configuration](#advanced-configuration)

## Prerequisites

### System Requirements

- **Operating System:** macOS 10.15+ (Claude Desktop requirement)
- **Java:** Version 17 or later
- **Claude Desktop:** Latest version from [claude.ai](https://claude.ai/download)
- **MonicaHQ Account:** Active account with API access

### Required Information

Before starting, gather:
- **MonicaHQ API URL:** Your MonicaHQ instance URL (e.g., `https://app.monicahq.com`)
- **MonicaHQ API Token:** Personal access token from your MonicaHQ settings

## Quick Setup

### Automated Setup Script

The fastest way to get started is using our automated setup script:

```bash
# Navigate to the project directory
cd /path/to/monicahq_mcp

# Run the automated setup
./validation/setup/verify-claude-desktop.sh
```

This script will:
1. ✅ Check if Claude Desktop is installed
2. ✅ Build the MCP server JAR file
3. ✅ Verify your environment variables
4. ✅ Generate Claude Desktop configuration
5. ✅ Test the MCP server connection
6. ✅ Provide next steps

### Setting Environment Variables

Before running the setup, ensure your environment variables are configured:

```bash
# Add to your ~/.zshrc or ~/.bash_profile
export MONICA_API_URL="https://your-monica-instance.com"
export MONICA_API_TOKEN="your-api-token-here"

# Reload your shell
source ~/.zshrc  # or ~/.bash_profile
```

## Manual Setup

If you prefer manual configuration or need custom settings:

### Step 1: Build the MCP Server

```bash
# Build the server
./gradlew build

# Verify the JAR file exists
ls build/libs/monica-hq-mcp-*.jar
```

### Step 2: Create Claude Desktop Configuration

Create or edit the Claude Desktop configuration file:

```bash
# Create the configuration directory
mkdir -p "$HOME/Library/Application Support/Claude"

# Edit the configuration file
nano "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
```

### Step 3: Configuration Content

Add the following configuration (replace paths and credentials):

```json
{
  "mcpServers": {
    "monicahq": {
      "command": "/usr/bin/java",
      "args": [
        "-jar",
        "/full/path/to/monicahq_mcp/build/libs/monica-hq-mcp-0.1.0.jar"
      ],
      "env": {
        "MONICA_API_URL": "https://your-monica-instance.com",
        "MONICA_API_TOKEN": "your-api-token-here",
        "MCP_DEBUG": "false"
      }
    }
  }
}
```

### Step 4: Restart Claude Desktop

1. Quit Claude Desktop completely
2. Relaunch Claude Desktop
3. Look for the 🔌 icon in the input area

## Configuration Options

### Production Configuration

For daily use:

```json
{
  "mcpServers": {
    "monicahq": {
      "command": "/usr/bin/java",
      "args": ["-jar", "/path/to/monica-hq-mcp.jar"],
      "env": {
        "MONICA_API_URL": "https://your-instance.com",
        "MONICA_API_TOKEN": "your-token",
        "MCP_DEBUG": "false"
      }
    }
  }
}
```

### Debug Configuration

For troubleshooting and development:

```json
{
  "mcpServers": {
    "monicahq-debug": {
      "command": "/usr/bin/java",
      "args": [
        "-jar",
        "/path/to/monica-hq-mcp.jar"
      ],
      "env": {
        "MONICA_API_URL": "https://your-instance.com",
        "MONICA_API_TOKEN": "your-token",
        "MCP_DEBUG": "true"
      }
    }
  }
}
```

### Memory-Optimized Configuration

For systems with limited memory:

```json
{
  "mcpServers": {
    "monicahq": {
      "command": "/usr/bin/java",
      "args": [
        "-Xmx256m",
        "-jar",
        "/path/to/monica-hq-mcp.jar"
      ],
      "env": {
        "MONICA_API_URL": "https://your-instance.com",
        "MONICA_API_TOKEN": "your-token"
      }
    }
  }
}
```

## Verification

### Check MCP Connection

After setup, verify the connection:

1. **Look for the 🔌 icon:** Should appear in Claude Desktop's input area
2. **Test basic functionality:** Ask Claude about your contacts
3. **Check server logs:** Look for connection messages

### Test Commands

Try these example commands in Claude Desktop:

```
Can you list my contacts from MonicaHQ?
```

```
Create a new activity in MonicaHQ for a meeting with John Doe
```

```
Show me my recent tasks from MonicaHQ
```

### Verification Script

Run our verification script to test all functionality:

```bash
./validation/integration/run-integration-tests.sh
```

## Usage Examples

### Contact Management

**List contacts:**
```
Show me my contacts from MonicaHQ, limit to 10
```

**Create a contact:**
```
Create a new contact in MonicaHQ:
Name: Jane Smith
Email: jane@example.com
Phone: +1-555-123-4567
```

**Search contacts:**
```
Search for contacts named "John" in MonicaHQ
```

### Activity Tracking

**Create an activity:**
```
Log an activity in MonicaHQ:
- Summary: Coffee meeting with client
- Attendees: John Doe, Jane Smith
- Date: Today at 2 PM
- Duration: 45 minutes
```

**List recent activities:**
```
What activities have been logged in MonicaHQ this week?
```

### Task Management

**Create a task:**
```
Add a task in MonicaHQ:
- Title: Follow up on proposal
- Contact: John Doe
- Description: Send the updated proposal draft
```

**List pending tasks:**
```
Show me all incomplete tasks from MonicaHQ
```

### Notes and Calls

**Add a note:**
```
Add a note to John Doe's contact in MonicaHQ:
"Prefers email communication over phone calls"
```

**Log a phone call:**
```
Record a phone call in MonicaHQ:
- Contact: Jane Smith
- Duration: 15 minutes
- Notes: Discussed project timeline
```

## Troubleshooting

### Common Issues

#### 🔌 Icon Not Appearing

**Symptoms:** No MCP connection icon in Claude Desktop

**Solutions:**
1. Check Claude Desktop configuration file syntax
2. Verify JAR file path is correct and absolute
3. Ensure Java is accessible at the specified path
4. Restart Claude Desktop completely

**Debug steps:**
```bash
# Test configuration syntax
python3 -m json.tool "$HOME/Library/Application Support/Claude/claude_desktop_config.json"

# Test MCP server directly
echo '{"jsonrpc":"2.0","id":"test","method":"initialize","params":{"protocolVersion":"2024-11-05"}}' | java -jar build/libs/monica-hq-mcp-*.jar
```

#### Connection Errors

**Symptoms:** MCP server fails to start or connect

**Solutions:**
1. Check environment variables are set correctly
2. Verify MonicaHQ API credentials
3. Test network connectivity to MonicaHQ
4. Check server logs

**Debug steps:**
```bash
# Enable debug mode
export MCP_DEBUG=true

# Test API connectivity
curl -H "Authorization: Bearer $MONICA_API_TOKEN" "$MONICA_API_URL/api/contacts?limit=1"
```

#### Tool Not Found Errors

**Symptoms:** "Tool 'monicahq:activity_create' not found"

**Solutions:**
1. Verify tool discovery is working
2. Check server initialization completed successfully
3. Test tools/list endpoint

**Debug steps:**
```bash
# Test tool discovery
echo '{"jsonrpc":"2.0","id":"tools","method":"tools/list","params":{}}' | java -jar build/libs/monica-hq-mcp-*.jar | grep -c "name"
```

#### Performance Issues

**Symptoms:** Slow responses or timeouts

**Solutions:**
1. Increase JVM memory allocation
2. Check network latency to MonicaHQ
3. Enable connection pooling
4. Monitor system resources

**Performance optimization:**
```json
{
  "command": "/usr/bin/java",
  "args": [
    "-Xmx512m",
    "-XX:+UseG1GC",
    "-jar",
    "/path/to/monica-hq-mcp.jar"
  ]
}
```

### Debug Mode

Enable detailed logging for troubleshooting:

```json
{
  "env": {
    "MCP_DEBUG": "true"
  }
}
```

Debug mode provides:
- Detailed request/response logging
- Parameter validation details
- Performance metrics
- Error context information

### Log Locations

**Claude Desktop Logs:**
- Menu: Help → Show Logs
- Location: `~/Library/Logs/Claude/`

**MCP Server Logs:**
- Debug logs go to stderr
- Visible in Claude Desktop logs when debug mode is enabled

### Getting Help

1. **Check the logs** first for error details
2. **Enable debug mode** for detailed diagnostics  
3. **Run validation scripts** to test functionality
4. **Verify configuration** syntax and paths
5. **Test API connectivity** independently

## Advanced Configuration

### Multiple MonicaHQ Instances

Configure multiple MonicaHQ accounts:

```json
{
  "mcpServers": {
    "monicahq-personal": {
      "command": "/usr/bin/java",
      "args": ["-jar", "/path/to/monica-hq-mcp.jar"],
      "env": {
        "MONICA_API_URL": "https://personal.monicahq.com",
        "MONICA_API_TOKEN": "personal-token"
      }
    },
    "monicahq-work": {
      "command": "/usr/bin/java", 
      "args": ["-jar", "/path/to/monica-hq-mcp.jar"],
      "env": {
        "MONICA_API_URL": "https://work.monicahq.com",
        "MONICA_API_TOKEN": "work-token"
      }
    }
  }
}
```

### Custom Java Configuration

Fine-tune Java performance:

```json
{
  "args": [
    "-Xms128m",
    "-Xmx512m", 
    "-XX:+UseG1GC",
    "-XX:MaxGCPauseMillis=200",
    "-jar",
    "/path/to/monica-hq-mcp.jar"
  ]
}
```

### Environment Variables

Additional configuration options:

```json
{
  "env": {
    "MONICA_API_URL": "https://instance.com",
    "MONICA_API_TOKEN": "token",
    "MCP_DEBUG": "false",
    "JAVA_OPTS": "-Xmx256m",
    "LOGGING_LEVEL": "INFO"
  }
}
```

### Automated Updates

Script to update configuration:

```bash
#!/bin/bash
# update-mcp-config.sh

CONFIG_FILE="$HOME/Library/Application Support/Claude/claude_desktop_config.json"
JAR_FILE="$(ls /path/to/monicahq_mcp/build/libs/monica-hq-mcp-*.jar | head -1)"

# Update JAR path in configuration
sed -i '' "s|/path/to/.*monica-hq-mcp.*jar|$JAR_FILE|g" "$CONFIG_FILE"

echo "Configuration updated with latest JAR: $JAR_FILE"
```

## Security Considerations

### API Token Security

- **Never commit tokens** to version control
- **Use environment variables** for credentials
- **Rotate tokens regularly** in MonicaHQ settings
- **Limit token permissions** if possible

### Network Security

- **Use HTTPS** for MonicaHQ API URLs
- **Verify SSL certificates** are valid
- **Monitor API usage** for unusual activity

### Local Security

- **Protect configuration files** with appropriate permissions
- **Keep Claude Desktop updated** to latest version
- **Monitor system logs** for security events

## Next Steps

After successful setup:

1. **Explore the tool parameters** in `docs/TOOL_PARAMETERS.md`
2. **Review usage examples** in `docs/examples/`
3. **Set up monitoring** for production use
4. **Configure automated backups** of your configuration

## Support

For additional help:
- **Documentation:** See other files in the `docs/` directory
- **Validation tools:** Run scripts in `validation/`
- **Examples:** Check `docs/examples/` for sample requests
- **Troubleshooting:** See `docs/TROUBLESHOOTING.md`

---

*This guide is maintained alongside the MonicaHQ MCP Server implementation to ensure accuracy.*