# MonicaHQ MCP Server - Troubleshooting Guide

This guide helps you diagnose and resolve common issues with the MonicaHQ MCP Server and Claude Desktop integration.

## Table of Contents

- [Quick Diagnostics](#quick-diagnostics)
- [Connection Issues](#connection-issues)
- [Tool Execution Problems](#tool-execution-problems)
- [Performance Issues](#performance-issues)
- [Configuration Problems](#configuration-problems)
- [API-Related Issues](#api-related-issues)
- [Debug Mode](#debug-mode)
- [Common Error Messages](#common-error-messages)
- [Validation Tools](#validation-tools)

## Quick Diagnostics

### Essential Checks

Run these commands to quickly identify common issues:

```bash
# 1. Check if Java is installed
java -version

# 2. Check if MCP server JAR exists
ls build/libs/monica-hq-mcp-*.jar

# 3. Test MCP server directly
echo '{"jsonrpc":"2.0","id":"test","method":"tools/list","params":{}}' | java -jar build/libs/monica-hq-mcp-*.jar

# 4. Check environment variables
echo "API URL: $MONICA_API_URL"
echo "API Token length: ${#MONICA_API_TOKEN}"

# 5. Validate Claude Desktop config
python3 -m json.tool "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
```

### Health Check Script

Run the comprehensive health check:

```bash
./validation/setup/verify-claude-desktop.sh
```

## Connection Issues

### Issue: 🔌 Icon Missing in Claude Desktop

**Symptom:** No MCP connection indicator in Claude Desktop

**Diagnostic Steps:**
1. Check Claude Desktop configuration file exists and is valid JSON
2. Verify JAR file path is absolute and file exists
3. Ensure Java path is correct
4. Check Claude Desktop logs

**Solutions:**

```bash
# Check configuration syntax
python3 -m json.tool "$HOME/Library/Application Support/Claude/claude_desktop_config.json"

# Test JAR file directly
java -jar /full/path/to/monica-hq-mcp.jar <<< '{"jsonrpc":"2.0","id":"test","method":"initialize","params":{"protocolVersion":"2024-11-05"}}'

# Verify Java path
which java

# Restart Claude Desktop completely
killall Claude && open /Applications/Claude.app
```

### Issue: MCP Server Fails to Start

**Symptom:** Claude Desktop shows connection errors

**Common Causes:**
- Incorrect JAR file path
- Java not found
- Invalid environment variables
- Port conflicts

**Solutions:**

```bash
# Use absolute paths in configuration
{
  "command": "/usr/bin/java",
  "args": [
    "-jar",
    "/Users/username/monicahq_mcp/build/libs/monica-hq-mcp-0.1.0.jar"
  ]
}

# Test environment variables
export MONICA_API_URL="https://your-instance.com"
export MONICA_API_TOKEN="your-token"

# Check for Java issues
java -version
java -jar build/libs/monica-hq-mcp-*.jar --help
```

### Issue: Connection Drops Frequently

**Symptom:** Intermittent connection losses

**Diagnostic:**
```bash
# Enable debug mode to see connection details
MCP_DEBUG=true java -jar build/libs/monica-hq-mcp-*.jar
```

**Solutions:**
- Check network stability
- Increase JVM memory allocation
- Monitor system resources
- Verify API token validity

## Tool Execution Problems

### Issue: "Tool not found" Errors

**Symptom:** `Tool 'monicahq:activity_create' not found`

**Diagnostic:**
```bash
# Check tool discovery
echo '{"jsonrpc":"2.0","id":"tools","method":"tools/list","params":{}}' | java -jar build/libs/monica-hq-mcp-*.jar | grep -c '"name"'

# Expected output: 122 (or more)
```

**Solutions:**
1. Verify server initialization completed
2. Check tools/list returns all expected tools
3. Restart Claude Desktop
4. Rebuild MCP server JAR

### Issue: Parameter Validation Errors

**Symptom:** `Invalid params` errors

**Enable Debug Mode:**
```json
{
  "env": {
    "MCP_DEBUG": "true"
  }
}
```

**Common Parameter Issues:**

1. **Attendees Format:**
   ```bash
   # ❌ Wrong
   "attendees": "John Doe"
   
   # ✅ Correct  
   "attendees": ["John Doe"]
   "attendees": [{"contactId": 123}]
   ```

2. **Date Format:**
   ```bash
   # ❌ Wrong
   "happenedAt": "2024-01-15"
   
   # ✅ Correct
   "happenedAt": "2024-01-15T10:30:00Z"
   ```

3. **Required Fields:**
   ```bash
   # ❌ Missing required field
   {"description": "Meeting notes"}
   
   # ✅ Include required fields
   {"summary": "Team meeting", "attendees": ["John"]}
   ```

### Issue: Empty or Unexpected Responses

**Symptom:** Tools execute but return empty data

**Diagnostic Steps:**

```bash
# Test API connectivity directly
curl -H "Authorization: Bearer $MONICA_API_TOKEN" \
     "$MONICA_API_URL/api/contacts?limit=1"

# Check API token permissions
curl -H "Authorization: Bearer $MONICA_API_TOKEN" \
     "$MONICA_API_URL/api/me"
```

**Solutions:**
- Verify API token has correct permissions
- Check MonicaHQ instance is accessible
- Ensure data exists in MonicaHQ
- Test with different parameters

## Performance Issues

### Issue: Slow Response Times

**Symptom:** Long delays for tool execution

**Diagnostic:**
```bash
# Monitor performance
time echo '{"jsonrpc":"2.0","id":"perf","method":"tools/list","params":{}}' | java -jar build/libs/monica-hq-mcp-*.jar
```

**Solutions:**

1. **Increase JVM Memory:**
   ```json
   {
     "args": [
       "-Xmx512m",
       "-jar",
       "/path/to/monica-hq-mcp.jar"
     ]
   }
   ```

2. **Optimize Garbage Collection:**
   ```json
   {
     "args": [
       "-XX:+UseG1GC",
       "-XX:MaxGCPauseMillis=200",
       "-jar", 
       "/path/to/monica-hq-mcp.jar"
     ]
   }
   ```

3. **Check Network Latency:**
   ```bash
   curl -w "@curl-format.txt" -o /dev/null -s "$MONICA_API_URL/api/me"
   ```

### Issue: High Memory Usage

**Symptom:** System becomes slow, memory warnings

**Diagnostic:**
```bash
# Monitor Java memory usage
jps -l | grep monica-hq-mcp
jstat -gc <pid>
```

**Solutions:**
- Set maximum heap size: `-Xmx256m`
- Use efficient garbage collector: `-XX:+UseG1GC`
- Monitor for memory leaks in debug mode

## Configuration Problems

### Issue: Invalid JSON Configuration

**Symptom:** Claude Desktop fails to start MCP servers

**Diagnostic:**
```bash
python3 -m json.tool "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
```

**Common JSON Errors:**
```bash
# ❌ Trailing comma
{
  "mcpServers": {
    "monicahq": {...},  # <- Remove this comma
  }
}

# ❌ Missing quotes
{
  command: "/usr/bin/java"  # <- Should be "command"
}

# ❌ Wrong path separators (Windows paths in macOS)
"command": "C:\\Program Files\\Java\\bin\\java"
```

### Issue: Environment Variables Not Working

**Symptom:** API calls fail with authentication errors

**Solutions:**

1. **Check Variable Export:**
   ```bash
   echo $MONICA_API_URL
   echo $MONICA_API_TOKEN
   ```

2. **Add to Shell Profile:**
   ```bash
   # Add to ~/.zshrc or ~/.bash_profile
   export MONICA_API_URL="https://your-instance.com"
   export MONICA_API_TOKEN="your-token"
   
   # Reload
   source ~/.zshrc
   ```

3. **Set in Configuration:**
   ```json
   {
     "env": {
       "MONICA_API_URL": "https://your-instance.com",
       "MONICA_API_TOKEN": "your-token"
     }
   }
   ```

## API-Related Issues

### Issue: Authentication Failures

**Symptom:** `Authentication failed` errors

**Diagnostic:**
```bash
# Test API token directly
curl -H "Authorization: Bearer $MONICA_API_TOKEN" \
     "$MONICA_API_URL/api/me"
```

**Solutions:**
1. Generate new API token in MonicaHQ settings
2. Verify token has correct permissions
3. Check token hasn't expired
4. Ensure URL includes https://

### Issue: API Rate Limiting

**Symptom:** `Rate limit exceeded` errors

**Solutions:**
- Reduce request frequency
- Implement request caching
- Contact MonicaHQ support for higher limits

### Issue: Network Connectivity

**Symptom:** Connection timeouts or DNS errors

**Diagnostic:**
```bash
# Test basic connectivity
ping your-monica-instance.com

# Test HTTPS connectivity  
curl -I https://your-monica-instance.com

# Check DNS resolution
nslookup your-monica-instance.com
```

## Debug Mode

### Enabling Debug Mode

Add debug configuration:
```json
{
  "env": {
    "MCP_DEBUG": "true"
  }
}
```

### Debug Output

Debug mode provides:
- Detailed request/response logging
- Parameter validation details
- Performance timing information
- Error stack traces
- Connection status updates

### Debug Log Analysis

Look for these patterns in debug logs:

```bash
# Successful initialization
[MCP-DEBUG] MCP Server started in DEBUG mode
[MCP-DEBUG] Initialize response: protocol=2024-11-05

# Tool discovery
[MCP-DEBUG] Returning 122 tools in list

# Parameter issues
[MCP-DEBUG] Parameter validation failed: attendees must be an array

# API connectivity
[MCP-DEBUG] API request successful: GET /api/contacts
```

## Common Error Messages

### `Parse error`

**Cause:** Invalid JSON in request
**Solution:** Check JSON syntax, escape special characters

### `Method not found`

**Cause:** Unknown MCP method
**Solution:** Use supported methods: `initialize`, `tools/list`, `tools/call`

### `Invalid params`

**Cause:** Missing or invalid parameters
**Solution:** Check parameter documentation, enable debug mode

### `Tool execution error`

**Cause:** API call failed or parameter validation failed
**Solution:** Check API connectivity, verify parameters

### `Authentication failed`

**Cause:** Invalid or missing API token
**Solution:** Generate new token, check environment variables

### `Resource not found`

**Cause:** Requested resource doesn't exist
**Solution:** Verify resource ID, check permissions

## Validation Tools

### Comprehensive Testing

```bash
# Run all integration tests
./validation/integration/run-integration-tests.sh

# Validate all tools
./validation/integration/validate-all-122-tools.sh

# Test Claude Desktop setup
./validation/setup/verify-claude-desktop.sh
```

### Individual Component Testing

```bash
# Test MCP protocol
echo '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2024-11-05"}}' | java -jar build/libs/monica-hq-mcp-*.jar

# Test tool discovery
echo '{"jsonrpc":"2.0","id":"2","method":"tools/list","params":{}}' | java -jar build/libs/monica-hq-mcp-*.jar

# Test tool execution
echo '{"jsonrpc":"2.0","id":"3","method":"tools/call","params":{"name":"monicahq:contact_list","arguments":{"limit":1}}}' | java -jar build/libs/monica-hq-mcp-*.jar
```

### Environment Validation

```bash
# Check all prerequisites
./validation/setup/verify-claude-desktop.sh --help

# Validate API connectivity
curl -f -H "Authorization: Bearer $MONICA_API_TOKEN" "$MONICA_API_URL/api/me"

# Test Java configuration  
java -version && java -jar build/libs/monica-hq-mcp-*.jar --help
```

## Getting Additional Help

### Log Collection

When reporting issues, collect these logs:

```bash
# System information
uname -a
java -version
sw_vers

# Configuration
cat "$HOME/Library/Application Support/Claude/claude_desktop_config.json"

# Environment
env | grep MONICA

# Test output
./validation/integration/run-integration-tests.sh > test-output.log 2>&1
```

### Best Practices for Troubleshooting

1. **Start with quick diagnostics** - Run the basic checks first
2. **Enable debug mode** - Get detailed information about what's happening
3. **Test components individually** - Isolate the problem area
4. **Check logs systematically** - Look for patterns in error messages
5. **Verify configuration step by step** - Ensure each part is correct
6. **Use validation tools** - Leverage built-in testing capabilities

### Emergency Recovery

If Claude Desktop becomes unresponsive:

```bash
# Kill all Claude processes
killall Claude

# Backup current configuration
cp "$HOME/Library/Application Support/Claude/claude_desktop_config.json" ~/claude_config_backup.json

# Remove MCP configuration temporarily
echo '{}' > "$HOME/Library/Application Support/Claude/claude_desktop_config.json"

# Restart Claude Desktop
open /Applications/Claude.app

# Restore configuration after testing
cp ~/claude_config_backup.json "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
```

---

*This troubleshooting guide is regularly updated based on user feedback and common issues encountered in production deployments.*