# MCP STDIO Mode - Critical Logging Requirements

## The Problem: STDOUT Contamination Breaks MCP Protocol

The MCP (Model Context Protocol) uses **JSON-RPC 2.0 over STDIO**, which means:
- **STDIN**: Receives JSON-RPC requests from Claude Desktop
- **STDOUT**: MUST contain ONLY valid JSON-RPC responses  
- **STDERR**: Available for logging, debugging, errors

**Critical Issue**: Any non-JSON output to STDOUT will break the MCP protocol and cause Claude Desktop integration to fail.

## Constitutional Principle Violated

This directly impacts **Principle I: MCP Protocol First** from Constitution v1.1.0:
> "Every operation MUST follow JSON-RPC 2.0 over STDIO specification... Protocol compliance is non-negotiable for Claude Desktop integration."

## Common STDOUT Contamination Sources

### 1. Spring Boot Banner
```
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
```
**Solution**: `spring.main.banner-mode=off`

### 2. Spring Boot Startup Logs
```
INFO  - Starting MonicaHqMcpApplication
INFO  - Started MonicaHqMcpApplication in 2.3 seconds
```
**Solution**: Redirect ALL logs to STDERR

### 3. Application Debug/Info Logs
```
DEBUG - Processing contact creation request
INFO  - MonicaHQ API call successful
```
**Solution**: Use dedicated STDIO logging configuration

### 4. Framework Logs (WebFlux, Netty, etc.)
```
DEBUG - Netty connection established
INFO  - WebFlux handler registered
```
**Solution**: Disable or redirect framework logging

## Our Implementation Solution

### 1. Dedicated STDIO Server (`McpStdioServer.java`)
```java
// Use STDIO-specific logging configuration
System.setProperty("logging.config", "classpath:logback-stdio.xml");

// Disable Spring Boot banner and web server
System.setProperty("spring.main.banner-mode", "off");
System.setProperty("spring.main.web-application-type", "none");

// Minimal logging levels
System.setProperty("logging.level.root", "WARN");
System.setProperty("logging.level.com.monicahq.mcp", "ERROR");
```

### 2. STDIO-Specific Logback Configuration (`logback-stdio.xml`)
```xml
<!-- ALL output goes to STDERR to keep STDOUT clean -->
<appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
    <target>System.err</target>
</appender>

<!-- Only errors and above -->
<root level="WARN">
    <appender-ref ref="STDERR"/>
</root>

<!-- Disable noisy frameworks -->
<logger name="org.apache" level="OFF"/>
<logger name="io.netty" level="OFF"/>
<logger name="reactor" level="OFF"/>
```

### 3. Clean JSON-RPC Response Handling
```java
// Send response - ONLY JSON to stdout
if (response != null) {
    String responseJson = objectMapper.writeValueAsString(response);
    writer.println(responseJson);  // Clean JSON only
    writer.flush();
}
```

## Testing STDOUT Cleanliness

### Manual Test
```bash
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  java -jar build/libs/monicahqmcp-0.1.0.jar --stdio | \
  jq .  # Should parse cleanly - no extra output
```

### Automated Validation
Our `./test-mcp-complete.sh` includes STDOUT contamination checks:
```bash
test_mcp_stdio_basic() {
    local response=$(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
        timeout 10s java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>/dev/null | \
        head -1)
    
    # Must be valid JSON-RPC
    echo "$response" | jq -e '.jsonrpc == "2.0" and .id == 1 and has("result")' >/dev/null 2>&1
}
```

## Architecture Patterns

### ✅ Correct: Dual-Mode Architecture
```
MonicaHqMcpApplication (main)
├── --stdio flag → McpStdioServer (clean STDOUT)
└── default → Web Server Mode (normal logging)
```

### ❌ Incorrect: Single Mode with Conditional Logging
```
Single application with if/else logging  
→ Risk of STDOUT contamination
→ Hard to test logging separation
```

## Development Best Practices

### 1. Never Log to STDOUT in STDIO Mode
```java
// ❌ WRONG - breaks MCP protocol
System.out.println("Debug info");

// ✅ CORRECT - use stderr or logger
System.err.println("Debug info");
log.debug("Debug info");  // Goes to STDERR via logback-stdio.xml
```

### 2. Test Both Modes Separately
```bash
# Test STDIO mode (clean STDOUT)
./test-mcp-complete.sh

# Test Web Server mode (normal logging)
java -jar build/libs/monicahqmcp-0.1.0.jar --web
curl http://localhost:8080/actuator/health
```

### 3. Use Constitutional Validation
```bash
./validate-constitution.sh
# Checks MCP Protocol compliance including STDOUT cleanliness
```

## Debugging STDIO Issues

### 1. Check for STDOUT Contamination
```bash
# Should output ONLY JSON
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# Redirect stderr to see what's being logged there
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  java -jar build/libs/monicahqmcp-0.1.0.jar --stdio 2>debug.log
```

### 2. Common Fixes for STDOUT Contamination

**Spring Boot Issues:**
```java
// Add to McpStdioServer
System.setProperty("spring.main.banner-mode", "off");
System.setProperty("logging.level.root", "OFF");
```

**Application Code Issues:**
```java
// Replace System.out with System.err or logger
System.err.println("Debug: " + message);
log.debug("Debug: {}", message);  // Uses STDERR appender
```

**Framework Noise:**
```xml
<!-- In logback-stdio.xml -->
<logger name="problematic.framework" level="OFF"/>
```

## Claude Desktop Integration Impact

### What Happens When STDOUT is Contaminated:
1. Claude Desktop receives malformed response
2. JSON parsing fails  
3. MCP connection drops
4. Tools become unavailable
5. User sees "MCP server error"

### Expected Clean STDOUT:
```json
{"jsonrpc":"2.0","result":{"tools":[{"name":"contact_list","description":"..."}]},"id":1}
```

### STDOUT Contamination Example:
```
INFO  - Spring Boot started
{"jsonrpc":"2.0","result":{"tools":[...]},"id":1}
DEBUG - Processing complete
```
↑ This breaks Claude Desktop parsing

## Constitutional Compliance Checklist

- [ ] STDOUT contains ONLY JSON-RPC responses in STDIO mode
- [ ] All logging redirected to STDERR via `logback-stdio.xml`
- [ ] Spring Boot banner disabled (`spring.main.banner-mode=off`)
- [ ] Framework logging minimized or disabled
- [ ] Dual-mode architecture maintains clean separation
- [ ] Testing validates STDOUT cleanliness
- [ ] No `System.out.println()` calls in STDIO code paths

## Validation Commands

```bash
# Constitutional compliance (includes STDOUT checks)
./validate-constitution.sh

# Comprehensive MCP testing
./test-mcp-complete.sh

# Claude Desktop integration testing
./test-claude-desktop.sh

# Manual STDOUT validation
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  ./run-mcp-server.sh | jq .
```

---

**Remember**: In MCP STDIO mode, STDOUT is sacred. Any contamination breaks the protocol and violates our constitutional principle of "MCP Protocol First".