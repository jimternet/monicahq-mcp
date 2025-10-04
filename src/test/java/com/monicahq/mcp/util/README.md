# MCP Test Utilities

This package contains test utilities for the MonicaHQ MCP Server project.

## StdioTestHelper

A comprehensive utility class for testing MCP STDIO communication. Provides methods to:

- **STDIO Communication**: Start MCP server process and communicate via stdin/stdout
- **Message Construction**: Create valid JSON-RPC 2.0 MCP messages
- **Protocol Validation**: Validate MCP protocol compliance
- **Asynchronous Support**: Handle async messaging with CompletableFuture
- **Error Testing**: Test error scenarios and malformed requests
- **Spring Boot Integration**: Works with existing Spring Boot test framework

### Key Features

- **Synchronous & Asynchronous Messaging**: Send tool calls and handle responses
- **Complete Protocol Validation**: Validate initialize, tools/list, tools/call responses
- **Error Scenario Testing**: Test invalid tools, malformed requests, timeouts
- **STDERR Monitoring**: Monitor server logs and wait for specific patterns
- **Process Management**: Start/stop MCP server with proper cleanup

### Files

- `StdioTestHelper.java` - Main utility class with full STDIO testing capabilities
- `StdioTestHelperUnitTest.java` - Unit tests demonstrating utility method usage
- `ContentFormatterTest.java` - Existing test for content formatting utilities

### Usage

See the JavaDoc in `StdioTestHelper.java` for complete usage examples for both integration testing (with server) and unit testing (without server).