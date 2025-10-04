# Implementation Plan: Fix Claude Desktop MCP Integration

## Tech Stack & Libraries
- **Language**: Java 17+
- **Framework**: Spring Boot 3.x
- **Build**: Gradle
- **Testing**: JUnit 5, Mockito
- **Protocols**: MCP (Model Context Protocol) over STDIO
- **JSON Processing**: Jackson
- **Reactive**: Spring WebFlux
- **Logging**: SLF4J with Logback

## Architecture Decisions
1. **STDIO Communication**: Keep existing stdin/stdout architecture
2. **Backward Compatibility**: Maintain support for existing tests
3. **Debug Mode**: Use environment variables for activation
4. **Error Handling**: Centralized error handler for consistent messages
5. **Testing Strategy**: TDD with contract tests first

## Implementation Phases

### Phase 1: Critical Fixes (Day 1-2)
- Fix parameter validation mismatches
- Create STDIO integration tests
- Ensure all 122 operations are discoverable

### Phase 2: Debugging (Day 3-4)
- Add debug mode with stderr logging
- Create validation scripts
- Tool execution tracing

### Phase 3: Compatibility (Day 5)
- Protocol version negotiation
- Setup verification tools

### Phase 4: Polish (Day 6-7)
- Enhanced error messages
- Documentation
- Performance optimization

## Key Components to Modify
1. **ActivityService.java**: Fix attendees parameter handling
2. **McpToolRegistry.java**: Update schema definitions
3. **McpStdioServer.java**: Add debug logging
4. **McpMessageHandler.java**: Protocol negotiation

## Testing Strategy
- Contract tests for each fix
- Integration tests for STDIO flow
- Validation scripts for all 122 operations
- Manual testing checklist

## Risk Mitigation
- Feature flags for new functionality
- Comprehensive test coverage before changes
- Rollback plan documented