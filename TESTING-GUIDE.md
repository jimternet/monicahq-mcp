# MonicaHQ MCP Server Testing Guide

**Constitutional Governance Framework v1.1.0**

This guide provides comprehensive testing instructions for the MonicaHQ MCP Server, following our constitutional principles and quality assurance standards.

## Quick Reference

```bash
# Constitutional compliance validation
./validate-constitution.sh

# Complete 7-phase testing suite
./test-mcp-complete.sh

# Claude Desktop integration testing
./test-claude-desktop.sh

# Individual principle testing
./tests/constitutional/test_mcp_protocol_first.sh
./tests/constitutional/test_tdd_compliance.sh
./tests/constitutional/test_spring_architecture.sh
./tests/constitutional/test_production_ready.sh
./tests/constitutional/test_type_safety.sh

# STDOUT contamination detection
./tests/integration/test_stdout_cleanliness.sh
```

## Testing Framework Overview

### Constitutional Governance Testing

Our testing framework validates adherence to **5 Core Constitutional Principles**:

1. **MCP Protocol First** - JSON-RPC 2.0 over STDIO compliance
2. **Test-Driven Development** - 100% test coverage (136/136 tests)
3. **Spring Boot Architecture Excellence** - WebFlux for external APIs
4. **Production-Ready Deployment** - Docker + Claude Desktop ready
5. **Type Safety & Code Generation** - MapStruct + Lombok patterns

### 7-Phase Testing Methodology

The comprehensive testing suite follows a systematic 7-phase approach:

1. **Constitutional Compliance** - Validate all 5 core principles
2. **Unit and Integration Tests** - Run full test suite (136 tests)
3. **MCP Protocol Validation** - JSON-RPC 2.0 over STDIO testing
4. **Architecture Validation** - Spring Boot and reactive patterns
5. **Security and Quality** - Type safety, OAuth2, no hardcoded secrets
6. **Deployment Readiness** - Docker, JAR execution, health checks
7. **Tool Accessibility** - Helper scripts, documentation, Claude Desktop config

## Detailed Testing Instructions

### 1. Constitutional Compliance Validation

The primary governance validation ensures adherence to all constitutional principles:

```bash
./validate-constitution.sh
```

**Expected Output:**
```
🏛️  MonicaHQ MCP Server Constitutional Compliance Validator
Constitution Version: 1.1.0
========================================================

Principle I: MCP Protocol First
===============================
✅ PASS: JSON-RPC 2.0 protocol implementation found
✅ PASS: Tool categorization implemented
✅ PASS: STDIO mode for Claude Desktop implemented
✅ PASS: STDIO logging properly configured (STDERR only)
✅ PASS: No STDOUT contamination detected

[... additional principles ...]

🎉 FULLY COMPLIANT - No violations or warnings
```

### 2. Individual Constitutional Principle Testing

Test each principle independently for detailed validation:

#### MCP Protocol First Principle
```bash
./tests/constitutional/test_mcp_protocol_first.sh
```
Validates:
- JSON-RPC 2.0 specification compliance
- MCP tool categorization and discoverability
- STDOUT cleanliness for Claude Desktop integration
- Protocol error handling
- Compliance validation scripts

#### Test-Driven Development Principle
```bash
./tests/constitutional/test_tdd_compliance.sh
```
Validates:
- 100% test coverage requirement (136+ tests)
- RED-GREEN-Refactor cycle evidence
- Test hierarchy (Contract→Integration→E2E→Unit)
- Quality gate enforcement
- JUnit test structure

#### Spring Boot Architecture Excellence
```bash
./tests/constitutional/test_spring_architecture.sh
```
Validates:
- Spring Boot 3.x version compliance
- WebFlux reactive patterns for I/O operations
- Dual-mode architecture (STDIO + Web Server)
- Dependency injection and component scanning
- Circuit breaker patterns with Resilience4j
- Proper component structure

#### Production-Ready Deployment
```bash
./tests/constitutional/test_production_ready.sh
```
Validates:
- Docker containerization support
- Environment variable configuration
- Health checks and monitoring
- Production logging configuration
- Claude Desktop integration readiness
- Security best practices

#### Type Safety and Code Generation
```bash
./tests/constitutional/test_type_safety.sh
```
Validates:
- MapStruct type-safe mapping implementation
- Lombok code generation
- DTO structure and type safety
- Strong typing in API interactions
- Bean validation annotations
- Generic type usage (no raw types)

### 3. STDOUT Contamination Detection

Critical for MCP protocol compliance:

```bash
./tests/integration/test_stdout_cleanliness.sh
```

This test validates:
- STDIO logging configuration prevents STDOUT contamination
- McpStdioServer STDOUT cleanliness
- Spring Boot banner suppression
- JSON-RPC response format validation
- Error response cleanliness

**Why This Matters:**
STDOUT contamination breaks Claude Desktop integration. Only JSON-RPC responses should appear on STDOUT in STDIO mode.

### 4. Complete Integration Testing

Run the full 7-phase testing suite:

```bash
./test-mcp-complete.sh
```

**Prerequisites:**
```bash
# Set environment variables for complete testing
export MONICA_API_URL=https://your-monica-instance.com/api
export MONICA_API_TOKEN=your-oauth2-bearer-token
```

**Expected Output:**
```
🧪 MonicaHQ MCP Server - Comprehensive Testing Suite
=====================================================

Phase 1: Constitutional Compliance
==================================
✅ PASSED: Constitutional compliance validation

Phase 2: Unit and Integration Tests
===================================
✅ PASSED: Gradle test suite (136 tests)
✅ PASSED: Test count validation (≥50 test files)

[... 7 phases ...]

🎉 ALL TESTS PASSED (XX/XX)
The MonicaHQ MCP Server is ready for production deployment
```

### 5. Claude Desktop Integration Testing

Validate Claude Desktop compatibility:

```bash
./test-claude-desktop.sh
```

This comprehensive test validates:
- Constitutional compliance
- Configuration file validation
- MCP server connectivity
- Claude Desktop compatibility
- Real-world integration simulation

**Cross-Platform Support:**
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Linux**: `~/.config/claude-desktop/claude_desktop_config.json`

## Test Failure Troubleshooting

### Constitutional Violations

If constitutional validation fails:

1. **Review specific violation messages**
2. **Address architectural issues** (e.g., missing WebFlux, hardcoded secrets)
3. **Update code following constitutional principles**
4. **Re-run validation**: `./validate-constitution.sh`

### Test Coverage Issues

If test coverage drops below 100%:

1. **Identify missing test coverage**: `./gradlew test jacocoTestReport`
2. **Write tests following TDD methodology** (RED-GREEN-Refactor)
3. **Ensure tests are properly categorized** (contract/integration/unit)
4. **Verify test count**: Should be 136+ tests

### STDOUT Contamination

If STDOUT contamination is detected:

1. **Check logging configuration**: `src/main/resources/logback-stdio.xml`
2. **Review McpStdioServer.java** for System.out usage
3. **Ensure Spring Boot banner is disabled**
4. **Verify all output goes to STDERR** except JSON-RPC responses

### Claude Desktop Integration Issues

If Claude Desktop integration fails:

1. **Validate configuration file** with `jq . claude_desktop_config.json`
2. **Check JAR file path** is absolute and exists
3. **Verify environment variables** are properly set
4. **Test STDIO mode manually**: `java -jar build/libs/monicahqmcp-0.1.0.jar --stdio`
5. **Restart Claude Desktop** after configuration changes

## Continuous Integration

### Pre-commit Validation

Before committing code:

```bash
# Quick validation (recommended)
./validate-constitution.sh && ./gradlew test

# Complete validation (comprehensive)
./test-mcp-complete.sh
```

### Pull Request Requirements

All pull requests must pass:

- ✅ Constitutional compliance validation
- ✅ All 136+ tests passing (100% coverage)
- ✅ STDOUT cleanliness verification
- ✅ Claude Desktop integration testing

### Quality Gates

The constitutional framework enforces these quality gates:

1. **Zero constitutional violations** before merge
2. **100% test coverage maintained** (136+ tests)
3. **MCP protocol compliance** verified
4. **Production readiness** validated

## Advanced Testing

### MCP Inspector Integration

For interactive testing:

```bash
# Install MCP Inspector
npm install -g @anthropic/mcp-inspector

# Test with JAR
mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# Test with Docker
mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp
```

### Performance Testing

Validate the 7-phase testing suite executes within 5 minutes:

```bash
time ./test-mcp-complete.sh
```

### Custom Test Scenarios

Create custom test scenarios in `tests/custom/`:

```bash
mkdir -p tests/custom
echo '#!/bin/bash
# Custom test scenario
echo "Custom test passed"' > tests/custom/my_test.sh
chmod +x tests/custom/my_test.sh
```

## Documentation

### Test Documentation Standards

All test files should include:

```bash
#!/bin/bash
# Test Purpose: Brief description
# Constitutional Principle: Which principle this validates
# Expected Behavior: What should pass/fail
```

### Constitutional Documentation

See `.specify/memory/constitution.md` for:
- Complete constitutional framework
- All 5 core principles
- Governance process
- Amendment procedures

---

## Support

For testing issues:

1. **Review this guide** for common solutions
2. **Check constitutional compliance** with `./validate-constitution.sh`
3. **Validate STDOUT cleanliness** - common Claude Desktop integration issue
4. **Ensure environment variables** are properly configured

**Remember**: The constitutional framework ensures quality, maintainability, and Claude Desktop compatibility. All tests serve the purpose of maintaining these standards.

---

*Constitutional Governance Framework v1.1.0 | Last Updated: 2025-09-20*