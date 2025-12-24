# Docker Integration Testing Guide

This document provides comprehensive instructions for running Docker integration tests against a live Monica API instance. These tests validate the MonicaHQ MCP server's behavior with real API responses, complementing the unit tests that use stubbed clients.

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Environment Setup](#environment-setup)
- [Running Tests](#running-tests)
- [Test Structure](#test-structure)
- [Writing New Tests](#writing-new-tests)
- [Troubleshooting](#troubleshooting)
- [Security Considerations](#security-considerations)

---

## Overview

The Docker testing framework provides integration tests that run against a live Monica API. Unlike unit tests that use `TestMonicaHqClient` with stubbed responses, Docker tests:

- Execute real HTTP requests to the Monica API
- Validate actual API response structures and data
- Test authentication with JWT Bearer tokens
- Verify error handling for real API errors (404, 401, 422, etc.)
- Test multi-step workflows with data persistence

### When to Use Docker Tests

| Scenario | Use Docker Tests | Use Unit Tests |
|----------|-----------------|----------------|
| Rapid development iteration | No | Yes |
| Validating API contract changes | Yes | No |
| CI/CD pipeline (quick feedback) | No | Yes |
| Pre-release validation | Yes | Yes |
| Testing error handling | Yes | Yes |

---

## Prerequisites

Before running Docker tests, ensure you have:

### 1. Monica API Access

You need a running Monica API instance with:
- Valid API credentials (JWT Bearer token)
- Network connectivity from your development machine
- Sufficient API rate limit quota (tests run serially to stay within limits)

**Options:**
- **Monica Cloud**: Use `https://app.monicahq.com/api` (requires account)
- **Self-hosted Monica**: Deploy locally or on a server

### 2. JWT Bearer Token

Obtain a JWT token from your Monica instance:

1. Log into your Monica account
2. Navigate to Settings > API
3. Generate a new Personal Access Token
4. Copy the full JWT token (starts with `eyJ0eXAiOiJKV1Qi...`)

**Token Details:**
- Algorithm: RS256
- Required scopes: Full access to contacts, notes, and related resources
- The token must be valid (not expired)

### 3. MCP Server Running

The MonicaHQ MCP server must be running for Docker tests:

```bash
# Option 1: Via Docker Compose
docker-compose up monicahq-mcp

# Option 2: Via Gradle
./gradlew bootRun

# Option 3: Via IDE
# Run MonicaHqMcpApplication.main()
```

### 4. Health Check Verification

Verify the server is healthy before running tests:

```bash
curl http://localhost:8080/actuator/health
# Expected: {"status":"UP"}
```

---

## Environment Setup

### Required Environment Variables

Set these environment variables before running Docker tests:

```bash
# Required: Monica API URL
export MONICA_API_URL="https://app.monicahq.com/api"

# Required: JWT Bearer Token (your personal access token)
export MONICA_API_TOKEN="eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9..."
```

### Optional Environment Variables

```bash
# Override health check endpoint (default: http://localhost:8080/actuator/health)
export DOCKER_TEST_HEALTH_URL="http://localhost:8080/actuator/health"
```

### Configuration File

Docker tests use the profile `docker-test` which loads configuration from:
`src/test/resources/application-docker-test.yml`

Key configuration properties:

```yaml
monica:
  api:
    url: ${MONICA_API_URL:https://app.monicahq.com/api}
    token: ${MONICA_API_TOKEN}  # Required - no default
    timeout: 30s
    max-retries: 3
    rate-limit:
      requests-per-minute: 60

spring:
  profiles:
    active: docker-test
  main:
    web-application-type: none  # Faster test startup
```

### Using .env File (Recommended)

Create a `.env` file in the project root for local development:

```bash
# .env (DO NOT COMMIT TO VERSION CONTROL)
MONICA_API_URL=https://app.monicahq.com/api
MONICA_API_TOKEN=eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9...
```

Load the environment before running tests:

```bash
source .env && ./gradlew dockerTest
```

> **Security Note:** Add `.env` to your `.gitignore` to prevent committing tokens.

---

## Running Tests

### Run All Docker Tests

```bash
./gradlew dockerTest
```

### Run Specific Test Class

```bash
# Run a single test class
./gradlew dockerTest --tests '*DockerContactCreateTest'

# Run all contract tests
./gradlew dockerTest --tests '*contract*'

# Run all integration tests
./gradlew dockerTest --tests '*integration*'
```

### Run Specific Test Method

```bash
./gradlew dockerTest --tests '*DockerContactCreateTest.shouldCreateContactViaMcpProtocol'
```

### Run with Verbose Output

```bash
./gradlew dockerTest --info
```

### Run with Debug Logging

```bash
./gradlew dockerTest -Dlogging.level.com.monicahq.mcp=DEBUG
```

### Gradle Task Details

The `dockerTest` task is configured with:

```groovy
task dockerTest(type: Test) {
    useJUnitPlatform()
    include '**/docker/**'            // Only Docker tests
    maxParallelForks = 1              // Serial execution (rate limiting)
    timeout = Duration.ofMinutes(15)  // Extended timeout for network calls
}
```

**Important:** Docker tests are excluded from the standard `./gradlew test` task to keep unit tests fast.

---

## Test Structure

### Package Organization

```
src/test/java/com/monicahq/mcp/docker/
├── DockerBaseTest.java              # Abstract base class
├── contract/                        # Contract tests (single operations)
│   ├── DockerContactCreateTest.java
│   ├── DockerContactListTest.java
│   ├── DockerContactGetTest.java
│   ├── DockerNoteCreateTest.java
│   └── DockerAuthenticationTest.java
└── integration/                     # Integration tests (workflows)
    ├── DockerContactNoteFlowTest.java
    └── DockerContactLifecycleTest.java
```

### Test Types

#### Contract Tests (`docker/contract/`)

Validate individual MCP operations against the live API:

| Test Class | What It Tests |
|------------|---------------|
| `DockerContactCreateTest` | Contact creation with various field combinations |
| `DockerContactListTest` | Contact listing, pagination, search filtering |
| `DockerContactGetTest` | Contact retrieval by ID, 404 handling |
| `DockerNoteCreateTest` | Note creation, validation, contact association |
| `DockerAuthenticationTest` | JWT token validation, 401 error handling |

#### Integration Tests (`docker/integration/`)

Validate multi-step workflows:

| Test Class | Workflow |
|------------|----------|
| `DockerContactNoteFlowTest` | Create contact → Add notes → Retrieve → Verify |
| `DockerContactLifecycleTest` | Create → Update → Get → Delete → Verify deletion |

### Base Test Class

All Docker tests extend `DockerBaseTest` which provides:

- **@BeforeAll**: Health check verification
- **@AfterEach**: Automatic cleanup of test data (contacts, notes)
- **Helper methods**: `callTool()`, `sendMcpRequest()`, `createContactAndTrack()`
- **Response utilities**: `isSuccessResponse()`, `isErrorResponse()`, `getErrorCode()`

---

## Writing New Tests

### Basic Test Template

```java
package com.monicahq.mcp.docker.contract;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class DockerMyNewTest extends DockerBaseTest {

    @Test
    void shouldDoSomethingAgainstLiveApi() {
        // Arrange: Create test data (tracked for cleanup)
        Long contactId = createContactAndTrack("Test", "User", 1L);
        assertNotNull(contactId, "Contact should be created");

        // Act: Call MCP tool
        Map<String, Object> response = callTool("my_tool", Map.of(
            "arg1", "value1",
            "arg2", 123
        ), 1);

        // Assert: Validate response
        assertTrue(isSuccessResponse(response));
        assertNotNull(response.get("result"));
    }

    @Test
    void shouldHandleErrorCase() {
        // Act: Call with invalid data
        Map<String, Object> response = callTool("my_tool", Map.of(
            "invalidArg", "value"
        ), 2);

        // Assert: Verify error response
        assertTrue(isErrorResponse(response));
        assertEquals(-32001, getErrorCode(response));
        assertTrue(getErrorMessage(response).contains("expected error text"));
    }
}
```

### Best Practices

1. **Extend DockerBaseTest**: Always extend the base class for health checks and cleanup
2. **Track created resources**: Use `createContactAndTrack()` and `createdContactIds.add()` for cleanup
3. **Use unique identifiers**: Include timestamps in test data names to avoid conflicts
4. **Assert both success and structure**: Check `isSuccessResponse()` AND validate response fields
5. **Test error cases**: Include tests for 404, validation errors, and edge cases

---

## Troubleshooting

### Common Errors

#### 1. "Monica API not available"

```
IllegalStateException: Monica API not available at http://localhost:8080/actuator/health
```

**Solution:**
- Ensure the MCP server is running: `./gradlew bootRun`
- Check the health endpoint: `curl http://localhost:8080/actuator/health`
- Verify the port is correct (default: 8080)

#### 2. "Health check failed with status: 401"

```
IllegalStateException: Health check failed with status: 401 UNAUTHORIZED
```

**Solution:**
- The health endpoint should not require authentication
- Check `management.endpoints.web.exposure.include` in application configuration
- Verify actuator security settings

#### 3. "MONICA_API_TOKEN is not set"

```
IllegalArgumentException: Could not resolve placeholder 'MONICA_API_TOKEN'
```

**Solution:**
- Set the environment variable: `export MONICA_API_TOKEN="your-token-here"`
- Or use a `.env` file: `source .env && ./gradlew dockerTest`

#### 4. "401 Unauthorized" in Test Results

**Causes:**
- Invalid or expired JWT token
- Token missing required scopes
- Wrong API URL

**Solution:**
- Generate a new token in Monica Settings > API
- Verify the token is complete (no truncation)
- Test with curl: `curl -H "Authorization: Bearer $MONICA_API_TOKEN" $MONICA_API_URL/me`

#### 5. "Connection refused"

```
ConnectException: Connection refused
```

**Solution:**
- Check if Monica API URL is correct
- Verify network connectivity
- For local Monica: ensure Docker container is running

#### 6. "Rate limit exceeded"

```
TooManyRequestsException: Rate limit exceeded
```

**Solution:**
- Docker tests run serially (`maxParallelForks = 1`) by default
- Wait a minute before re-running tests
- Check Monica API rate limits (typically 60 req/min)

#### 7. Tests Timeout

```
TestTimedOutException: test timed out after 15 minutes
```

**Solution:**
- Check network connectivity
- Verify Monica API is responsive
- Increase timeout if needed in `build.gradle`

### Debug Mode

Enable detailed logging for troubleshooting:

```bash
./gradlew dockerTest \
  -Dlogging.level.com.monicahq.mcp.client=DEBUG \
  -Dlogging.level.org.springframework.web.reactive=DEBUG
```

### Verify Environment

Run this command to verify your setup:

```bash
echo "MONICA_API_URL: ${MONICA_API_URL:-NOT SET}"
echo "MONICA_API_TOKEN: ${MONICA_API_TOKEN:+SET (hidden)}"
echo "Health check:"
curl -s http://localhost:8080/actuator/health | jq .
echo "API connectivity:"
curl -s -H "Authorization: Bearer $MONICA_API_TOKEN" "$MONICA_API_URL/me" | jq .
```

---

## Security Considerations

### Token Handling

**DO:**
- Store tokens in environment variables
- Use `.env` files for local development (add to `.gitignore`)
- Use secret management systems in CI/CD
- Regenerate tokens periodically

**DON'T:**
- Hardcode tokens in source code
- Commit tokens to version control
- Log tokens in test output
- Share tokens between team members

### Example .gitignore

```gitignore
# Environment files with secrets
.env
.env.local
.env.*.local

# IDE files that might contain secrets
.idea/
*.iml
.vscode/
```

### Token Expiration

The JWT token has an expiration date. If tests suddenly start failing with 401 errors:

1. Check token expiration: Decode the JWT at [jwt.io](https://jwt.io)
2. Look for `exp` claim in the payload
3. Generate a new token if expired

### CI/CD Integration

For automated testing:

```yaml
# GitHub Actions example
jobs:
  docker-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run Docker Tests
        env:
          MONICA_API_URL: ${{ secrets.MONICA_API_URL }}
          MONICA_API_TOKEN: ${{ secrets.MONICA_API_TOKEN }}
        run: ./gradlew dockerTest
```

---

## Quick Reference

### Essential Commands

```bash
# Set environment
export MONICA_API_URL="https://app.monicahq.com/api"
export MONICA_API_TOKEN="eyJ0eXAiOiJKV1Qi..."

# Start MCP server
./gradlew bootRun

# Verify health
curl http://localhost:8080/actuator/health

# Run all Docker tests
./gradlew dockerTest

# Run specific test
./gradlew dockerTest --tests '*DockerContactCreateTest'

# Run with verbose output
./gradlew dockerTest --info
```

### Test Counts by Category

| Category | Test Count | Purpose |
|----------|------------|---------|
| Contract Tests | 25+ | Individual MCP operations |
| Integration Tests | 10+ | Multi-step workflows |
| **Total** | **35+** | **Full API coverage** |

### API Endpoints Tested

| Endpoint | Operations |
|----------|------------|
| `/contacts` | create, list, get, update, delete |
| `/notes` | create, list, delete |
| `/me` | Authentication verification |
