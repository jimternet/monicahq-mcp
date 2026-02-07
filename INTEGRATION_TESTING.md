# Integration Testing with Real Monica Instance

This document explains how to run **true end-to-end integration tests** against a real Monica CRM instance using Docker.

## Current Testing Architecture

### 1. Unit/Contract Tests (JUnit)
- **Location**: `src/test/java/`
- **Count**: 1,792 tests
- **Type**: Mock-based (uses `TestMonicaHqClient` stub)
- **Purpose**: Protocol compliance, service logic validation
- **Speed**: Fast (15-20 seconds)
- **Coverage**: ✅ MCP protocol, ❌ Real Monica API

**Run with:**
```bash
./gradlew test
```

### 2. Validation Scripts (Bash + Real API)
- **Location**: `validation/`
- **Type**: Integration tests against **REAL Monica instance**
- **Purpose**: End-to-end CRUD workflows via MCP STDIO protocol
- **Speed**: Slower (requires running MCP server + Monica API)
- **Coverage**: ✅ MCP protocol, ✅ Real Monica API

**Run with:**
```bash
export MONICA_API_URL=https://app.monicahq.com/api
export MONICA_API_TOKEN=your-production-token
./validation/crud/validate-contact-crud.sh
```

### 3. Docker-Based Integration Tests (NEW)
- **Location**: `docker-compose.test.yml`
- **Type**: Fully isolated testing environment
- **Purpose**: Reproducible integration testing with local Monica instance
- **Speed**: Medium (Docker startup overhead)
- **Coverage**: ✅ MCP protocol, ✅ Real Monica API, ✅ Isolated environment

## Quick Start: Docker Integration Testing

### Prerequisites
- Docker Desktop installed
- Docker Compose v2.x
- 4GB available RAM
- 2GB available disk space

### Setup (One-time)

1. **Run automated setup:**
   ```bash
   ./scripts/setup-integration-tests.sh
   ```

   This will:
   - Build the MCP server JAR
   - Generate Monica APP_KEY
   - Start MySQL database
   - Start Monica CRM instance
   - Create `.env.test` configuration

2. **Generate Monica API Token:**
   - Visit: http://localhost:8081
   - Login: `test@example.com` / `test_password_123`
   - Go to: Settings → API → Personal Access Tokens
   - Create token with full permissions
   - Copy the token

3. **Update `.env.test`:**
   ```bash
   # Add the generated token
   MONICA_TEST_API_TOKEN=your-generated-token-here
   ```

### Running Integration Tests

**Option 1: Run all validation scripts in Docker**
```bash
docker-compose -f docker-compose.test.yml --profile test up integration-tests
```

**Option 2: Run specific validation scripts manually**
```bash
# Export environment variables
export MONICA_API_URL=http://localhost:8081/api
export MONICA_API_TOKEN=$(cat .env.test | grep MONICA_TEST_API_TOKEN | cut -d= -f2)

# Run individual test scripts
./validation/crud/validate-contact-crud.sh
./validation/crud/validate-activity-crud.sh
./validation/crud/validate-note-crud.sh

# Or run all CRUD tests
./validation/run-all-crud-tests.sh
```

**Option 3: Run comprehensive test suite**
```bash
export MONICA_API_URL=http://localhost:8081/api
export MONICA_API_TOKEN=$(cat .env.test | grep MONICA_TEST_API_TOKEN | cut -d= -f2)

./validation/integration/test-mcp-complete.sh
```

## Docker Services

The test environment includes 4 services:

### 1. `monica-db` (MySQL 8.0)
- Port: 3306 (internal only)
- Database: `monica`
- User: `monica_user`
- Password: `monica_pass`

### 2. `monica` (Monica CRM)
- Port: 8081 → 80
- URL: http://localhost:8081
- Admin: test@example.com / test_password_123
- API: http://localhost:8081/api

### 3. `monicahq-mcp-test` (MCP Server)
- Port: 8082 → 8080
- Connected to local Monica instance
- Health: http://localhost:8082/actuator/health

### 4. `integration-tests` (Test Runner)
- Profile: `test` (only runs when explicitly requested)
- Executes all validation scripts
- Outputs results to `/test-results`

## Managing the Test Environment

### Start services
```bash
docker-compose -f docker-compose.test.yml up -d
```

### Check status
```bash
docker-compose -f docker-compose.test.yml ps
```

### View logs
```bash
# All services
docker-compose -f docker-compose.test.yml logs -f

# Specific service
docker-compose -f docker-compose.test.yml logs -f monica
docker-compose -f docker-compose.test.yml logs -f monicahq-mcp-test
```

### Stop services
```bash
docker-compose -f docker-compose.test.yml down
```

### Clean up (remove volumes)
```bash
docker-compose -f docker-compose.test.yml down -v
```

## Test Coverage Comparison

| Test Type | Location | Monica API | Isolation | Speed | CI/CD Ready |
|-----------|----------|------------|-----------|-------|-------------|
| **Unit Tests** | `src/test/java/` | ❌ Mock | ✅ Full | ⚡ Fast | ✅ Yes |
| **Validation Scripts** | `validation/` | ✅ Real | ⚠️ Requires credentials | 🐢 Slow | ⚠️ Depends |
| **Docker Integration** | `docker-compose.test.yml` | ✅ Real | ✅ Full | 🚀 Medium | ✅ Yes |

## CI/CD Integration

### GitHub Actions Example
```yaml
name: Integration Tests

on: [push, pull_request]

jobs:
  integration-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build JAR
        run: ./gradlew build -x test

      - name: Start Monica Test Environment
        run: |
          docker-compose -f docker-compose.test.yml up -d monica-db monica
          sleep 60  # Wait for Monica to initialize

      - name: Generate Monica API Token
        run: |
          # Script to automate token generation via Monica API
          ./scripts/generate-test-token.sh

      - name: Run Integration Tests
        run: |
          docker-compose -f docker-compose.test.yml --profile test up integration-tests

      - name: Cleanup
        if: always()
        run: docker-compose -f docker-compose.test.yml down -v
```

## Troubleshooting

### Monica not starting
```bash
# Check Monica logs
docker-compose -f docker-compose.test.yml logs monica

# Common issues:
# - APP_KEY not set correctly
# - Database not ready (wait longer)
# - Port 8081 already in use
```

### MCP Server connection refused
```bash
# Check MCP server logs
docker-compose -f docker-compose.test.yml logs monicahq-mcp-test

# Common issues:
# - MONICA_API_TOKEN not set
# - Monica not healthy yet
# - Network connectivity between containers
```

### Tests failing with 401 Unauthorized
```bash
# Verify token is valid
curl -H "Authorization: Bearer $MONICA_API_TOKEN" \
  http://localhost:8081/api/me

# Regenerate token if expired
```

## Best Practices

1. **Always use Docker for integration testing** - ensures reproducibility
2. **Don't commit `.env.test`** - contains sensitive tokens (already in `.gitignore`)
3. **Reset Monica database between test runs** - use `docker-compose down -v` to clean state
4. **Run unit tests first** - they're faster and catch most issues
5. **Use validation scripts for regression testing** - comprehensive CRUD workflows

## Next Steps

- [ ] Automate token generation in setup script
- [ ] Add performance benchmarking to integration tests
- [ ] Create GitHub Actions workflow
- [ ] Add test data seeding script
- [ ] Create test report aggregation

## References

- [Monica Docker Documentation](https://docs.monicahq.com/developers/docker)
- [Monica Official Docker Image](https://hub.docker.com/_/monica)
- [Monica GitHub Repository](https://github.com/monicahq/monica)

---

**Status**: ✅ Ready for use (Phase 2 implementation)
**Last Updated**: 2026-02-07
