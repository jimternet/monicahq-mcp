# Contributing to MonicaHQ MCP Server

First off, thank you for considering contributing to the MonicaHQ MCP Server! This project bridges Monica CRM with Claude Desktop through the Model Context Protocol, and we welcome contributions that improve this integration.

## Table of Contents
- [Code of Conduct](#code-of-conduct)
- [Constitutional Governance Framework](#constitutional-governance-framework)
- [Getting Started](#getting-started)
- [Development Process](#development-process)
- [Code Style Guidelines](#code-style-guidelines)
- [Commit Message Conventions](#commit-message-conventions)
- [Testing Requirements](#testing-requirements)
- [Pull Request Process](#pull-request-process)
- [Reporting Issues](#reporting-issues)

## Code of Conduct

Please read our [Code of Conduct](CODE_OF_CONDUCT.md) before contributing. We are committed to providing a welcoming and inspiring community for all.

## Constitutional Governance Framework

This project follows a **Constitutional Governance Framework (v1.3.0)** with 6 core principles that MUST be maintained:

### 1. MCP Protocol First
- Every operation MUST follow JSON-RPC 2.0 over STDIO specification
- STDOUT must remain clean (only JSON-RPC responses)
- Tool categorization for optimal discoverability
- Claude Desktop integration is the priority

### 2. Test-Driven Development (Non-Negotiable)
- Write tests BEFORE implementation
- Maintain 100% test coverage (currently 188 tests)
- Follow test hierarchy: Contract → Integration → E2E → Unit
- Every PR must include tests

### 3. Spring Boot Architecture Excellence
- Use Spring Boot 3.x with WebFlux for reactive operations
- All external API calls must be non-blocking
- Follow dependency injection patterns
- Maintain dual-mode architecture (STDIO + Web Server)

### 4. Production-Ready Deployment
- Docker support from day one
- Environment variable configuration
- Health checks and monitoring
- Claude Desktop configuration templates

### 5. Type Safety & Code Generation
- Use MapStruct for type-safe mapping
- Leverage Lombok for boilerplate reduction
- No raw types or unchecked warnings
- DTOs for all Monica entities

### 6. Complete Monica API Data Access
- ALL fields from Monica API must be visible in MCP responses
- Use generic content formatting for unknown fields
- Never filter or hide API data

## Getting Started

### Prerequisites
- Java 17+ (OpenJDK or Oracle JDK)
- Docker and Docker Compose
- A Monica instance with API access
- Git

### Development Setup

1. **Fork and Clone**
   ```bash
   git clone https://github.com/YOUR-USERNAME/monicahq-mcp.git
   cd monicahq-mcp
   ```

2. **Set Up Environment**
   ```bash
   cp .env.example .env
   # Edit .env with your Monica API credentials
   ```

3. **Start Development Stack**
   ```bash
   make dev-up  # Starts Monica + MCP Server + Database
   ```

4. **Run Tests**
   ```bash
   make test  # Run all tests
   make dev-validate  # Run constitutional validation
   ```

5. **Access Services**
   - Monica CRM: http://localhost:8081
   - MCP Server: http://localhost:8080
   - Health Check: http://localhost:8080/actuator/health

## Development Process

### 1. Create a Feature Branch
```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-description
```

### 2. Follow TDD Methodology
1. Write failing tests first (RED)
2. Implement minimum code to pass (GREEN)
3. Refactor while keeping tests green (REFACTOR)

### 3. Validate Constitutional Compliance
```bash
./validation/constitutional/validate-constitution.sh
```

### 4. Run Complete Test Suite
```bash
./gradlew test  # Must show 188/188 passing
```

## Code Style Guidelines

### Java Code Style
- Use 4 spaces for indentation (no tabs)
- Maximum line length: 120 characters
- Use meaningful variable and method names
- Follow Java naming conventions
- Add JavaDoc for all public methods

### Spring Boot Patterns
```java
// Use constructor injection
@Service
@RequiredArgsConstructor
public class ContactService {
    private final MonicaHqClient monicaClient;
    private final ContactMapper contactMapper;
    
    // Reactive patterns for I/O
    public Mono<Contact> getContact(Long id) {
        return monicaClient.getContact(id)
            .map(contactMapper::toDto);
    }
}
```

### DTO Structure
```java
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Contact {
    private Long id;
    private String firstName;
    private String lastName;
    // Include ALL Monica API fields
    private Map<String, Object> additionalProperties;
}
```

## Commit Message Conventions

We follow [Conventional Commits](https://www.conventionalcommits.org/):

### Format
```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Test additions or corrections
- `chore`: Maintenance tasks
- `perf`: Performance improvements

### Examples
```bash
feat(contact): add support for custom contact fields

Implements custom field management for contacts including:
- CRUD operations for contact fields
- Type validation for field values
- Integration with Monica API v5

Closes #123
```

```bash
fix(mcp): prevent STDOUT contamination in STDIO mode

- Redirect all logs to STDERR
- Suppress Spring Boot banner
- Clean JSON-RPC responses only

Fixes #456
```

## Testing Requirements

### Test Categories

1. **Contract Tests** (`src/test/java/com/monicahq/mcp/contract/`)
   - Test individual MCP operations
   - Mock Monica API responses
   - Validate request/response formats

2. **Integration Tests** (`src/test/java/com/monicahq/mcp/integration/`)
   - Test complete workflows
   - Validate constitutional compliance
   - Check STDOUT cleanliness

3. **Unit Tests**
   - Test individual components
   - Validate mappers and utilities
   - Check business logic

### Writing Tests
```java
@Test
@DisplayName("Should create contact with all fields")
void testCreateContactComplete() {
    // Given
    var request = createContactRequest();
    
    // When
    var response = mcpClient.execute(request);
    
    // Then
    assertThat(response).isNotNull();
    assertThat(response.getResult()).containsKey("id");
    // Verify ALL fields are present
}
```

### Test Coverage
- Minimum: 100% line coverage
- Current: 188 tests passing
- Run coverage: `./gradlew test jacocoTestReport`

## Pull Request Process

1. **Before Creating PR**
   - Ensure all tests pass
   - Run constitutional validation
   - Update documentation if needed
   - Follow commit conventions

2. **Create Pull Request**
   - Use the PR template
   - Fill out all sections
   - Link related issues
   - Add screenshots if UI changes

3. **PR Checklist**
   - [ ] Constitutional compliance verified
   - [ ] All tests passing (188/188)
   - [ ] Documentation updated
   - [ ] No STDOUT contamination
   - [ ] Commit messages follow conventions

4. **Review Process**
   - At least one approval required
   - CI checks must pass
   - Constitutional validation must pass
   - Address all review comments

5. **After Merge**
   - Delete your feature branch
   - Pull latest main branch
   - Celebrate your contribution! 🎉

## Reporting Issues

### Bug Reports
Use the [bug report template](.github/ISSUE_TEMPLATE/bug_report.md) and include:
- Steps to reproduce
- Expected vs actual behavior
- Environment details
- Error messages/logs
- Validation results

### Feature Requests
Use the [feature request template](.github/ISSUE_TEMPLATE/feature_request.md) and include:
- Use case description
- Monica API alignment
- Constitutional compliance consideration
- Success criteria

## Project Structure

```
monicahq_mcp/
├── src/main/java/com/monicahq/mcp/
│   ├── controller/      # MCP message handlers
│   ├── service/         # Business logic (122 operations)
│   ├── dto/            # Data models (23 entities)
│   ├── mapper/         # MapStruct mappers
│   └── client/         # Monica API client
├── src/test/           # Test suite (188 tests)
├── validation/         # Constitutional validation
├── docker-compose*.yml # Docker configurations
└── Makefile           # Development commands
```

## Useful Commands

```bash
# Development
make dev-up        # Start development stack
make dev-down      # Stop development stack
make dev-validate  # Run validation suite
make dev-test      # Run comprehensive tests
make dev-logs      # View logs

# Testing
make test              # Run all tests
make test-constitutional  # Constitutional validation
make validate-all      # Complete validation

# Building
make build         # Build JAR
make docker-build  # Build Docker image

# Status
make status        # Check environment status
```

## Getting Help

- **Documentation**: [README.md](README.md), [TESTING-GUIDE.md](TESTING-GUIDE.md)
- **Issues**: [GitHub Issues](https://github.com/jimternet/monicahq-mcp/issues)
- **Monica API**: [Monica API Docs](https://github.com/monicahq/monica/blob/main/docs/api/readme.md)
- **MCP Protocol**: [MCP Specification](https://modelcontextprotocol.io)

## Recognition

Contributors will be recognized in:
- Git commit history
- GitHub contributors page
- Special mentions for significant contributions

Thank you for contributing to MonicaHQ MCP Server! Your efforts help make Monica CRM more accessible through Claude Desktop.