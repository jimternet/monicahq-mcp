# Research Report: MonicaHQ MCP Server Implementation

**Feature**: MonicaHQ MCP Server Integration  
**Date**: 2025-09-13  
**Status**: Complete

## Executive Summary
Research conducted to resolve technical decisions for implementing an MCP server that interfaces with MonicaHQ CRM. Focus areas include MCP protocol specification, MonicaHQ API capabilities, Spring Boot integration patterns, and authentication strategies.

## Key Decisions

### 1. MCP Protocol Implementation
**Decision**: Use MCP SDK for Java/Spring Boot with JSON-RPC 2.0  
**Rationale**: 
- MCP (Model Context Protocol) is standardized for AI tool integration
- JSON-RPC 2.0 provides structured request/response handling
- Spring Boot's WebSocket support enables persistent connections
**Alternatives Considered**:
- REST-only implementation: Rejected due to MCP requirement for bidirectional communication
- GraphQL: Overcomplicated for this use case

### 2. MonicaHQ Authentication
**Decision**: OAuth2 Bearer token authentication  
**Rationale**:
- MonicaHQ API requires OAuth2 Bearer tokens in Authorization header
- Format: "Authorization: Bearer OAUTH-TOKEN"
- Supports both OAuth2 personal access tokens and server-to-server
- Tokens can be configured via environment variables
**Alternatives Considered**:
- Simple API key: Not supported by MonicaHQ API
- Basic Auth: Less secure and not supported

### 3. Spring Boot Architecture
**Decision**: Reactive WebFlux for async operations  
**Rationale**:
- Non-blocking I/O for concurrent MonicaHQ API calls
- Better handling of rate limits with backpressure
- Natural fit for MCP's async message handling
**Alternatives Considered**:
- Traditional Spring MVC: Would block threads during API calls
- Vert.x: Less familiar to Spring Boot developers

### 4. Error Handling Strategy
**Decision**: Circuit breaker pattern with Resilience4j  
**Rationale**:
- Graceful handling of MonicaHQ downtime
- Built-in retry logic with exponential backoff
- Rate limit protection
**Alternatives Considered**:
- Simple try-catch: Insufficient for production use
- Hystrix: Deprecated in favor of Resilience4j

### 5. Data Transformation
**Decision**: MapStruct for DTO mapping  
**Rationale**:
- Compile-time code generation (no reflection overhead)
- Type-safe mappings between MonicaHQ and MCP formats
- Clean separation of concerns
**Alternatives Considered**:
- Manual mapping: Error-prone and verbose
- ModelMapper: Runtime overhead and less type safety

### 6. Configuration Management
**Decision**: Spring Boot externalized configuration with profiles  
**Rationale**:
- Environment-specific settings (dev/test/prod)
- Secure credential management via environment variables
- YAML format for hierarchical configuration
**Alternatives Considered**:
- Properties files only: Less readable for complex config
- Hardcoded values: Security risk and inflexible

### 7. Testing Strategy
**Decision**: MockWebServer for MonicaHQ API mocking  
**Rationale**:
- Realistic HTTP interaction testing
- Control over response timing and errors
- No dependency on live MonicaHQ instance for tests
**Alternatives Considered**:
- WireMock: More complex setup for simple needs
- Live API testing: Slow and unreliable

### 8. Connection Management
**Decision**: Connection pooling with configurable keep-alive  
**Rationale**:
- Reuse HTTP connections to MonicaHQ
- Configurable pool size based on load
- Automatic reconnection on failure
**Alternatives Considered**:
- Single connection: Performance bottleneck
- No pooling: Resource wastage

### 9. MCP Server Port Configuration
**Decision**: Configurable port with default 8080, MCP WebSocket on /mcp  
**Rationale**:
- Standard Spring Boot port configuration
- Dedicated WebSocket endpoint for MCP protocol
- Health check endpoint on /actuator/health
**Alternatives Considered**:
- Random port: Harder for clients to connect
- Fixed non-standard port: May conflict with other services

### 10. Concurrent Client Support
**Decision**: Multi-session support with session isolation  
**Rationale**:
- Each MCP client gets isolated session
- Shared MonicaHQ connection pool
- Thread-safe operation handling
**Alternatives Considered**:
- Single client only: Too limiting for automation scenarios
- Process per client: Resource intensive

## MonicaHQ API Capabilities Research

### API Version
- Current stable: v1
- Base URL format: `https://{instance}/api`
- Rate limits: Typically 60 requests per minute

### Complete MonicaHQ API Endpoints (2024)
**29 Entity Types with Full API Support:**

| Entity | API Endpoint | Status |
|--------|-------------|---------|
| Activities | /api/activities | ✅ Confirmed |
| Activity Types | /api/activitytypes | ✅ Confirmed |
| Activity Type Categories | /api/activitytypecategories | ✅ Confirmed |
| Addresses | /api/addresses | ✅ Confirmed |
| Audit Logs | /api/auditlogs | ✅ Confirmed |
| Calls | /api/calls | ✅ Confirmed |
| Companies | /api/companies | ✅ Confirmed |
| Contacts | /api/contacts | ✅ Confirmed |
| Contact Fields | /api/contactfields | ✅ Confirmed |
| Contact Field Types | /api/contactfieldtypes | ✅ Confirmed |
| Conversations | /api/conversations | ✅ Confirmed |
| Countries | /api/countries | ✅ Confirmed |
| Currencies | /api/currencies | ✅ Confirmed |
| Debts | /api/debts | ✅ Confirmed |
| Documents | /api/documents | ✅ Confirmed |
| Genders | /api/genders | ✅ Confirmed |
| Gifts | /api/gifts | ✅ Confirmed |
| Groups | /api/groups | ✅ Confirmed |
| Journal Entries | /api/journal | ✅ Confirmed |
| Notes | /api/notes | ✅ Confirmed |
| Occupations | /api/occupations | ✅ Confirmed |
| Photos | /api/photos | ✅ Confirmed |
| Relationships | /api/relationships | ✅ Confirmed |
| Relationship Types | /api/relationshiptypes | ✅ Confirmed |
| Relationship Type Groups | /api/relationshiptypegroups | ✅ Confirmed |
| Reminders | /api/reminders | ✅ Confirmed |
| Tags | /api/tags | ✅ Confirmed |
| Tasks | /api/tasks | ✅ Confirmed |
| Users | /api/users | ✅ Confirmed |

**Total: 29 API endpoints supporting standard HTTP methods (GET, POST, PUT, DELETE)**

### Pagination Support
- Standard page/limit parameters  
- Default page size: 10 (not 15)
- Maximum page size: 100
- Response includes links and meta objects for navigation

### Response Format
- JSON with consistent structure
- Includes metadata (timestamps, relationships)
- Error responses follow RFC 7807

## Spring Boot Implementation Patterns

### Project Structure (Gradle)
```
src/main/java/com/monicahq/mcp/
├── config/          # Configuration classes
├── controller/      # MCP WebSocket handlers
├── service/         # Business logic
├── client/          # MonicaHQ API client
├── dto/            # Data transfer objects
├── mapper/         # MapStruct mappers
└── exception/      # Custom exceptions

build.gradle         # Gradle build configuration
settings.gradle      # Gradle settings
```

### Dependencies Required (Gradle Groovy DSL)
```groovy
dependencies {
    implementation 'org.springframework.boot:spring-boot-starter-webflux'
    implementation 'org.springframework.boot:spring-boot-starter-websocket'
    implementation 'org.springframework.boot:spring-boot-starter-validation'
    implementation 'io.github.resilience4j:resilience4j-spring-boot3'
    implementation 'org.mapstruct:mapstruct'
    implementation 'org.projectlombok:lombok'
    
    testImplementation 'org.springframework.boot:spring-boot-starter-test'
    testImplementation 'com.squareup.okhttp3:mockwebserver'
    
    annotationProcessor 'org.mapstruct:mapstruct-processor'
    annotationProcessor 'org.projectlombok:lombok'
}
```

## Security Considerations

### Token Storage
- Environment variable: `MONICA_API_TOKEN`
- Never logged or exposed in responses
- Encrypted in memory using Spring Security

### Input Validation
- Bean Validation API for all inputs
- Sanitization of user-provided data
- SQL injection not applicable (no direct DB)

### HTTPS Requirements
- TLS for MonicaHQ communication
- Optional TLS for local MCP server
- Certificate validation configurable

## Performance Optimizations

### Caching Strategy
- No caching initially (data freshness priority)
- Future: Redis for frequently accessed contacts
- ETag support for conditional requests

### Batch Operations
- Bulk create/update via parallel streams
- Chunked processing for large datasets
- Progress reporting via MCP status messages

## Deployment Considerations

### Docker Support
- Multi-stage build for smaller image
- JRE-only runtime image
- Environment-based configuration

### Monitoring
- Spring Boot Actuator endpoints
- Micrometer metrics for Prometheus
- Structured logging with correlation IDs

## Next Steps for Implementation

1. Initialize Spring Boot project via start.spring.io
2. Implement MCP protocol handler
3. Create MonicaHQ client with Resilience4j
4. Define DTOs and mappers
5. Implement each entity service
6. Add comprehensive test coverage
7. Create Docker packaging
8. Write deployment documentation

## Unresolved Questions - UPDATED 2024

**RESOLVED:**
- Authentication method: ✅ OAuth2 Bearer tokens confirmed
- Rate limiting: ✅ 60 requests/minute confirmed  
- Pagination: ✅ Default 10 items, max 100 confirmed

**RESOLVED - COMPLETE API DISCOVERY:**
- ✅ All 29 MonicaHQ API endpoints discovered and documented
- ✅ Core entities (contacts, activities, notes, reminders, calls, tasks, tags, journal, conversations) confirmed
- ✅ Extended entities identified (companies, gifts, documents, photos, addresses, relationships, debts, groups, users, etc.)
- ✅ Authentication flow confirmed (OAuth2 Bearer tokens)

**IMPLEMENTATION APPROACH:**
- **Phase 1**: Implement 12 core entities for essential MCP functionality
- **Phase 2**: Add extended entities based on user demand
- **Total Potential**: 29 entities × 5 operations each = 145+ MCP tools available

**IMMEDIATE REQUIREMENTS:**
- Update MCP contract to reflect complete capabilities
- Design modular architecture to support all 29 endpoints
- Prioritize most commonly used entities for initial release

---
*Research completed successfully. Ready for Phase 1: Design & Contracts*