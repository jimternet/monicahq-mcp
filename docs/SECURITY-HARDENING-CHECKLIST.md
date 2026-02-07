# Security Hardening Checklist - MonicaHQ MCP Server

**Version**: 1.0
**Date**: 2026-02-06
**Author**: Devil's Advocate Security Review
**Status**: DRAFT - Requires Team Review

---

## Purpose

This checklist ensures the MonicaHQ MCP Server meets production security standards before deployment. Use this document for pre-deployment security audits and compliance verification.

---

## 🚨 CRITICAL SECURITY ISSUES (P0 - MUST FIX)

### P0-001: API Token Exposure in Debug Logs

**Status**: ❌ OPEN
**Severity**: CRITICAL
**Risk**: Complete account compromise if logs are leaked/stolen

**Location**: `/src/main/java/com/monicahq/mcp/client/AuthInterceptor.java:118-120`

**Current Code**:
```java
log.debug("Token validation - length: {} (valid: {}), pattern: {} (valid: {})",
    trimmedToken.length(), lengthValid,
    trimmedToken.substring(0, Math.min(20, trimmedToken.length())) + "...", patternValid);
```

**Required Fix**:
```java
log.debug("Token validation - length: {} (valid: {}), pattern match: {}",
    trimmedToken.length(), lengthValid, patternValid);
// NEVER log token content, even partially
```

**Verification**:
- [ ] Grep codebase for `token.*substring` patterns
- [ ] Verify no token content in any log statement
- [ ] Test with MCP_DEBUG=true to confirm no leakage
- [ ] Review all log statements in AuthInterceptor.java

---

### P0-002: Debug Mode in Production

**Status**: ❌ OPEN
**Severity**: HIGH
**Risk**: Sensitive data exposure, performance degradation

**Location**: Multiple files, `/src/main/resources/application.yml:68`

**Current Configuration**:
```yaml
logging:
  level:
    com.monicahq.mcp: DEBUG  # TOO VERBOSE FOR PRODUCTION
```

**Required Fix**:
1. Create `application-production.yml`:
```yaml
logging:
  level:
    com.monicahq.mcp: WARN
    org.springframework.web: WARN
    org.springframework.websocket: INFO
```

2. Update Docker ENTRYPOINT to force production profile:
```dockerfile
ENV SPRING_PROFILES_ACTIVE=production
```

3. Add startup validation:
```java
@PostConstruct
public void validateProductionSecurity() {
    if (isProduction() && isDebugEnabled()) {
        throw new IllegalStateException("DEBUG logging is not allowed in production");
    }
}
```

**Verification**:
- [ ] Test with SPRING_PROFILES_ACTIVE=production
- [ ] Verify log level is WARN/INFO in production
- [ ] Confirm MCP_DEBUG env var is ignored in production
- [ ] Test startup validation fails if debug enabled

---

### P0-003: Unsafe Environment Variable Handling

**Status**: ⚠️ PARTIAL
**Severity**: MEDIUM
**Risk**: Token exposure via process inspection

**Current Issues**:
1. Tokens stored in plain environment variables (visible in `docker inspect`, `ps aux`)
2. `.env.example` contains actual default passwords
3. No encryption at rest

**Required Fixes**:

1. **Docker Secrets Support**:
```yaml
# docker-compose.yml
services:
  monicahq-mcp:
    secrets:
      - monica_api_token
    environment:
      MONICA_API_TOKEN_FILE: /run/secrets/monica_api_token

secrets:
  monica_api_token:
    external: true
```

2. **Update .env.example**:
```bash
# BAD - DO NOT USE THESE VALUES
# MYSQL_PASSWORD=monica123

# GOOD - Placeholder values
MYSQL_PASSWORD=<generate-secure-password-here>
MONICA_API_TOKEN=<your-oauth2-bearer-token-here>
```

3. **Add Security Documentation**:
```markdown
### Secure Token Storage

**NEVER** commit tokens to version control.

**Recommended Approaches**:
1. Docker Secrets (Swarm/Kubernetes)
2. AWS Secrets Manager / Azure Key Vault
3. HashiCorp Vault
4. Encrypted environment files with git-crypt

**NOT Recommended**:
- Plain environment variables in docker-compose.yml
- Hardcoded tokens in code
- Tokens in logs
```

**Verification**:
- [ ] Docker secrets configuration documented
- [ ] .env.example has no real passwords
- [ ] README.md warns about token security
- [ ] Token storage best practices documented

---

## 🔒 HIGH PRIORITY SECURITY ISSUES (P1 - SHOULD FIX)

### P1-001: Missing Input Sanitization

**Status**: ❌ OPEN
**Severity**: HIGH
**Risk**: XSS, data corruption, injection attacks

**Current Issue**: `MonicaHqClient` accepts raw `Map<String, Object>` without sanitization

**Required Fix**: Add MCP boundary validation layer

```java
@Component
public class InputSanitizer {

    private static final Pattern SCRIPT_PATTERN = Pattern.compile(
        "<script|javascript:|onerror=|onclick=",
        Pattern.CASE_INSENSITIVE
    );

    private static final Pattern HTML_TAG_PATTERN = Pattern.compile(
        "<[^>]+>",
        Pattern.CASE_INSENSITIVE
    );

    public Map<String, Object> sanitize(Map<String, Object> input) {
        Map<String, Object> sanitized = new HashMap<>();

        for (Map.Entry<String, Object> entry : input.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (value instanceof String) {
                sanitized.put(key, sanitizeString((String) value));
            } else if (value instanceof Map) {
                sanitized.put(key, sanitize((Map<String, Object>) value));
            } else if (value instanceof List) {
                sanitized.put(key, sanitizeList((List<?>) value));
            } else {
                sanitized.put(key, value);
            }
        }

        return sanitized;
    }

    private String sanitizeString(String input) {
        if (input == null) return null;

        // Remove script tags and javascript: URIs
        if (SCRIPT_PATTERN.matcher(input).find()) {
            log.warn("Detected potential XSS attempt, sanitizing input");
            return input.replaceAll(SCRIPT_PATTERN.pattern(), "");
        }

        // For fields that shouldn't contain HTML, strip all tags
        // (configure per field in production)
        return input;
    }
}
```

**Integration Point**:
```java
@Service
public class ContactService {
    private final InputSanitizer sanitizer;

    public Mono<Map<String, Object>> createContact(Map<String, Object> arguments) {
        Map<String, Object> sanitized = sanitizer.sanitize(arguments);
        return create(sanitized);
    }
}
```

**Verification**:
- [ ] Add InputSanitizer component
- [ ] Integrate with all service classes
- [ ] Add tests for XSS/injection patterns
- [ ] Document sanitization rules

---

### P1-002: Rate Limiting Configuration Mismatch

**Status**: ❌ OPEN
**Severity**: MEDIUM
**Risk**: API rate limit violations, service degradation

**Current Issue**:
- Configuration exists: `monica.api.rate-limit.requests-per-minute: 60`
- No implementation found
- Only handles HTTP 429 reactively

**Required Fix**: Implement client-side rate limiting OR remove misleading config

**Option A - Implement Rate Limiting**:
```java
@Component
public class RateLimiter {

    private final int requestsPerMinute;
    private final Semaphore semaphore;
    private final ScheduledExecutorService scheduler;

    public RateLimiter(@Value("${monica.api.rate-limit.requests-per-minute}") int rpm) {
        this.requestsPerMinute = rpm;
        this.semaphore = new Semaphore(rpm);
        this.scheduler = Executors.newScheduledThreadPool(1);

        // Refresh permits every minute
        scheduler.scheduleAtFixedRate(
            () -> semaphore.drainPermits() + semaphore.release(rpm),
            1, 1, TimeUnit.MINUTES
        );
    }

    public <T> Mono<T> execute(Supplier<Mono<T>> operation) {
        return Mono.defer(() -> {
            if (semaphore.tryAcquire()) {
                return operation.get();
            } else {
                log.warn("Rate limit reached, throttling request");
                return Mono.delay(Duration.ofSeconds(1))
                    .then(execute(operation));
            }
        });
    }
}
```

**Option B - Remove Configuration**:
Remove misleading config from `application.yml` if not implementing.

**Verification**:
- [ ] Decide: Implement or remove
- [ ] If implementing: Add rate limiter tests
- [ ] If removing: Update documentation
- [ ] Test behavior under high load

---

### P1-003: Request Size Limits

**Status**: ⚠️ PARTIAL
**Severity**: MEDIUM
**Risk**: DoS via large request payloads

**Current**: 1MB limit on RESPONSE size only
**Missing**: Request size validation

**Required Fix**:
```yaml
# application.yml
spring:
  codec:
    max-in-memory-size: 1MB  # Request size limit

server:
  max-http-header-size: 16KB
  max-http-request-header-size: 16KB
```

**Verification**:
- [ ] Add request size limits to config
- [ ] Test with >1MB request payload
- [ ] Verify rejection with 413 Payload Too Large
- [ ] Document limits in API docs

---

## 📋 MEDIUM PRIORITY ISSUES (P2 - RECOMMENDED)

### P2-001: Circuit Breaker Fallback Enhancement

**Status**: ❌ OPEN
**Severity**: LOW
**Recommendation**: Add cache-based fallback

**Current**: Fallback just wraps exception
**Desired**: Graceful degradation with cached data

**Suggested Implementation**:
```java
@Service
public class CachedMonicaClient {
    private final MonicaHqClient client;
    private final ConcurrentHashMap<String, CachedResponse> cache;

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackWithCache")
    public Mono<Map<String, Object>> get(String endpoint, Map<String, String> params) {
        return client.get(endpoint, params)
            .doOnSuccess(response -> cache.put(endpoint, new CachedResponse(response)));
    }

    private Mono<Map<String, Object>> fallbackWithCache(String endpoint, Map<String, String> params, Exception ex) {
        CachedResponse cached = cache.get(endpoint);
        if (cached != null && !cached.isExpired()) {
            log.warn("Using cached response due to API unavailability");
            return Mono.just(cached.getData())
                .map(data -> addStaleDataWarning(data));
        }
        return Mono.error(new RuntimeException("MonicaHQ API unavailable and no cache available", ex));
    }

    private Map<String, Object> addStaleDataWarning(Map<String, Object> data) {
        data.put("_warning", "This data may be stale due to API unavailability");
        return data;
    }
}
```

**Verification**:
- [ ] Implement cache-based fallback
- [ ] Add TTL configuration
- [ ] Test circuit breaker with cache
- [ ] Document stale data handling

---

### P2-002: Error Code Standardization

**Status**: ❌ OPEN
**Severity**: LOW
**Recommendation**: Consolidate error code schemes

**Current Issue**: Overlapping error codes (-32000 used for multiple purposes)

**Proposed Fix**:
```java
public static final class McpErrorCodes {
    // JSON-RPC 2.0 Standard Errors
    public static final int PARSE_ERROR = -32700;
    public static final int INVALID_REQUEST = -32600;
    public static final int METHOD_NOT_FOUND = -32601;
    public static final int INVALID_PARAMS = -32602;
    public static final int INTERNAL_ERROR = -32603;

    // Application-Specific Errors (non-overlapping range)
    public static final int MONICA_CONNECTION_ERROR = -32100;
    public static final int MONICA_AUTHENTICATION_ERROR = -32101;
    public static final int MONICA_VALIDATION_ERROR = -32102;
    public static final int MONICA_API_ERROR = -32103;
    public static final int API_NOT_AVAILABLE = -32104;
    public static final int WEBSOCKET_ERROR = -32105;
    public static final int TOOL_EXECUTION_ERROR = -32106;
    public static final int RATE_LIMIT_ERROR = -32107;
}
```

**Verification**:
- [ ] Update error code definitions
- [ ] Update exception handlers
- [ ] Update error documentation
- [ ] Test error responses

---

## 🔍 VERIFICATION PROCEDURES

### Pre-Deployment Security Audit

Run this checklist before EVERY production deployment:

```bash
#!/bin/bash
# security-audit.sh

echo "🔒 MonicaHQ MCP Security Audit"
echo "================================"

# 1. Token exposure check
echo "Checking for token exposure in logs..."
grep -r "token.*substring\|token.*log" src/main/java && echo "❌ FAIL: Token logging detected" || echo "✅ PASS"

# 2. Debug mode check
echo "Checking production logging configuration..."
grep -A5 "logging:" src/main/resources/application-production.yml | grep "DEBUG" && echo "❌ FAIL: DEBUG logging in production" || echo "✅ PASS"

# 3. Dependency vulnerabilities
echo "Checking for dependency vulnerabilities..."
./gradlew dependencyCheckAnalyze

# 4. Test execution
echo "Running all tests..."
./gradlew clean test

# 5. Build verification
echo "Building production JAR..."
./gradlew clean bootJar

# 6. Docker security scan
echo "Scanning Docker image..."
docker build -t monicahq-mcp:security-scan .
trivy image monicahq-mcp:security-scan

echo "✅ Security audit complete!"
```

**Checklist**:
- [ ] All tests passing (1,780 test cases)
- [ ] No DEBUG logging in production profile
- [ ] No token content in logs
- [ ] No CVE vulnerabilities in dependencies
- [ ] Docker image security scan passed
- [ ] Resource limits configured
- [ ] Health checks functional

---

## 📚 SECURITY DOCUMENTATION REQUIREMENTS

### Required Documentation

1. **Security Policy** (`SECURITY.md`):
   - Responsible disclosure process
   - Supported versions
   - Security contact

2. **Deployment Security Guide**:
   - Token storage best practices
   - Network security configuration
   - TLS/SSL setup for WebSocket mode
   - Docker security hardening

3. **Incident Response Plan**:
   - Token compromise procedure
   - Security breach response
   - Audit log analysis

**Verification**:
- [ ] SECURITY.md exists and is complete
- [ ] Deployment guide includes security section
- [ ] Incident response plan documented

---

## ✅ PRODUCTION READINESS CRITERIA

### Security Sign-Off Requirements

Before production deployment, ALL of the following MUST be verified:

**P0 Issues (MANDATORY)**:
- [ ] P0-001: Token logging removed
- [ ] P0-002: Production logging profile enforced
- [ ] P0-003: Secure token storage documented

**P1 Issues (STRONGLY RECOMMENDED)**:
- [ ] P1-001: Input sanitization implemented
- [ ] P1-002: Rate limiting implemented or config removed
- [ ] P1-003: Request size limits configured

**Security Testing**:
- [ ] Penetration testing completed
- [ ] Security audit passed
- [ ] No CRITICAL/HIGH CVEs in dependencies

**Documentation**:
- [ ] SECURITY.md published
- [ ] Security best practices documented
- [ ] Incident response plan ready

---

## 📞 CONTACT

**Security Issues**: Report to security@monicahq-mcp.example.com
**General Issues**: GitHub Issues
**Questions**: Team Lead

---

**Last Updated**: 2026-02-06
**Next Review**: Before each production deployment
**Document Owner**: Security Team / Devil's Advocate
