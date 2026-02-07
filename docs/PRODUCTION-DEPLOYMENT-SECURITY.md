# Production Deployment Security Requirements

**Version**: 1.0
**Date**: 2026-02-06
**Audience**: DevOps, SRE, Security Teams
**Status**: REQUIRED for production deployments

---

## Purpose

This document defines mandatory security requirements for deploying the MonicaHQ MCP Server to production environments. ALL requirements must be met before go-live.

---

## 🔐 Security Baseline Requirements

### 1. Environment Configuration

#### 1.1 Spring Profiles

**Requirement**: Production deployments MUST use the `production` Spring profile.

```bash
# Environment variable
export SPRING_PROFILES_ACTIVE=production

# Or in docker-compose.yml
environment:
  SPRING_PROFILES_ACTIVE: production
```

**Verification**:
```bash
# Check application logs on startup
grep "The following profiles are active: production" logs/application.log
```

#### 1.2 Logging Configuration

**Requirement**: Production logging MUST be set to WARN or INFO level.

Create `src/main/resources/application-production.yml`:
```yaml
logging:
  level:
    root: WARN
    com.monicahq.mcp: INFO
    org.springframework.web: WARN
    org.springframework.websocket: INFO

  # Log to file for audit trail
  file:
    name: /var/log/monicahq-mcp/application.log
    max-size: 100MB
    max-history: 30  # Keep 30 days
```

**FORBIDDEN**:
- ❌ DEBUG logging in production
- ❌ MCP_DEBUG=true in production
- ❌ Logging sensitive data (tokens, passwords, PII)

**Verification**:
```bash
# Ensure no DEBUG logs in production
./gradlew bootJar
java -jar build/libs/monicahqmcp-0.1.0.jar --spring.profiles.active=production 2>&1 | grep -i "DEBUG" && echo "FAIL" || echo "PASS"
```

---

### 2. Secret Management

#### 2.1 API Token Storage

**Requirement**: API tokens MUST NEVER be:
- Committed to version control
- Hardcoded in source code
- Stored in plain environment variables in production

**Approved Methods**:

**Option A: Docker Secrets (Recommended for Docker Swarm/Kubernetes)**
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

Create the secret:
```bash
# Docker Swarm
echo "your-api-token-here" | docker secret create monica_api_token -

# Kubernetes
kubectl create secret generic monica-api-token \
  --from-literal=MONICA_API_TOKEN='your-api-token-here'
```

**Option B: Cloud Secret Manager**

AWS Secrets Manager:
```bash
# Store secret
aws secretsmanager create-secret \
  --name monicahq/api-token \
  --secret-string "your-api-token-here"

# Retrieve in application startup script
export MONICA_API_TOKEN=$(aws secretsmanager get-secret-value \
  --secret-id monicahq/api-token \
  --query SecretString \
  --output text)
```

Azure Key Vault:
```bash
# Store secret
az keyvault secret set \
  --vault-name monicahq-vault \
  --name api-token \
  --value "your-api-token-here"

# Retrieve
export MONICA_API_TOKEN=$(az keyvault secret show \
  --vault-name monicahq-vault \
  --name api-token \
  --query value -o tsv)
```

**Option C: HashiCorp Vault**
```bash
# Store secret
vault kv put secret/monicahq/api-token value="your-api-token-here"

# Retrieve
export MONICA_API_TOKEN=$(vault kv get -field=value secret/monicahq/api-token)
```

**Verification**:
- [ ] No tokens in `.env` files committed to git
- [ ] No tokens visible in `docker inspect`
- [ ] No tokens in application logs
- [ ] Tokens rotated every 90 days

#### 2.2 Token Rotation Policy

**Requirement**: API tokens MUST be rotated:
- Every 90 days (normal rotation)
- Immediately if compromised
- After employee offboarding

**Rotation Procedure**:
1. Generate new token in MonicaHQ
2. Update secret in secret manager
3. Restart application to pick up new token
4. Revoke old token in MonicaHQ
5. Verify application is functional
6. Document rotation in audit log

---

### 3. Network Security

#### 3.1 TLS/SSL Configuration

**Requirement**: ALL external communication MUST use TLS 1.2 or higher.

**Monica API Connection**:
```yaml
# application-production.yml
monica:
  api:
    url: https://your-monica-instance.com/api  # HTTPS required
```

**WebSocket Mode (if used)**:

Nginx reverse proxy configuration:
```nginx
server {
    listen 443 ssl http2;
    server_name mcp.example.com;

    # TLS configuration
    ssl_certificate /etc/nginx/ssl/cert.pem;
    ssl_certificate_key /etc/nginx/ssl/key.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;

    # Security headers
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    add_header X-Frame-Options "DENY" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;

    location / {
        proxy_pass http://monicahq-mcp:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

**Verification**:
```bash
# Test TLS configuration
openssl s_client -connect mcp.example.com:443 -tls1_2

# Verify TLS version
nmap --script ssl-enum-ciphers -p 443 mcp.example.com
```

#### 3.2 Firewall Rules

**Requirement**: Restrict network access to minimum required.

**Inbound Rules**:
| Port | Protocol | Source | Purpose |
|------|----------|--------|---------|
| 8080 | TCP | Internal network only | MCP WebSocket (if used) |
| 22 | TCP | Bastion host only | SSH (management) |

**Outbound Rules**:
| Port | Protocol | Destination | Purpose |
|------|----------|-------------|---------|
| 443 | TCP | Monica API URL | Monica API calls |
| 53 | UDP | DNS servers | Name resolution |

**Docker Network Isolation**:
```yaml
# docker-compose.yml
networks:
  backend:
    driver: bridge
    internal: true  # No external access
  frontend:
    driver: bridge

services:
  monicahq-mcp:
    networks:
      - backend
      - frontend
```

---

### 4. Container Security

#### 4.1 Docker Image Hardening

**Requirement**: Production Docker images MUST:
- Run as non-root user ✅ (Already implemented)
- Use minimal base images ✅ (eclipse-temurin:21-jre-alpine)
- Have no HIGH/CRITICAL CVEs
- Be scanned regularly

**Current Dockerfile** (already secure):
```dockerfile
# Non-root user (line 26)
RUN addgroup -S mcp && adduser -S mcp -G mcp
USER mcp
```

**Image Scanning**:
```bash
# Before deployment, scan for vulnerabilities
trivy image monicahq-mcp:latest --severity HIGH,CRITICAL

# Expected: No HIGH or CRITICAL findings
```

**Verification**:
- [ ] Image runs as non-root user
- [ ] No CVEs with severity ≥ HIGH
- [ ] Image size < 300MB
- [ ] Only necessary packages installed

#### 4.2 Resource Limits

**Requirement**: Container resource limits MUST be configured.

```yaml
# docker-compose.yml
services:
  monicahq-mcp:
    deploy:
      resources:
        limits:
          memory: 768m      # Maximum memory
          cpus: '1.0'       # Maximum CPU
        reservations:
          memory: 256m      # Guaranteed memory
          cpus: '0.25'      # Guaranteed CPU

    # Prevent privilege escalation
    security_opt:
      - no-new-privileges:true

    # Read-only root filesystem (where possible)
    read_only: true
    tmpfs:
      - /tmp
      - /app/logs
```

**Verification**:
```bash
# Check resource usage
docker stats monicahq-mcp

# Verify security options
docker inspect monicahq-mcp | grep -A10 "SecurityOpt"
```

---

### 5. Monitoring & Alerting

#### 5.1 Health Checks

**Requirement**: Health checks MUST be configured and monitored.

```yaml
# docker-compose.yml
services:
  monicahq-mcp:
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/actuator/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
```

**External Monitoring**:
```bash
# Uptime monitoring (e.g., Datadog, New Relic, Prometheus)
curl http://localhost:8080/actuator/health | jq '.status'
# Expected: "UP"
```

**Verification**:
- [ ] Health endpoint returns 200 OK
- [ ] Circuit breaker status monitored
- [ ] Memory/CPU usage tracked
- [ ] API response times monitored

#### 5.2 Security Monitoring

**Requirement**: Security events MUST be logged and monitored.

**Security Events to Monitor**:
1. Authentication failures
2. Rate limit violations
3. Circuit breaker activations
4. Unusual API error rates
5. Resource exhaustion

**Log Aggregation**:
```yaml
# docker-compose.yml
services:
  monicahq-mcp:
    logging:
      driver: "json-file"
      options:
        max-size: "100m"
        max-file: "10"
        labels: "service,environment"
```

**Alerting Rules** (example for Prometheus):
```yaml
groups:
  - name: monicahq-mcp-security
    rules:
      - alert: HighAuthenticationFailureRate
        expr: rate(authentication_failures[5m]) > 10
        annotations:
          summary: "High authentication failure rate detected"

      - alert: CircuitBreakerOpen
        expr: circuit_breaker_state == 1
        for: 5m
        annotations:
          summary: "Circuit breaker open for >5 minutes"
```

**Verification**:
- [ ] Logs centralized to SIEM/log aggregator
- [ ] Security alerts configured
- [ ] On-call rotation defined
- [ ] Incident response plan documented

---

### 6. Access Control

#### 6.1 SSH Access

**Requirement**: SSH access MUST be restricted and audited.

```bash
# /etc/ssh/sshd_config
PermitRootLogin no
PasswordAuthentication no
PubkeyAuthentication yes
AllowUsers deployuser

# Audit logging
Subsystem sftp /usr/lib/openssh/sftp-server -f AUTHPRIV -l INFO
```

**Verification**:
- [ ] SSH keys rotated every 180 days
- [ ] Failed SSH attempts monitored
- [ ] Bastion host or VPN required
- [ ] SSH sessions logged

#### 6.2 Docker Access

**Requirement**: Docker daemon access MUST be restricted.

```bash
# Only specific users can run docker commands
sudo groupadd docker
sudo usermod -aG docker deployuser

# Verify
docker run --rm hello-world  # Should work for deployuser only
```

**Verification**:
- [ ] Docker socket not exposed externally
- [ ] Only authorized users in docker group
- [ ] Docker daemon TLS-protected
- [ ] Image pulls from trusted registries only

---

### 7. Backup & Recovery

#### 7.1 Configuration Backup

**Requirement**: Configuration MUST be backed up regularly.

**What to Back Up**:
- Application configuration files
- Environment variables (encrypted)
- Secret references (not secrets themselves)
- Docker Compose files
- Nginx configurations

**Backup Procedure**:
```bash
#!/bin/bash
# backup-config.sh

BACKUP_DIR="/backups/monicahq-mcp/$(date +%Y%m%d)"
mkdir -p "$BACKUP_DIR"

# Back up configuration (no secrets!)
cp /app/config/application-production.yml "$BACKUP_DIR/"
cp /app/docker-compose.yml "$BACKUP_DIR/"
cp /etc/nginx/sites-available/monicahq-mcp "$BACKUP_DIR/"

# Encrypt and upload to S3
tar -czf - "$BACKUP_DIR" | \
  openssl enc -aes-256-cbc -salt -out "$BACKUP_DIR.tar.gz.enc" \
  -pass pass:$(cat /etc/backup-key)

aws s3 cp "$BACKUP_DIR.tar.gz.enc" s3://backups/monicahq-mcp/
```

**Verification**:
- [ ] Backups run daily
- [ ] Backups encrypted at rest
- [ ] Recovery tested quarterly
- [ ] Retention policy: 30 days

#### 7.2 Disaster Recovery Plan

**Requirement**: DR plan MUST be documented and tested.

**Recovery Time Objective (RTO)**: 4 hours
**Recovery Point Objective (RPO)**: 24 hours

**DR Procedure**:
1. Provision new infrastructure
2. Restore configuration from backup
3. Retrieve secrets from secret manager
4. Deploy application
5. Verify health checks
6. Update DNS (if necessary)
7. Monitor for 24 hours

**Verification**:
- [ ] DR plan documented
- [ ] DR drill conducted annually
- [ ] Recovery time < RTO
- [ ] Data loss < RPO

---

## 📋 Production Deployment Checklist

### Pre-Deployment (T-1 week)

- [ ] Security audit completed (see SECURITY-HARDENING-CHECKLIST.md)
- [ ] All P0/P1 security issues resolved
- [ ] Dependency vulnerabilities scanned
- [ ] Docker image scanned for CVEs
- [ ] TLS certificates obtained and configured
- [ ] Secret manager configured
- [ ] Monitoring/alerting configured
- [ ] Backup procedures tested
- [ ] DR plan documented and reviewed

### Deployment Day (T-0)

- [ ] Deploy to staging environment first
- [ ] Run smoke tests on staging
- [ ] Update secrets in production secret manager
- [ ] Deploy to production with blue/green deployment
- [ ] Verify health checks passing
- [ ] Verify circuit breaker functional
- [ ] Check logs for errors
- [ ] Monitor resource usage
- [ ] Test end-to-end workflows
- [ ] Notify stakeholders of go-live

### Post-Deployment (T+1 day)

- [ ] Review logs for security events
- [ ] Verify monitoring alerts working
- [ ] Check resource utilization
- [ ] Conduct smoke tests
- [ ] Update documentation
- [ ] Schedule post-mortem meeting

---

## 🚨 Security Incident Response

### Incident Classification

**P0 - Critical** (Response: Immediate):
- API token compromised
- Unauthorized access detected
- Data breach suspected

**P1 - High** (Response: <4 hours):
- Circuit breaker failure
- Authentication system down
- Rate limit bypass detected

**P2 - Medium** (Response: <24 hours):
- Unusual error rate
- Resource exhaustion
- Dependency vulnerability

### Incident Response Procedure

1. **Detect**: Alert fires or issue reported
2. **Assess**: Classify severity, identify scope
3. **Contain**:
   - Rotate API tokens if compromised
   - Enable rate limiting
   - Block suspicious IPs
4. **Investigate**: Review logs, identify root cause
5. **Remediate**: Apply fix, deploy update
6. **Recover**: Restore normal operations
7. **Document**: Post-incident report

### Emergency Contacts

- **Security Team**: security@example.com
- **On-Call Engineer**: Pagerduty/Opsgenie
- **DevOps Lead**: devops@example.com

---

## 📊 Security Compliance

### Required Audits

| Audit Type | Frequency | Owner |
|------------|-----------|-------|
| Vulnerability Scan | Weekly | DevOps |
| Penetration Test | Quarterly | Security Team |
| Access Review | Monthly | Security Team |
| Dependency Audit | Weekly | Development |
| Configuration Review | Monthly | SRE |

### Compliance Standards

Document alignment with relevant standards:

- **GDPR** (if handling EU data): Data protection measures
- **SOC 2**: Security controls documentation
- **ISO 27001**: Information security management
- **PCI DSS** (if applicable): Payment data protection

---

## 🔄 Ongoing Maintenance

### Regular Security Tasks

**Weekly**:
- [ ] Review security logs
- [ ] Check for dependency updates
- [ ] Scan Docker images
- [ ] Review failed authentication attempts

**Monthly**:
- [ ] Rotate service accounts
- [ ] Review firewall rules
- [ ] Update security documentation
- [ ] Conduct access review

**Quarterly**:
- [ ] Rotate API tokens
- [ ] Penetration testing
- [ ] DR drill
- [ ] Security training

**Annually**:
- [ ] Full security audit
- [ ] Update incident response plan
- [ ] Review compliance requirements
- [ ] Renew TLS certificates

---

## 📖 References

- [SECURITY-HARDENING-CHECKLIST.md](./SECURITY-HARDENING-CHECKLIST.md)
- [TEST-COVERAGE-VERIFICATION.md](./TEST-COVERAGE-VERIFICATION.md)
- [docs/API-LIMITATIONS.md](./API-LIMITATIONS.md)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [CIS Docker Benchmark](https://www.cisecurity.org/benchmark/docker)

---

**Document Owner**: Security Team / DevOps
**Last Updated**: 2026-02-06
**Next Review**: Before production deployment
**Approval Required**: Security Team Lead, DevOps Lead
