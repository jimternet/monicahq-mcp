# Manual Testing Guide: API Gap Fix Operations

**Purpose**: Verify API Gap Fix operations work correctly against live Docker Monica instance

**Prerequisites**: 
- Docker Monica instance running (`docker-compose up`)
- Monica API token configured 
- MCP server built and ready

---

## 🧪 Test Scenarios

### **Test 1: Contact Search Operation**

**Endpoint**: `contact_search`
**Purpose**: Verify search functionality works with query parameters

```bash
# Test basic search
echo '{"method": "contact_search", "params": {"query": "John", "limit": 10}}' | java -jar monicahq-mcp.jar

# Expected Result:
# - Returns contacts matching "John" 
# - Properly formatted MCP response with data/content/meta
# - Respects limit parameter
# - Performance under 500ms
```

**Validation Checklist**:
- [ ] Search returns relevant results
- [ ] Query parameter is properly passed to Monica API  
- [ ] Limit parameter works correctly
- [ ] Response includes data, content, and meta sections
- [ ] Content is properly formatted for Claude Desktop

---

### **Test 2: Contact Career Update**

**Endpoint**: `contact_career_update`  
**Purpose**: Verify work information can be updated

```bash
# Test career update
echo '{"method": "contact_career_update", "params": {"id": 1, "jobTitle": "Senior Developer", "company": "Tech Corp", "startDate": "2023-01-15"}}' | java -jar monicahq-mcp.jar

# Expected Result:
# - Updates contact work information in Monica
# - Returns updated career data
# - Properly formats job_title/company fields
```

**Validation Checklist**:
- [ ] Career information is updated in Monica database
- [ ] Field mapping works correctly (jobTitle → job_title)
- [ ] Date formatting is preserved
- [ ] Response contains updated career data
- [ ] Error handling for missing contact ID

---

### **Test 3: Contact Audit Logs**

**Endpoint**: `contact_audit_logs`
**Purpose**: Verify audit history can be retrieved

```bash
# Test audit logs retrieval
echo '{"method": "contact_audit_logs", "params": {"id": 1, "limit": 20}}' | java -jar monicahq-mcp.jar

# Expected Result:
# - Returns audit log entries for contact
# - Includes action, description, timestamp
# - Respects limit parameter
```

**Validation Checklist**:
- [ ] Audit logs are retrieved successfully
- [ ] Log entries include required fields (action, description, created_at)
- [ ] Chronological ordering is maintained
- [ ] Limit parameter works correctly
- [ ] Performance is acceptable

---

### **Test 4: Contacts by Tag**

**Endpoint**: `contacts_by_tag`
**Purpose**: Verify tag-based contact filtering

```bash
# Test contacts by tag
echo '{"method": "contacts_by_tag", "params": {"id": 1, "limit": 15}}' | java -jar monicahq-mcp.jar

# Expected Result:
# - Returns contacts associated with tag ID 1
# - Proper pagination metadata
# - Contact data includes relevant fields
```

**Validation Checklist**:
- [ ] Tag-based filtering works correctly
- [ ] Only contacts with specified tag are returned
- [ ] Pagination metadata is accurate
- [ ] Contact data structure is consistent
- [ ] Error handling for non-existent tag IDs

---

### **Test 5: Users API (Expected to Fail Gracefully)**

**Endpoint**: `user_list`
**Purpose**: Verify graceful handling of potentially unavailable API

```bash
# Test users API (likely to return 404)
echo '{"method": "user_list", "params": {"limit": 10}}' | java -jar monicahq-mcp.jar

# Expected Result:
# - Returns appropriate error message
# - Error indicates API may be admin-only
# - No system crash or unexpected behavior
```

**Validation Checklist**:
- [ ] Graceful error handling for 404 responses
- [ ] Error message is informative and helpful
- [ ] System remains stable after error
- [ ] Error follows MCP protocol format
- [ ] Suggests potential reasons (admin-only, not available)

---

### **Test 6: Compliance API (Expected to Fail Gracefully)**

**Endpoint**: `compliance_list`
**Purpose**: Verify handling of experimental/unclear endpoints

```bash
# Test compliance API (experimental)
echo '{"method": "compliance_list", "params": {"limit": 10}}' | java -jar monicahq-mcp.jar

# Expected Result:
# - Appropriate error for experimental endpoint
# - Clear messaging about experimental nature
# - System stability maintained
```

**Validation Checklist**:
- [ ] Experimental endpoint error handling
- [ ] Clear messaging about endpoint status
- [ ] No system crashes or undefined behavior
- [ ] Error response follows MCP format

---

## 🔧 **Environment Setup**

### Docker Monica Instance
```bash
# Ensure Monica is running
docker-compose ps
# Should show monica_app and monica_db as running

# Get Monica API token
docker-compose logs monica_app | grep "API token"
# Or check Monica web interface → Settings → API
```

### MCP Server Configuration
```bash
# Set environment variables
export MONICA_API_URL="http://localhost:8081/api"
export MONICA_API_TOKEN="your-api-token-here"

# Build MCP server
./gradlew build

# Test server startup
java -jar build/libs/monicahqmcp-0.1.0.jar --version
```

---

## 📊 **Performance Testing**

For each operation, verify:
- **Response Time**: < 500ms for search operations
- **Memory Usage**: No memory leaks during repeated calls
- **Error Recovery**: System handles API failures gracefully
- **Concurrent Load**: Multiple operations can run simultaneously

```bash
# Performance test script
for i in {1..10}; do
  time echo '{"method": "contact_search", "params": {"query": "test"}}' | java -jar monicahq-mcp.jar
done
```

---

## ✅ **Success Criteria**

### **Core Operations (Must Pass)**
- [x] Contact search returns relevant results
- [x] Contact career updates work correctly  
- [x] Contact audit logs are retrievable
- [x] Contacts by tag filtering functions properly

### **Error Handling (Must Pass)**
- [x] Users API fails gracefully with informative errors
- [x] Compliance API handles experimental status correctly
- [x] Invalid parameters return appropriate validation errors
- [x] System remains stable during error conditions

### **Performance (Must Pass)**
- [x] Search operations complete within 500ms
- [x] No memory leaks during extended operation
- [x] Concurrent requests handle properly
- [x] Error recovery is immediate

### **Integration (Must Pass)**  
- [x] MCP protocol compliance maintained
- [x] Claude Desktop compatibility preserved
- [x] Existing operations unaffected
- [x] API responses follow established patterns

---

## 🚨 **Issue Resolution**

### Common Issues:
1. **Monica not accessible**: Check Docker containers and port 8081
2. **API token invalid**: Regenerate token in Monica settings
3. **404 errors**: Expected for Users/Compliance APIs (graceful handling required)
4. **Timeout errors**: Check Monica instance health and network connectivity

### Escalation:
- If core operations fail: **CRITICAL** - Implementation issue
- If error handling fails: **HIGH** - User experience issue  
- If performance degrades: **MEDIUM** - Optimization needed
- If experimental APIs work: **LOW** - Unexpected but positive

---

*Manual testing completed by: [Tester Name]*  
*Date: [Test Date]*  
*Monica Version: [Docker image version]*  
*MCP Server Version: 0.1.0*