# MonicaHQ MCP Server - Manual Testing Checklist

This checklist ensures comprehensive manual testing of the MonicaHQ MCP Server with Claude Desktop integration.

## Pre-Testing Setup

### Environment Preparation

- [ ] **Java 17+ installed and accessible**
  ```bash
  java -version
  ```

- [ ] **MonicaHQ instance accessible**
  - [ ] API URL configured: `$MONICA_API_URL`
  - [ ] API token configured: `$MONICA_API_TOKEN`
  - [ ] API connectivity test passes:
    ```bash
    curl -H "Authorization: Bearer $MONICA_API_TOKEN" "$MONICA_API_URL/api/me"
    ```

- [ ] **MCP Server built successfully**
  ```bash
  ./gradlew build
  ls build/libs/monica-hq-mcp-*.jar
  ```

- [ ] **Claude Desktop installed and updated**
  - [ ] Version: Latest from claude.ai
  - [ ] Configuration directory exists: `~/Library/Application Support/Claude/`

### Configuration Validation

- [ ] **Claude Desktop configuration valid**
  ```bash
  python3 -m json.tool "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
  ```

- [ ] **MCP server starts independently**
  ```bash
  echo '{"jsonrpc":"2.0","id":"test","method":"initialize","params":{"protocolVersion":"2024-11-05"}}' | java -jar build/libs/monica-hq-mcp-*.jar
  ```

## Core MCP Protocol Testing

### Connection and Initialization

- [ ] **MCP server initializes successfully**
  - [ ] Claude Desktop shows 🔌 icon
  - [ ] No error messages in Claude Desktop logs
  - [ ] Initialization completes within 10 seconds

- [ ] **Protocol version negotiation**
  - [ ] Server accepts current protocol version: `2024-11-05`
  - [ ] Server handles missing protocol version gracefully
  - [ ] Server rejects incompatible protocol versions

- [ ] **Tool discovery works**
  - [ ] tools/list returns expected number of tools (122+)
  - [ ] All critical tools present:
    - [ ] `monicahq:activity_create`
    - [ ] `monicahq:contact_list`
    - [ ] `monicahq:contact_create`
    - [ ] `monicahq:note_create`
    - [ ] `monicahq:task_create`

### Basic Communication

- [ ] **JSON-RPC 2.0 compliance**
  - [ ] Valid requests receive proper responses
  - [ ] Invalid JSON returns parse error
  - [ ] Unknown methods return method not found error
  - [ ] Response IDs match request IDs

- [ ] **STDIO protocol**
  - [ ] Single messages processed correctly
  - [ ] Multiple messages handled in sequence
  - [ ] Large responses don't cause buffer issues
  - [ ] No stdout contamination (all logs to stderr)

## Tool Execution Testing

### Contact Management

**Contact Creation:**
- [ ] **Create contact with minimal data**
  ```
  Create a contact in MonicaHQ:
  Name: Test User
  ```
  - [ ] Contact created successfully
  - [ ] Valid contact ID returned
  - [ ] Response includes complete contact data

- [ ] **Create contact with full data**
  ```
  Create a contact in MonicaHQ:
  Name: Jane Doe
  Email: jane@example.com
  Phone: +1-555-123-4567
  ```
  - [ ] All fields saved correctly
  - [ ] Email and phone validation works
  - [ ] Response formatting is consistent

**Contact Listing:**
- [ ] **List all contacts**
  ```
  Show me my contacts from MonicaHQ
  ```
  - [ ] Returns paginated results
  - [ ] Includes contact details
  - [ ] Proper formatting for Claude display

- [ ] **Search contacts**
  ```
  Search for contacts named "Jane" in MonicaHQ
  ```
  - [ ] Search functionality works
  - [ ] Relevant results returned
  - [ ] Empty search handles gracefully

- [ ] **Paginated listing**
  ```
  Show me contacts from MonicaHQ, page 2, limit 5
  ```
  - [ ] Pagination parameters work
  - [ ] Consistent result formatting
  - [ ] Page boundaries respected

### Activity Management

**Activity Creation:**
- [ ] **Create activity with string attendees**
  ```
  Log an activity in MonicaHQ:
  Summary: Team meeting
  Attendees: John Doe, Jane Smith
  ```
  - [ ] Attendees processed correctly
  - [ ] Activity saved with proper attendee format
  - [ ] Response includes activity details

- [ ] **Create activity with contact ID attendees**
  ```
  Create an activity with contact ID 123 as attendee
  Summary: Client call
  ```
  - [ ] Contact ID format handled correctly
  - [ ] Proper API mapping occurs
  - [ ] Validation passes for existing contacts

- [ ] **Create activity with full details**
  ```
  Log an activity:
  Summary: Project review
  Description: Quarterly project status review
  Attendees: John Doe
  Date: 2024-01-15T14:00:00Z
  Duration: 90 minutes
  ```
  - [ ] All fields saved correctly
  - [ ] Date parsing works properly
  - [ ] Duration validation correct

### Note and Task Management

**Note Creation:**
- [ ] **Create note for contact**
  ```
  Add a note to contact John Doe in MonicaHQ:
  "Prefers morning meetings"
  ```
  - [ ] Note associated with correct contact
  - [ ] Content saved accurately
  - [ ] Timestamp added automatically

**Task Management:**
- [ ] **Create task**
  ```
  Create a task in MonicaHQ:
  Title: Follow up on proposal
  Contact: Jane Smith
  Description: Send updated timeline
  ```
  - [ ] Task created with all details
  - [ ] Contact association works
  - [ ] Task appears in contact's task list

- [ ] **List tasks**
  ```
  Show me tasks from MonicaHQ for John Doe
  ```
  - [ ] Tasks filtered by contact correctly
  - [ ] Completion status visible
  - [ ] Proper formatting applied

### Call and Reminder Management

**Call Logging:**
- [ ] **Log phone call**
  ```
  Record a call in MonicaHQ:
  Contact: Jane Smith
  Duration: 15 minutes
  Notes: Discussed project timeline
  ```
  - [ ] Call logged correctly
  - [ ] Duration and notes saved
  - [ ] Timestamp recorded

**Reminder Creation:**
- [ ] **Create reminder**
  ```
  Set a reminder in MonicaHQ:
  Contact: John Doe
  Title: Birthday reminder
  Date: 2024-06-15T09:00:00Z
  ```
  - [ ] Reminder created successfully
  - [ ] Date validation works
  - [ ] Contact association correct

## Error Handling Testing

### Parameter Validation

- [ ] **Missing required parameters**
  ```
  Create an activity in MonicaHQ without summary
  ```
  - [ ] Proper error message returned
  - [ ] Validation details provided
  - [ ] No server crash or corruption

- [ ] **Invalid parameter types**
  ```
  Create contact with contactId as string instead of number
  ```
  - [ ] Type validation works
  - [ ] Clear error messages
  - [ ] Helpful suggestions provided

- [ ] **Invalid attendees formats**
  - [ ] Empty attendees array rejected
  - [ ] Invalid object format handled
  - [ ] Mixed valid/invalid attendees processed correctly

### API Error Handling

- [ ] **Network connectivity issues**
  - [ ] Temporary network issues handled gracefully
  - [ ] Timeout errors properly reported
  - [ ] Recovery after network restoration

- [ ] **Authentication errors**
  - [ ] Invalid API token detected
  - [ ] Expired token handled properly
  - [ ] Permission errors reported clearly

- [ ] **Resource not found**
  - [ ] Invalid contact IDs handled
  - [ ] Missing resources reported accurately
  - [ ] No crashes on invalid references

## Debug Mode Testing

### Debug Mode Activation

- [ ] **Enable debug mode**
  - [ ] Set `MCP_DEBUG=true` in configuration
  - [ ] Restart Claude Desktop
  - [ ] Verify debug capabilities in initialize response

- [ ] **Debug logging verification**
  - [ ] Detailed logs appear in stderr
  - [ ] Request/response logging works
  - [ ] Parameter validation details shown
  - [ ] Performance metrics included

### Debug Information Quality

- [ ] **Error diagnostics**
  - [ ] Stack traces provided for failures
  - [ ] Context information included
  - [ ] Troubleshooting hints present

- [ ] **Performance monitoring**
  - [ ] Request timing information
  - [ ] Memory usage tracking
  - [ ] Operation statistics collection

## Performance Testing

### Response Time Validation

- [ ] **Tool discovery performance**
  - [ ] tools/list completes within 5 seconds
  - [ ] Consistent response times across requests
  - [ ] No memory leaks over time

- [ ] **Tool execution performance**
  - [ ] Simple operations (get, list) < 3 seconds
  - [ ] Complex operations (create, update) < 10 seconds
  - [ ] Large result sets handled efficiently

### Resource Usage

- [ ] **Memory consumption**
  - [ ] Server starts with < 128MB
  - [ ] Memory usage stable over time
  - [ ] No obvious memory leaks

- [ ] **CPU utilization**
  - [ ] Low CPU usage during idle
  - [ ] Reasonable CPU usage during operations
  - [ ] No CPU spikes or hangs

## Integration Testing with Claude Desktop

### User Experience Testing

- [ ] **Natural language interaction**
  ```
  Can you show me my contacts from MonicaHQ?
  ```
  - [ ] Claude understands the request
  - [ ] Appropriate tool selected automatically
  - [ ] Results presented clearly

- [ ] **Complex multi-step operations**
  ```
  Create a contact named Alice Johnson with email alice@example.com, 
  then create a task for her to "Review contract proposal"
  ```
  - [ ] Multiple tool calls executed correctly
  - [ ] Data consistency maintained
  - [ ] Proper error handling if any step fails

- [ ] **Context awareness**
  ```
  Create an activity for the meeting we discussed earlier
  ```
  - [ ] Claude maintains conversation context
  - [ ] Reasonable defaults applied
  - [ ] Clarification requested when needed

### Error Recovery

- [ ] **Graceful error handling**
  - [ ] Clear error messages displayed
  - [ ] Suggestions for resolution provided
  - [ ] No Claude Desktop crashes

- [ ] **Connection recovery**
  - [ ] Automatic reconnection after brief outages
  - [ ] Clear status indicators
  - [ ] Data integrity maintained

## Edge Cases and Stress Testing

### Data Validation

- [ ] **Special characters in data**
  - [ ] Unicode characters handled correctly
  - [ ] Emoji in contact names and notes
  - [ ] Special symbols in descriptions

- [ ] **Large data sets**
  - [ ] Large contact lists (>100 contacts)
  - [ ] Long descriptions and notes
  - [ ] Multiple attendees (>10 people)

- [ ] **Boundary conditions**
  - [ ] Empty strings handled appropriately
  - [ ] Maximum length limits respected
  - [ ] Null and undefined values processed

### Concurrent Operations

- [ ] **Multiple simultaneous requests**
  - [ ] Server handles concurrent tool calls
  - [ ] No data corruption occurs
  - [ ] Responses remain consistent

- [ ] **Long-running operations**
  - [ ] Operations that take >30 seconds
  - [ ] Timeout handling works correctly
  - [ ] User receives appropriate feedback

## Security Testing

### Data Protection

- [ ] **Sensitive data handling**
  - [ ] API tokens not logged or exposed
  - [ ] Personal data properly transmitted
  - [ ] No credentials in error messages

- [ ] **Input validation**
  - [ ] SQL injection attempts blocked
  - [ ] Script injection prevented
  - [ ] Malformed requests rejected safely

## Final Validation

### Comprehensive Workflow

- [ ] **Complete CRM workflow**
  1. Create a new contact
  2. Add notes about the contact
  3. Create tasks related to the contact
  4. Log activities with the contact
  5. Set reminders for follow-up
  6. List and review all data

- [ ] **Data consistency verification**
  - [ ] All created data appears correctly in MonicaHQ web interface
  - [ ] Relationships between entities maintained
  - [ ] No data corruption or loss

### Production Readiness

- [ ] **Stability over time**
  - [ ] Server runs for >1 hour without issues
  - [ ] Memory usage remains stable
  - [ ] No degradation in performance

- [ ] **Error recovery testing**
  - [ ] Server recovers from API outages
  - [ ] Handles MonicaHQ maintenance windows
  - [ ] Graceful degradation when appropriate

## Sign-off Checklist

### Technical Validation

- [ ] All core functionalities tested and working
- [ ] Error handling comprehensive and user-friendly
- [ ] Performance meets acceptable standards
- [ ] Debug mode provides useful information
- [ ] Security considerations addressed

### User Experience Validation

- [ ] Natural language interaction works smoothly
- [ ] Error messages are clear and helpful
- [ ] Complex workflows complete successfully
- [ ] Claude Desktop integration feels native

### Production Readiness

- [ ] Configuration documentation complete
- [ ] Troubleshooting guides available
- [ ] Monitoring and logging adequate
- [ ] Update procedures documented

## Testing Notes

**Date:** _______________  
**Tester:** _______________  
**Environment:** _______________  
**MonicaHQ Version:** _______________  
**Claude Desktop Version:** _______________  

### Issues Found

| Issue | Severity | Description | Status |
|-------|----------|-------------|--------|
|       |          |             |        |
|       |          |             |        |
|       |          |             |        |

### Recommendations

- [ ] **Ready for production deployment**
- [ ] **Requires minor fixes before deployment**
- [ ] **Requires major fixes before deployment**
- [ ] **Not ready for deployment**

**Additional Notes:**
_________________________________
_________________________________
_________________________________

---

*This checklist should be completed by a qualified tester before production deployment. All critical items must pass for production readiness.*