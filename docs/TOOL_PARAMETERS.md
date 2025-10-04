# MonicaHQ MCP Server - Tool Parameters Reference

This document provides comprehensive documentation for all tool parameters in the MonicaHQ MCP Server.

## Table of Contents

- [Activity Tools](#activity-tools)
- [Contact Tools](#contact-tools)
- [Note Tools](#note-tools)
- [Task Tools](#task-tools)
- [Call Tools](#call-tools)
- [Reminder Tools](#reminder-tools)
- [Parameter Format Guidelines](#parameter-format-guidelines)
- [Common Patterns](#common-patterns)
- [Error Handling](#error-handling)

## Activity Tools

### `monicahq:activity_create`

Creates a new activity record in MonicaHQ.

**Required Parameters:**
- `summary` (string): Brief description of the activity
- `attendees` (array): List of activity attendees

**Optional Parameters:**
- `description` (string): Detailed description of the activity
- `happenedAt` (string): When the activity occurred (ISO 8601 format)
- `duration` (number): Duration in minutes
- `activityTypeId` (number): ID of the activity type

**Attendees Format:**
The `attendees` parameter supports two formats:

1. **String Array** (names only):
   ```json
   "attendees": ["John Doe", "Jane Smith"]
   ```

2. **Object Array** (with contact IDs):
   ```json
   "attendees": [
     {"contactId": 123},
     {"contactId": 456}
   ]
   ```

**Example:**
```json
{
  "summary": "Weekly team meeting",
  "description": "Discussed project progress and next steps",
  "attendees": ["John Doe", "Jane Smith"],
  "happenedAt": "2024-01-15T10:30:00Z",
  "duration": 60
}
```

### `monicahq:activity_get`

Retrieves an activity by ID.

**Required Parameters:**
- `id` (number): Activity ID

### `monicahq:activity_update`

Updates an existing activity.

**Required Parameters:**
- `id` (number): Activity ID

**Optional Parameters:**
- Same as `activity_create` (all fields except attendees can be updated)

### `monicahq:activity_delete`

Deletes an activity.

**Required Parameters:**
- `id` (number): Activity ID

### `monicahq:activity_list`

Lists activities with optional filtering.

**Optional Parameters:**
- `page` (number): Page number (default: 1)
- `limit` (number): Items per page (max: 100, default: 10)
- `contactId` (number): Filter by contact ID

## Contact Tools

### `monicahq:contact_create`

Creates a new contact in MonicaHQ.

**Required Parameters:**
- `firstName` (string): Contact's first name

**Optional Parameters:**
- `lastName` (string): Contact's last name
- `nickname` (string): Contact's nickname
- `gender` (string): Contact's gender
- `email` (string): Contact's email address
- `phone` (string): Contact's phone number
- `birthdate` (string): Birth date (ISO 8601 format)

**Example:**
```json
{
  "firstName": "John",
  "lastName": "Doe",
  "email": "john.doe@example.com",
  "phone": "+1-555-123-4567"
}
```

### `monicahq:contact_get`

Retrieves a contact by ID.

**Required Parameters:**
- `id` (number): Contact ID

### `monicahq:contact_update`

Updates an existing contact.

**Required Parameters:**
- `id` (number): Contact ID

**Optional Parameters:**
- Same as `contact_create`

### `monicahq:contact_delete`

Deletes a contact.

**Required Parameters:**
- `id` (number): Contact ID

### `monicahq:contact_list`

Lists contacts with optional filtering and search.

**Optional Parameters:**
- `page` (number): Page number (default: 1)
- `limit` (number): Items per page (max: 100, default: 10)
- `query` (string): Search query for names

## Note Tools

### `monicahq:note_create`

Creates a note associated with a contact.

**Required Parameters:**
- `body` (string): Note content
- `contactId` (number): Associated contact ID

**Optional Parameters:**
- `title` (string): Note title

### `monicahq:note_get`

Retrieves a note by ID.

**Required Parameters:**
- `id` (number): Note ID

### `monicahq:note_update`

Updates an existing note.

**Required Parameters:**
- `id` (number): Note ID

**Optional Parameters:**
- `body` (string): Note content
- `title` (string): Note title

### `monicahq:note_delete`

Deletes a note.

**Required Parameters:**
- `id` (number): Note ID

### `monicahq:note_list`

Lists notes with optional filtering.

**Optional Parameters:**
- `page` (number): Page number
- `limit` (number): Items per page
- `contactId` (number): Filter by contact ID

## Task Tools

### `monicahq:task_create`

Creates a task associated with a contact.

**Required Parameters:**
- `title` (string): Task title
- `contactId` (number): Associated contact ID

**Optional Parameters:**
- `description` (string): Task description
- `completed` (boolean): Task completion status
- `completedAt` (string): Completion date (ISO 8601)

### `monicahq:task_get`

Retrieves a task by ID.

**Required Parameters:**
- `id` (number): Task ID

### `monicahq:task_update`

Updates an existing task.

**Required Parameters:**
- `id` (number): Task ID

**Optional Parameters:**
- Same as `task_create`

### `monicahq:task_delete`

Deletes a task.

**Required Parameters:**
- `id` (number): Task ID

### `monicahq:task_list`

Lists tasks with optional filtering.

**Optional Parameters:**
- `page` (number): Page number
- `limit` (number): Items per page
- `contactId` (number): Filter by contact ID

## Call Tools

### `monicahq:call_create`

Records a phone call with a contact.

**Required Parameters:**
- `contactId` (number): Contact who was called

**Optional Parameters:**
- `duration` (number): Call duration in minutes
- `description` (string): Call notes
- `happenedAt` (string): When the call occurred (ISO 8601)

### `monicahq:call_get`

Retrieves a call record by ID.

**Required Parameters:**
- `id` (number): Call ID

### `monicahq:call_update`

Updates an existing call record.

**Required Parameters:**
- `id` (number): Call ID

**Optional Parameters:**
- Same as `call_create`

### `monicahq:call_delete`

Deletes a call record.

**Required Parameters:**
- `id` (number): Call ID

### `monicahq:call_list`

Lists call records with optional filtering.

**Optional Parameters:**
- `page` (number): Page number
- `limit` (number): Items per page
- `contactId` (number): Filter by contact ID

## Reminder Tools

### `monicahq:reminder_create`

Creates a reminder for a contact.

**Required Parameters:**
- `title` (string): Reminder title
- `contactId` (number): Associated contact ID
- `triggerDate` (string): When to trigger the reminder (ISO 8601)

**Optional Parameters:**
- `description` (string): Reminder description
- `frequency` (string): Recurrence frequency
- `numberOfOccurrences` (number): How many times to repeat

### `monicahq:reminder_get`

Retrieves a reminder by ID.

**Required Parameters:**
- `id` (number): Reminder ID

### `monicahq:reminder_update`

Updates an existing reminder.

**Required Parameters:**
- `id` (number): Reminder ID

**Optional Parameters:**
- Same as `reminder_create`

### `monicahq:reminder_delete`

Deletes a reminder.

**Required Parameters:**
- `id` (number): Reminder ID

### `monicahq:reminder_list`

Lists reminders with optional filtering.

**Optional Parameters:**
- `page` (number): Page number
- `limit` (number): Items per page
- `contactId` (number): Filter by contact ID

## Parameter Format Guidelines

### Date and Time

All date/time parameters should use ISO 8601 format:
- **Date only:** `2024-01-15`
- **Date and time:** `2024-01-15T10:30:00Z`
- **With timezone:** `2024-01-15T10:30:00-05:00`

### Numbers

- **Integers:** Contact IDs, durations, limits, page numbers
- **Range:** Page numbers start from 1, limit max is 100
- **Validation:** IDs must be positive integers

### Strings

- **Non-empty:** Required string fields cannot be empty
- **Encoding:** UTF-8 encoding for international characters
- **Length:** Generally limited to reasonable lengths (names: 255 chars, descriptions: 2000 chars)

### Arrays

- **Attendees:** Can be strings or objects as documented above
- **Empty arrays:** Not allowed for required array parameters

## Common Patterns

### Pagination

Most list operations support pagination:
```json
{
  "page": 1,
  "limit": 25
}
```

### Filtering by Contact

Many operations can be filtered by contact:
```json
{
  "contactId": 123,
  "page": 1,
  "limit": 10
}
```

### Search Operations

Text search is available on some endpoints:
```json
{
  "query": "search terms",
  "limit": 20
}
```

## Error Handling

### Common Error Scenarios

1. **Missing Required Parameters**
   ```json
   {
     "error": {
       "code": -32602,
       "message": "Invalid params",
       "data": "Required parameter 'summary' is missing"
     }
   }
   ```

2. **Invalid Parameter Types**
   ```json
   {
     "error": {
       "code": -32602,
       "message": "Invalid params", 
       "data": "Parameter 'contactId' must be a number"
     }
   }
   ```

3. **Resource Not Found**
   ```json
   {
     "error": {
       "code": -32000,
       "message": "Resource not found",
       "data": "Contact with ID 999 not found"
     }
   }
   ```

4. **Authentication Issues**
   ```json
   {
     "error": {
       "code": -32000,
       "message": "Authentication failed",
       "data": "Invalid or expired API token"
     }
   }
   ```

### Parameter Validation

The server performs comprehensive parameter validation:

- **Type checking:** Ensures parameters are the correct type
- **Range validation:** Numbers within acceptable ranges
- **Format validation:** Dates, emails, phone numbers
- **Required field checking:** All required parameters present
- **Cross-parameter validation:** Related parameters are consistent

### Debug Mode

Enable debug mode for detailed parameter validation errors:

```bash
export MCP_DEBUG=true
```

In debug mode, error messages include:
- Detailed parameter analysis
- Suggestions for fixing issues
- Examples of correct parameter formats
- Full validation context

## Best Practices

1. **Always include required parameters**
2. **Use ISO 8601 for all dates**
3. **Validate data client-side when possible**
4. **Handle attendees format consistently**
5. **Use pagination for large result sets**
6. **Enable debug mode during development**
7. **Check error responses for validation details**

## Examples Repository

For complete working examples of all tool calls, see:
- `docs/examples/activity-examples.json`
- `docs/examples/contact-examples.json`

## Support

For issues with parameter validation:
1. Enable debug mode (`MCP_DEBUG=true`)
2. Check the error message details
3. Verify parameter types and formats
4. Consult this documentation
5. Review the example files

---

*This documentation is automatically validated against the actual tool implementations to ensure accuracy.*