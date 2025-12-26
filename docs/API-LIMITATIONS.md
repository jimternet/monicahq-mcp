# MonicaHQ API Limitations and Workarounds

This document details known limitations of the MonicaHQ API and the workarounds implemented in the MCP server to provide seamless functionality.

## Table of Contents

- [Overview](#overview)
- [Unsupported Features](#unsupported-features)
  - [Photo Upload](#photo-upload)
  - [Document Upload](#document-upload)
- [API Endpoint Workarounds](#api-endpoint-workarounds)
  - [Root-Level POST Pattern](#root-level-post-pattern)
  - [Conversation Messages Extraction](#conversation-messages-extraction)
- [Affected Services](#affected-services)
- [Troubleshooting](#troubleshooting)

## Overview

The MonicaHQ API has several endpoints that return HTTP 405 (Method Not Allowed) errors despite being documented. This MCP server implements workarounds for these limitations to provide a fully functional interface.

**Key Points:**
- 6 nested POST endpoints have been replaced with root-level POST endpoints
- 1 nested GET endpoint has been replaced with data extraction from parent resource
- Photo and document uploads are NOT supported (no workaround exists)

## Unsupported Features

### Photo Upload

**Status:** NOT SUPPORTED

The MonicaHQ API does not provide a working endpoint for uploading photos via the API.

**What This Means:**
- `photo_create` and `photo_upload` operations are NOT available
- Photos cannot be added to contacts programmatically
- This is a limitation of the MonicaHQ API itself, not this MCP server

**Workaround:** None available. Photos must be uploaded manually through the MonicaHQ web interface.

**Technical Details:**
```
Endpoint: POST /photos (or any variant)
Status: Returns HTTP 405 Method Not Allowed
Alternative: None - requires manual upload via web UI
```

### Document Upload

**Status:** NOT SUPPORTED

The MonicaHQ API does not provide a working endpoint for uploading documents via the API.

**What This Means:**
- `document_create` and `document_upload` operations are NOT available
- Documents cannot be attached to contacts programmatically
- This is a limitation of the MonicaHQ API itself, not this MCP server

**Workaround:** None available. Documents must be uploaded manually through the MonicaHQ web interface.

**Technical Details:**
```
Endpoint: POST /documents (or any variant)
Status: Returns HTTP 405 Method Not Allowed
Alternative: None - requires manual upload via web UI
```

## API Endpoint Workarounds

### Root-Level POST Pattern

**Problem:** MonicaHQ's nested POST endpoints under `/contacts/{id}/` return HTTP 405 errors.

**Solution:** Use root-level POST endpoints with `contact_id` included in the request body.

#### Affected Endpoints

| Broken Endpoint | Working Endpoint | Change Required |
|-----------------|------------------|-----------------|
| `POST /contacts/{id}/relationships` | `POST /relationships` | Add `contact_id` to body |
| `POST /contacts/{id}/addresses` | `POST /addresses` | Add `contact_id` to body |
| `POST /contacts/{id}/contactfields` | `POST /contactfields` | Add `contact_id` to body |
| `POST /contacts/{id}/pets` | `POST /pets` | Add `contact_id` to body |
| `POST /contacts/{id}/calls` | `POST /calls` | Add `contact_id` to body |

#### Example: Creating a Pet

**Broken (HTTP 405):**
```bash
# This returns HTTP 405 Method Not Allowed
POST /contacts/123/pets
Content-Type: application/json

{
  "pet_category_id": 1,
  "name": "Fluffy"
}
```

**Working:**
```bash
# This works correctly
POST /pets
Content-Type: application/json

{
  "contact_id": 123,
  "pet_category_id": 1,
  "name": "Fluffy"
}
```

#### Implementation in Services

All affected services in this MCP server automatically handle this pattern:

```java
// PetService.java - Example implementation
public Mono<Map<String, Object>> createPet(Map<String, Object> arguments) {
    Map<String, Object> apiRequest = mapToApiFormat(arguments);
    // contact_id is included in apiRequest automatically

    // Use root-level POST /pets endpoint to avoid HTTP 405 error
    return monicaClient.post("/pets", apiRequest)
        .map(this::formatPetResponse);
}
```

**Services implementing this pattern:**
- `RelationshipService` - POST /relationships
- `AddressService` - POST /addresses
- `ContactFieldService` - POST /contactfields
- `PetService` - POST /pets
- `CallService` - POST /calls

### Conversation Messages Extraction

**Problem:** The endpoint `GET /conversations/{id}/messages` returns HTTP 405 (Method Not Allowed).

**Solution:** Fetch the full conversation via `GET /conversations/{id}` and extract messages from the `data.messages` field in the response.

#### Technical Details

**Broken (HTTP 405):**
```bash
# This returns HTTP 405 Method Not Allowed
GET /conversations/123/messages
```

**Working:**
```bash
# Fetch full conversation
GET /conversations/123

# Response includes messages:
{
  "data": {
    "id": 123,
    "contact_field_type_id": 1,
    "messages": [
      {"id": 1, "content": "Hello", "written_at": "2024-01-15T10:00:00Z"},
      {"id": 2, "content": "Hi there", "written_at": "2024-01-15T10:05:00Z"}
    ]
  }
}
```

#### Implementation

The `ConversationMessageService` implements this workaround:

```java
// ConversationMessageService.java
public Mono<Map<String, Object>> listConversationMessages(Map<String, Object> arguments) {
    Long conversationId = extractConversationId(arguments);

    // WORKAROUND: Use GET /conversations/{id} instead of GET /conversations/{id}/messages
    // The /messages endpoint returns HTTP 405, but messages are included in conversation
    return monicaClient.get("/conversations/" + conversationId, null)
        .map(this::extractMessagesFromConversation);
}

private Map<String, Object> extractMessagesFromConversation(Map<String, Object> apiResponse) {
    Map<String, Object> data = (Map<String, Object>) apiResponse.get("data");

    // Extract messages array - handle null/missing messages gracefully
    List<Map<String, Object>> messages = Collections.emptyList();
    if (data != null && data.containsKey("messages")) {
        Object messagesObj = data.get("messages");
        if (messagesObj instanceof List) {
            messages = (List<Map<String, Object>>) messagesObj;
        }
    }

    // Return formatted response
    return Map.of(
        "data", messages,
        "meta", Map.of("total", messages.size(), "source", "extracted_from_conversation")
    );
}
```

**Key Points:**
- The `meta.source` field indicates data was extracted from conversation
- Empty message lists are handled gracefully
- Response format matches standard list operation format

## Affected Services

| Service | Workaround | Status |
|---------|------------|--------|
| `RelationshipService` | Root-level POST | Implemented |
| `AddressService` | Root-level POST | Implemented |
| `ContactFieldService` | Root-level POST | Implemented |
| `PetService` | Root-level POST | Implemented |
| `CallService` | Root-level POST | Implemented |
| `ConversationMessageService` | Message extraction | Implemented |
| `PhotoService` | N/A | NOT SUPPORTED |
| `DocumentService` | N/A | NOT SUPPORTED |

## Troubleshooting

### HTTP 405 Errors

If you encounter HTTP 405 errors, check:

1. **Endpoint Format:** Ensure you're not using nested endpoints like `/contacts/{id}/resource`
2. **Request Body:** Verify `contact_id` is included in the request body for POST operations
3. **Method:** Confirm you're using the correct HTTP method (POST, GET, PUT, DELETE)

### Empty Message Lists

If `conversation_message_list` returns empty results:

1. **Verify Conversation ID:** Ensure the conversation exists
2. **Check Conversation Content:** Fetch the conversation directly to verify messages exist
3. **API Token Permissions:** Ensure your token has read access to conversations

### Debugging

Enable debug mode for detailed logging:

```bash
export MCP_DEBUG=true
java -jar monicahqmcp-0.1.0.jar --stdio
```

Debug logs will show:
- Actual endpoints being called
- Request/response payloads
- Any transformation applied to data

### Reporting Issues

When reporting API limitation issues:

1. Include the exact HTTP status code received
2. Provide the endpoint and method attempted
3. Note if this is a new limitation or existing behavior
4. Check MonicaHQ API documentation for any updates

## References

- [MonicaHQ API Documentation](https://github.com/monicahq/monica/blob/main/docs/api/readme.md)
- [MonicaHQ OpenAPI Spec](./monica-api-openapi.yaml)
- [MCP Server Troubleshooting](./TROUBLESHOOTING.md)

---

*Last Updated: December 2024*

*This document is maintained as part of the MonicaHQ MCP Server project. If you discover new API limitations or workarounds, please submit a pull request to update this documentation.*
