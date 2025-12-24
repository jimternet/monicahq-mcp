package com.monicahq.mcp.docker.contract;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for note_create MCP operation.
 * Tests note creation against the real Monica API.
 * Extends DockerBaseTest for health checks and automatic cleanup.
 *
 * Notes require an associated contact, so this test first creates
 * a contact to link notes to.
 */
public class DockerNoteCreateTest extends DockerBaseTest {

    @Test
    void shouldCreateNoteViaMcpProtocol() throws Exception {
        // Given: Create a contact to associate the note with
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "NoteTest" + timestamp.substring(timestamp.length() - 6);
        Long contactId = createContactAndTrack(firstName, "ForNote", 1L);
        assertNotNull(contactId, "Should create contact for note testing");

        // When: Send MCP tools/call request to create note
        Map<String, Object> response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "John mentioned he's interested in the new project. Follow up next week.",
            "isFavorited", false
        ), 1);

        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");

        // Extract and track note ID for cleanup
        Long noteId = extractNoteId(response);
        assertNotNull(noteId, "Should be able to extract note ID from response");
        createdNoteIds.add(noteId);
    }

    @Test
    void shouldValidateRequiredFieldsForNote() throws Exception {
        // Given: MCP request missing required contactId field
        Map<String, Object> response = callTool("note_create", Map.of(
            "body", "Missing contact ID"
        ), 2);

        // Then: Expect validation error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(2L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for missing required fields");

        Integer errorCode = getErrorCode(response);
        assertNotNull(errorCode, "Error response should contain error code");
        assertEquals(-32602, errorCode, "Should return invalid params error code");

        String errorMessage = getErrorMessage(response);
        assertNotNull(errorMessage, "Error response should contain error message");
        assertTrue(errorMessage.contains("Invalid params"), "Error message should indicate invalid params");
    }

    @Test
    void shouldCreateNoteWithEmptyBody() throws Exception {
        // Given: Create a contact to associate the note with
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "EmptyNote" + timestamp.substring(timestamp.length() - 6);
        Long contactId = createContactAndTrack(firstName, "EmptyBody", 1L);
        assertNotNull(contactId, "Should create contact for note testing");

        // When: Create note with empty body (should be allowed)
        Map<String, Object> response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", ""
        ), 3);

        // Then: Verify success (empty notes are allowed)
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(3L, response.get("id"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");

        // Track for cleanup
        Long noteId = extractNoteId(response);
        assertNotNull(noteId, "Should be able to extract note ID from response");
        createdNoteIds.add(noteId);
    }

    @Test
    void shouldCreateFavoritedNote() throws Exception {
        // Given: Create a contact to associate the note with
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "FavNote" + timestamp.substring(timestamp.length() - 6);
        Long contactId = createContactAndTrack(firstName, "Favorited", 1L);
        assertNotNull(contactId, "Should create contact for note testing");

        // When: Create note with isFavorited set to true
        Map<String, Object> response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "This is an important note that should be favorited.",
            "isFavorited", true
        ), 4);

        // Then: Verify success
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(4L, response.get("id"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");

        // Track for cleanup
        Long noteId = extractNoteId(response);
        assertNotNull(noteId, "Should be able to extract note ID from response");
        createdNoteIds.add(noteId);
    }

    @Test
    void shouldFailForNonExistentContact() throws Exception {
        // Given: Use a non-existent contact ID
        long nonExistentContactId = 99999999L;

        // When: Try to create a note for non-existent contact
        Map<String, Object> response = callTool("note_create", Map.of(
            "contactId", nonExistentContactId,
            "body", "This note should fail because contact doesn't exist."
        ), 5);

        // Then: Expect error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(5L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for non-existent contact");
    }

    @Test
    void shouldCreateMultipleNotesForSameContact() throws Exception {
        // Given: Create a contact to associate notes with
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "MultiNote" + timestamp.substring(timestamp.length() - 6);
        Long contactId = createContactAndTrack(firstName, "MultiNotes", 1L);
        assertNotNull(contactId, "Should create contact for note testing");

        // When: Create first note
        Map<String, Object> response1 = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "First note for this contact."
        ), 6);

        // Then: Verify first note success
        assertNotNull(response1);
        assertTrue(isSuccessResponse(response1), "Expected success response for first note");
        Long noteId1 = extractNoteId(response1);
        assertNotNull(noteId1, "Should be able to extract first note ID");
        createdNoteIds.add(noteId1);

        // When: Create second note
        Map<String, Object> response2 = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "Second note for the same contact."
        ), 7);

        // Then: Verify second note success
        assertNotNull(response2);
        assertTrue(isSuccessResponse(response2), "Expected success response for second note");
        Long noteId2 = extractNoteId(response2);
        assertNotNull(noteId2, "Should be able to extract second note ID");
        createdNoteIds.add(noteId2);

        // Verify they are different notes
        assertNotEquals(noteId1, noteId2, "Two notes should have different IDs");
    }
}
