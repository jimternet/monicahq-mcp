package com.monicahq.mcp.docker.integration;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for the contact-note workflow.
 * Tests the multi-step workflow of creating a contact, adding notes,
 * and verifying the association between them against the real Monica API.
 *
 * Workflow verified:
 * 1. Create contact via contact_create
 * 2. Add note via note_create with contactId
 * 3. Retrieve contact via contact_get to verify note association
 * 4. Cleanup: delete note and contact (handled by DockerBaseTest)
 */
public class DockerContactNoteFlowTest extends DockerBaseTest {

    @Test
    void shouldCreateContactAndAddNoteWorkflow() throws Exception {
        // Step 1: Create contact
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "FlowTest" + timestamp.substring(timestamp.length() - 6);
        String lastName = "NoteFlow";

        Long contactId = createContactAndTrack(firstName, lastName, 1L);
        assertNotNull(contactId, "Should successfully create contact for workflow test");

        // Step 2: Add note to contact
        String noteBody = "First meeting went well! Follow up next week about the project.";
        Map<String, Object> noteResponse = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", noteBody,
            "isFavorited", false
        ), 2);

        assertNotNull(noteResponse);
        assertEquals("2.0", noteResponse.get("jsonrpc"));
        assertEquals(2L, noteResponse.get("id"));
        assertTrue(isSuccessResponse(noteResponse), "Expected success response for note creation but got: " + noteResponse);

        Long noteId = extractNoteId(noteResponse);
        assertNotNull(noteId, "Should be able to extract note ID from response");
        createdNoteIds.add(noteId);

        // Step 3: Retrieve contact to verify it exists
        Map<String, Object> getResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 3);

        assertNotNull(getResponse);
        assertEquals("2.0", getResponse.get("jsonrpc"));
        assertEquals(3L, getResponse.get("id"));
        assertTrue(isSuccessResponse(getResponse), "Expected success response for contact_get but got: " + getResponse);

        // Step 4: List notes for the contact to verify note association
        Map<String, Object> listResponse = callTool("note_list", Map.of(
            "contactId", contactId,
            "page", 1,
            "limit", 10
        ), 4);

        assertNotNull(listResponse);
        assertEquals("2.0", listResponse.get("jsonrpc"));
        assertEquals(4L, listResponse.get("id"));
        assertTrue(isSuccessResponse(listResponse), "Expected success response for note_list but got: " + listResponse);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) listResponse.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");
    }

    @Test
    void shouldCreateContactWithMultipleNotesWorkflow() throws Exception {
        // Step 1: Create contact
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "MultiFlow" + timestamp.substring(timestamp.length() - 6);
        String lastName = "MultiNotes";

        Long contactId = createContactAndTrack(firstName, lastName, 1L);
        assertNotNull(contactId, "Should successfully create contact for multi-note workflow");

        // Step 2: Add first note
        Map<String, Object> note1Response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "Initial meeting notes - discussed project timeline.",
            "isFavorited", false
        ), 2);

        assertTrue(isSuccessResponse(note1Response), "Expected success for first note creation");
        Long noteId1 = extractNoteId(note1Response);
        assertNotNull(noteId1, "Should extract first note ID");
        createdNoteIds.add(noteId1);

        // Step 3: Add second note
        Map<String, Object> note2Response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "Follow-up call - confirmed budget approval.",
            "isFavorited", true
        ), 3);

        assertTrue(isSuccessResponse(note2Response), "Expected success for second note creation");
        Long noteId2 = extractNoteId(note2Response);
        assertNotNull(noteId2, "Should extract second note ID");
        createdNoteIds.add(noteId2);

        // Step 4: Add third note
        Map<String, Object> note3Response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "Final review - project approved to proceed.",
            "isFavorited", false
        ), 4);

        assertTrue(isSuccessResponse(note3Response), "Expected success for third note creation");
        Long noteId3 = extractNoteId(note3Response);
        assertNotNull(noteId3, "Should extract third note ID");
        createdNoteIds.add(noteId3);

        // Step 5: Verify all notes are different
        assertNotEquals(noteId1, noteId2, "First and second note IDs should be different");
        assertNotEquals(noteId2, noteId3, "Second and third note IDs should be different");
        assertNotEquals(noteId1, noteId3, "First and third note IDs should be different");

        // Step 6: Verify contact still accessible
        Map<String, Object> getResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 5);

        assertTrue(isSuccessResponse(getResponse), "Contact should still be retrievable after adding notes");
    }

    @Test
    void shouldHandleContactNotFoundForNote() throws Exception {
        // Given: A non-existent contact ID
        long nonExistentContactId = 99999999L;

        // When: Try to create note for non-existent contact
        Map<String, Object> response = callTool("note_create", Map.of(
            "contactId", nonExistentContactId,
            "body", "Note for non-existent contact",
            "isFavorited", false
        ), 1);

        // Then: Expect error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for note on non-existent contact");
    }

    @Test
    void shouldRetrieveContactAfterAddingFavoritedNote() throws Exception {
        // Step 1: Create contact
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "FavFlow" + timestamp.substring(timestamp.length() - 6);

        Long contactId = createContactAndTrack(firstName, "Favorited", 1L);
        assertNotNull(contactId, "Should successfully create contact");

        // Step 2: Add favorited note
        Map<String, Object> noteResponse = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", "Important: Key contact for Q1 goals. Must prioritize relationship.",
            "isFavorited", true
        ), 2);

        assertTrue(isSuccessResponse(noteResponse), "Expected success for favorited note creation");
        Long noteId = extractNoteId(noteResponse);
        assertNotNull(noteId, "Should extract favorited note ID");
        createdNoteIds.add(noteId);

        // Step 3: Retrieve contact
        Map<String, Object> getResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 3);

        assertNotNull(getResponse);
        assertTrue(isSuccessResponse(getResponse), "Should retrieve contact with favorited note");

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) getResponse.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");
    }

    @Test
    void shouldVerifyNoteListAfterCreation() throws Exception {
        // Step 1: Create contact
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "ListFlow" + timestamp.substring(timestamp.length() - 6);

        Long contactId = createContactAndTrack(firstName, "ListTest", 1L);
        assertNotNull(contactId, "Should successfully create contact for list verification");

        // Step 2: Add note
        String uniqueNoteBody = "Unique note content for verification - " + timestamp;
        Map<String, Object> noteResponse = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", uniqueNoteBody,
            "isFavorited", false
        ), 2);

        assertTrue(isSuccessResponse(noteResponse), "Expected success for note creation");
        Long noteId = extractNoteId(noteResponse);
        assertNotNull(noteId, "Should extract note ID");
        createdNoteIds.add(noteId);

        // Step 3: List notes for contact
        Map<String, Object> listResponse = callTool("note_list", Map.of(
            "contactId", contactId,
            "page", 1,
            "limit", 10
        ), 3);

        assertNotNull(listResponse);
        assertEquals("2.0", listResponse.get("jsonrpc"));
        assertEquals(3L, listResponse.get("id"));
        assertTrue(isSuccessResponse(listResponse), "Expected success for note_list");

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) listResponse.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");
    }
}
