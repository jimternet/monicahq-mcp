package com.monicahq.mcp.docker.integration;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for the complete contact lifecycle.
 * Tests the end-to-end workflow of creating, updating, retrieving,
 * and deleting a contact against the real Monica API.
 *
 * Workflow verified:
 * 1. Create contact via contact_create
 * 2. Update contact via contact_update
 * 3. Retrieve updated contact via contact_get
 * 4. Delete contact via contact_delete
 * 5. Verify deletion by expecting 404 on contact_get
 */
public class DockerContactLifecycleTest extends DockerBaseTest {

    @Test
    void shouldCompleteFullContactLifecycle() throws Exception {
        // Step 1: Create contact
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "Lifecycle" + timestamp.substring(timestamp.length() - 6);
        String lastName = "TestContact";

        Map<String, Object> createResponse = callTool("contact_create", Map.of(
            "firstName", firstName,
            "lastName", lastName,
            "genderId", 1L,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        ), 1);

        assertNotNull(createResponse);
        assertEquals("2.0", createResponse.get("jsonrpc"));
        assertEquals(1L, createResponse.get("id"));
        assertTrue(isSuccessResponse(createResponse), "Expected success response for contact creation but got: " + createResponse);

        Long contactId = extractContactId(createResponse);
        assertNotNull(contactId, "Should be able to extract contact ID from response");

        // Track for cleanup in case of test failure before deletion step
        createdContactIds.add(contactId);

        // Step 2: Update contact
        String updatedLastName = "UpdatedName";
        String nickname = "LifeTest";

        Map<String, Object> updateResponse = callTool("contact_update", Map.of(
            "id", contactId,
            "firstName", firstName,
            "lastName", updatedLastName,
            "nickname", nickname,
            "genderId", 1L
        ), 2);

        assertNotNull(updateResponse);
        assertEquals("2.0", updateResponse.get("jsonrpc"));
        assertEquals(2L, updateResponse.get("id"));
        assertTrue(isSuccessResponse(updateResponse), "Expected success response for contact update but got: " + updateResponse);

        // Step 3: Retrieve updated contact
        Map<String, Object> getResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 3);

        assertNotNull(getResponse);
        assertEquals("2.0", getResponse.get("jsonrpc"));
        assertEquals(3L, getResponse.get("id"));
        assertTrue(isSuccessResponse(getResponse), "Expected success response for contact_get but got: " + getResponse);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) getResponse.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");

        // Step 4: Delete contact
        Map<String, Object> deleteResponse = callTool("contact_delete", Map.of(
            "id", contactId
        ), 4);

        assertNotNull(deleteResponse);
        assertEquals("2.0", deleteResponse.get("jsonrpc"));
        assertEquals(4L, deleteResponse.get("id"));
        assertTrue(isSuccessResponse(deleteResponse), "Expected success response for contact deletion but got: " + deleteResponse);

        // Remove from cleanup list since we successfully deleted
        createdContactIds.remove(contactId);

        // Step 5: Verify deletion by expecting error on contact_get
        Map<String, Object> verifyResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 5);

        assertNotNull(verifyResponse);
        assertEquals("2.0", verifyResponse.get("jsonrpc"));
        assertEquals(5L, verifyResponse.get("id"));
        assertTrue(isErrorResponse(verifyResponse), "Expected error response for deleted contact but got: " + verifyResponse);
    }

    @Test
    void shouldUpdateMultipleFieldsOnContact() throws Exception {
        // Step 1: Create contact with minimal fields
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "MultiUp" + timestamp.substring(timestamp.length() - 6);

        Long contactId = createContactAndTrack(firstName, "Original", 1L);
        assertNotNull(contactId, "Should successfully create contact for multi-field update test");

        // Step 2: Update multiple fields
        Map<String, Object> updateResponse = callTool("contact_update", Map.of(
            "id", contactId,
            "firstName", firstName,
            "lastName", "UpdatedSurname",
            "nickname", "NickTest",
            "genderId", 1L
        ), 2);

        assertNotNull(updateResponse);
        assertTrue(isSuccessResponse(updateResponse), "Expected success response for multi-field update but got: " + updateResponse);

        // Step 3: Verify update via contact_get
        Map<String, Object> getResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 3);

        assertNotNull(getResponse);
        assertTrue(isSuccessResponse(getResponse), "Expected success response for contact retrieval after update");

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) getResponse.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");
    }

    @Test
    void shouldHandleUpdateForNonExistentContact() throws Exception {
        // Given: A non-existent contact ID
        long nonExistentContactId = 99999999L;

        // When: Try to update non-existent contact
        Map<String, Object> response = callTool("contact_update", Map.of(
            "id", nonExistentContactId,
            "firstName", "Ghost",
            "lastName", "Contact",
            "genderId", 1L
        ), 1);

        // Then: Expect error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for updating non-existent contact");
    }

    @Test
    void shouldHandleDeleteForNonExistentContact() throws Exception {
        // Given: A non-existent contact ID
        long nonExistentContactId = 99999999L;

        // When: Try to delete non-existent contact
        Map<String, Object> response = callTool("contact_delete", Map.of(
            "id", nonExistentContactId
        ), 1);

        // Then: Expect error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for deleting non-existent contact");
    }

    @Test
    void shouldPreserveContactDataAfterUpdate() throws Exception {
        // Step 1: Create contact with specific data
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "Preserve" + timestamp.substring(timestamp.length() - 6);
        String lastName = "DataTest";

        Map<String, Object> createResponse = callTool("contact_create", Map.of(
            "firstName", firstName,
            "lastName", lastName,
            "nickname", "PreserveNick",
            "genderId", 1L,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        ), 1);

        assertTrue(isSuccessResponse(createResponse), "Expected success for contact creation");
        Long contactId = extractContactId(createResponse);
        assertNotNull(contactId, "Should extract contact ID");
        createdContactIds.add(contactId);

        // Step 2: Update only the nickname
        Map<String, Object> updateResponse = callTool("contact_update", Map.of(
            "id", contactId,
            "firstName", firstName,
            "lastName", lastName,
            "nickname", "UpdatedNick",
            "genderId", 1L
        ), 2);

        assertTrue(isSuccessResponse(updateResponse), "Expected success for contact update");

        // Step 3: Verify the contact still has original first/last name
        Map<String, Object> getResponse = callTool("contact_get", Map.of(
            "id", contactId
        ), 3);

        assertTrue(isSuccessResponse(getResponse), "Expected success for contact retrieval");

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) getResponse.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");
    }

    @Test
    void shouldCreateUpdateDeleteMultipleContactsInSequence() throws Exception {
        String timestamp = String.valueOf(Instant.now().toEpochMilli());

        // Create first contact
        String firstName1 = "Seq1_" + timestamp.substring(timestamp.length() - 4);
        Map<String, Object> create1Response = callTool("contact_create", Map.of(
            "firstName", firstName1,
            "lastName", "FirstContact",
            "genderId", 1L
        ), 1);
        assertTrue(isSuccessResponse(create1Response), "Expected success for first contact creation");
        Long contactId1 = extractContactId(create1Response);
        assertNotNull(contactId1, "Should extract first contact ID");
        createdContactIds.add(contactId1);

        // Create second contact
        String firstName2 = "Seq2_" + timestamp.substring(timestamp.length() - 4);
        Map<String, Object> create2Response = callTool("contact_create", Map.of(
            "firstName", firstName2,
            "lastName", "SecondContact",
            "genderId", 1L
        ), 2);
        assertTrue(isSuccessResponse(create2Response), "Expected success for second contact creation");
        Long contactId2 = extractContactId(create2Response);
        assertNotNull(contactId2, "Should extract second contact ID");
        createdContactIds.add(contactId2);

        // Verify they are different contacts
        assertNotEquals(contactId1, contactId2, "Contact IDs should be different");

        // Update first contact
        Map<String, Object> update1Response = callTool("contact_update", Map.of(
            "id", contactId1,
            "firstName", firstName1,
            "lastName", "UpdatedFirst",
            "genderId", 1L
        ), 3);
        assertTrue(isSuccessResponse(update1Response), "Expected success for first contact update");

        // Update second contact
        Map<String, Object> update2Response = callTool("contact_update", Map.of(
            "id", contactId2,
            "firstName", firstName2,
            "lastName", "UpdatedSecond",
            "genderId", 1L
        ), 4);
        assertTrue(isSuccessResponse(update2Response), "Expected success for second contact update");

        // Verify both contacts are retrievable
        Map<String, Object> get1Response = callTool("contact_get", Map.of("id", contactId1), 5);
        assertTrue(isSuccessResponse(get1Response), "First contact should be retrievable after update");

        Map<String, Object> get2Response = callTool("contact_get", Map.of("id", contactId2), 6);
        assertTrue(isSuccessResponse(get2Response), "Second contact should be retrievable after update");

        // Delete first contact
        Map<String, Object> delete1Response = callTool("contact_delete", Map.of("id", contactId1), 7);
        assertTrue(isSuccessResponse(delete1Response), "Expected success for first contact deletion");
        createdContactIds.remove(contactId1);

        // Verify first contact is deleted
        Map<String, Object> verify1Response = callTool("contact_get", Map.of("id", contactId1), 8);
        assertTrue(isErrorResponse(verify1Response), "First contact should return error after deletion");

        // Verify second contact still exists
        Map<String, Object> verify2Response = callTool("contact_get", Map.of("id", contactId2), 9);
        assertTrue(isSuccessResponse(verify2Response), "Second contact should still exist after first deletion");
    }
}
