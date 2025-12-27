package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.RelationshipFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for RelationshipService covering relationship CRUD and type handling.
 */
@ExtendWith(MockitoExtension.class)
class RelationshipServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private RelationshipService relationshipService;

    private Map<String, Object> mockRelationshipData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        RelationshipFieldMappingConfig fieldMappingConfig = new RelationshipFieldMappingConfig();
        relationshipService = new RelationshipService(monicaClient, contentFormatter, fieldMappingConfig);

        mockRelationshipData = relationshipBuilder()
            .id(1L)
            .contactIs(10L)
            .ofContact(20L)
            .relationshipTypeId(5)
            .build();

        mockApiResponse = createSingleEntityResponse(mockRelationshipData);
    }

    // ========================================================================================
    // CREATE RELATIONSHIP TESTS
    // ========================================================================================

    @Test
    void createRelationship_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.createRelationship(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted relationship JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/relationships"), argThat(data ->
            data.get("contact_is").equals(10L) &&
            data.get("of_contact").equals(20L) &&
            data.get("relationship_type_id").equals(5)
        ));
    }

    @Test
    void createRelationship_MissingContactIs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.createRelationship(arguments).block();
        });
        assertEquals("contactIs is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createRelationship_NullContactIs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", null);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.createRelationship(arguments).block();
        });
        assertEquals("contactIs is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createRelationship_MissingOfContact_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.createRelationship(arguments).block();
        });
        assertEquals("ofContact is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createRelationship_NullOfContact_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", null);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.createRelationship(arguments).block();
        });
        assertEquals("ofContact is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createRelationship_MissingRelationshipTypeId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.createRelationship(arguments).block();
        });
        assertEquals("relationshipTypeId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createRelationship_NullRelationshipTypeId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.createRelationship(arguments).block();
        });
        assertEquals("relationshipTypeId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createRelationship_MapsContactIsField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        relationshipService.createRelationship(arguments).block();

        // Then - verify contactIs is mapped to contact_is
        verify(monicaClient).post(eq("/relationships"), argThat(data ->
            data.get("contact_is").equals(10L) &&
            !data.containsKey("contactIs")
        ));
    }

    @Test
    void createRelationship_MapsOfContactField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        relationshipService.createRelationship(arguments).block();

        // Then - verify ofContact is mapped to of_contact
        verify(monicaClient).post(eq("/relationships"), argThat(data ->
            data.get("of_contact").equals(20L) &&
            !data.containsKey("ofContact")
        ));
    }

    @Test
    void createRelationship_MapsRelationshipTypeIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        relationshipService.createRelationship(arguments).block();

        // Then - verify relationshipTypeId is mapped to relationship_type_id
        verify(monicaClient).post(eq("/relationships"), argThat(data ->
            data.get("relationship_type_id").equals(5) &&
            !data.containsKey("relationshipTypeId")
        ));
    }

    @Test
    void createRelationship_WithNotes_PassesThroughUnchanged() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);
        arguments.put("notes", "Met at the conference");

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        relationshipService.createRelationship(arguments).block();

        // Then - verify notes passes through unchanged
        verify(monicaClient).post(eq("/relationships"), argThat(data ->
            "Met at the conference".equals(data.get("notes"))
        ));
    }

    @Test
    void createRelationship_IntegerContactIs_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10);  // Integer instead of Long
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.createRelationship(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/relationships"), any());
    }

    @Test
    void createRelationship_StringContactIds_Succeeds() {
        // Given - String IDs that are parseable to numbers
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", "10");
        arguments.put("ofContact", "20");
        arguments.put("relationshipTypeId", "5");

        when(monicaClient.post(eq("/relationships"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.createRelationship(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/relationships"), any());
    }

    // ========================================================================================
    // GET RELATIONSHIP TESTS
    // ========================================================================================

    @Test
    void getRelationship_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/relationships/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.getRelationship(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/relationships/1"), any());
    }

    @Test
    void getRelationship_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            relationshipBuilder().id(42L).build()
        );

        when(monicaClient.get(eq("/relationships/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.getRelationship(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationships/42"), any());
    }

    @Test
    void getRelationship_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            relationshipBuilder().id(123L).build()
        );

        when(monicaClient.get(eq("/relationships/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.getRelationship(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationships/123"), any());
    }

    @Test
    void getRelationship_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("contactIs", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.getRelationship(arguments).block();
        });
        assertEquals("Relationship ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationship_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.getRelationship(arguments).block();
        });
        assertEquals("Relationship ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationship_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.getRelationship(arguments).block();
        });
        assertEquals("Invalid relationship ID format: not-a-number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationship_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("contact_is", 10L);
        apiData.put("of_contact", 20L);
        apiData.put("relationship_type_id", 5);
        apiData.put("notes", "Brother");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/relationships/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = relationshipService.getRelationship(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(10L, data.get("contactIs"));
        assertEquals(20L, data.get("ofContact"));
        assertEquals(5, data.get("relationshipTypeId"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // Notes should pass through unchanged
        assertEquals("Brother", data.get("notes"));
    }

    @Test
    void getRelationship_WithoutDataWrapper_StillWorks() {
        // Given - response without "data" wrapper (edge case)
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("contact_is", 10L);
        directResponse.put("of_contact", 20L);
        directResponse.put("relationship_type_id", 5);

        when(monicaClient.get(eq("/relationships/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = relationshipService.getRelationship(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(10L, data.get("contactIs"));
    }

    // ========================================================================================
    // UPDATE RELATIONSHIP TESTS
    // ========================================================================================

    @Test
    void updateRelationship_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 6);

        when(monicaClient.put(eq("/relationships/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        Map<String, Object> result = relationshipService.updateRelationship(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/relationships/1"), argThat(data ->
            data.get("contact_is").equals(10L) &&
            data.get("of_contact").equals(20L) &&
            data.get("relationship_type_id").equals(6)
        ));
    }

    @Test
    void updateRelationship_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.updateRelationship(arguments).block();
        });
        assertEquals("Relationship ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateRelationship_MissingContactIs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.updateRelationship(arguments).block();
        });
        assertEquals("contactIs is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateRelationship_MissingOfContact_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactIs", 10L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.updateRelationship(arguments).block();
        });
        assertEquals("ofContact is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateRelationship_MissingRelationshipTypeId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.updateRelationship(arguments).block();
        });
        assertEquals("relationshipTypeId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateRelationship_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        when(monicaClient.put(eq("/relationships/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        relationshipService.updateRelationship(arguments).block();

        // Then
        verify(monicaClient).put(eq("/relationships/42"), any());
    }

    @Test
    void updateRelationship_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("contactIs", 10L);
        arguments.put("ofContact", 20L);
        arguments.put("relationshipTypeId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.updateRelationship(arguments).block();
        });
        assertEquals("Invalid relationship ID format: invalid", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateRelationship_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactIs", 15L);
        arguments.put("ofContact", 25L);
        arguments.put("relationshipTypeId", 8);
        arguments.put("notes", "Updated notes");

        when(monicaClient.put(eq("/relationships/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship JSON");

        // When
        relationshipService.updateRelationship(arguments).block();

        // Then - verify all fields are mapped correctly
        verify(monicaClient).put(eq("/relationships/1"), argThat(data ->
            data.get("contact_is").equals(15L) &&
            !data.containsKey("contactIs") &&
            data.get("of_contact").equals(25L) &&
            !data.containsKey("ofContact") &&
            data.get("relationship_type_id").equals(8) &&
            !data.containsKey("relationshipTypeId") &&
            "Updated notes".equals(data.get("notes"))
        ));
    }

    // ========================================================================================
    // DELETE RELATIONSHIP TESTS
    // ========================================================================================

    @Test
    void deleteRelationship_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/relationships/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(eq("Delete"), eq("Relationship"), eq(1L), eq(true), anyString()))
            .thenReturn("Relationship 1 deleted successfully");

        // When
        Map<String, Object> result = relationshipService.deleteRelationship(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/relationships/1"));
    }

    @Test
    void deleteRelationship_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/relationships/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(eq("Delete"), eq("Relationship"), eq(99L), eq(true), anyString()))
            .thenReturn("Relationship 99 deleted successfully");

        // When
        Map<String, Object> result = relationshipService.deleteRelationship(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/relationships/99"));
    }

    @Test
    void deleteRelationship_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/relationships/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(eq("Delete"), eq("Relationship"), eq(55L), eq(true), anyString()))
            .thenReturn("Relationship 55 deleted successfully");

        // When
        Map<String, Object> result = relationshipService.deleteRelationship(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/relationships/55"));
    }

    @Test
    void deleteRelationship_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("contactIs", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.deleteRelationship(arguments).block();
        });
        assertEquals("Relationship ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteRelationship_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.deleteRelationship(arguments).block();
        });
        assertEquals("Relationship ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteRelationship_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipService.deleteRelationship(arguments).block();
        });
        assertEquals("Invalid relationship ID format: invalid", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST RELATIONSHIPS TESTS
    // ========================================================================================

    @Test
    void listRelationships_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).contactIs(10L).ofContact(20L).build(),
            relationshipBuilder().id(2L).contactIs(30L).ofContact(40L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships, 2, 20, 50);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = relationshipService.listRelationships(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/relationships"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationships_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        relationshipService.listRelationships(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/relationships"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationships_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships, 1, 10, 100);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = relationshipService.listRelationships(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(100, meta.get("total"));
        assertEquals(10, meta.get("last_page"));
    }

    @Test
    void listRelationships_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = relationshipService.listRelationships(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listRelationships_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        relationshipService.listRelationships(arguments).block();

        // Then
        verify(monicaClient).get(eq("/relationships"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationships_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships, 3, 10, 30);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        relationshipService.listRelationships(arguments).block();

        // Then
        verify(monicaClient).get(eq("/relationships"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listRelationships_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> relationshipWithSnakeCaseFields = new HashMap<>();
        relationshipWithSnakeCaseFields.put("id", 1L);
        relationshipWithSnakeCaseFields.put("contact_is", 10L);
        relationshipWithSnakeCaseFields.put("of_contact", 20L);
        relationshipWithSnakeCaseFields.put("relationship_type_id", 5);
        relationshipWithSnakeCaseFields.put("notes", "Brother");
        relationshipWithSnakeCaseFields.put("created_at", "2024-01-15T10:00:00Z");
        relationshipWithSnakeCaseFields.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(relationshipWithSnakeCaseFields));

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = relationshipService.listRelationships(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping from snake_case to camelCase
        assertEquals(10L, data.get(0).get("contactIs"));
        assertEquals(20L, data.get(0).get("ofContact"));
        assertEquals(5, data.get(0).get("relationshipTypeId"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // Notes should pass through unchanged
        assertEquals("Brother", data.get(0).get("notes"));
    }

    @Test
    void listRelationships_IntegerLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 50);

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        relationshipService.listRelationships(arguments).block();

        // Then
        verify(monicaClient).get(eq("/relationships"), argThat(params ->
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationships_IntegerPage_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("page", 5);

        List<Map<String, Object>> relationships = List.of(
            relationshipBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(relationships, 5, 10, 50);

        when(monicaClient.get(eq("/relationships"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        relationshipService.listRelationships(arguments).block();

        // Then
        verify(monicaClient).get(eq("/relationships"), argThat(params ->
            "5".equals(params.get("page"))
        ));
    }
}
