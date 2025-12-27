package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ContactTagFieldMappingConfig;
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
 * Unit tests for ContactTagService covering tag association with contacts,
 * including attach, detach, get, update, and list operations.
 */
@ExtendWith(MockitoExtension.class)
class ContactTagServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ContactTagService contactTagService;

    private Map<String, Object> mockTagData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        ContactTagFieldMappingConfig config = new ContactTagFieldMappingConfig();
        contactTagService = new ContactTagService(monicaClient, contentFormatter, config);

        mockTagData = tagBuilder()
            .id(1L)
            .name("Family")
            .contactCount(5)
            .build();

        mockApiResponse = createSingleEntityResponse(mockTagData);
    }

    // ========================================================================================
    // ATTACH TAG TESTS
    // ========================================================================================

    @Test
    void attachTag_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("tagId", 1L);

        when(monicaClient.post(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted contact tag JSON");

        // When
        Map<String, Object> result = contactTagService.attachTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted contact tag JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/contacts/10/setTags"), any());
    }

    @Test
    void attachTag_MapsTagIdToTagsArray() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("tagId", 5L);

        when(monicaClient.post(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactTagService.attachTag(arguments).block();

        // Then - verify tagId is converted to tags array format
        verify(monicaClient).post(eq("/contacts/10/setTags"), argThat(data -> {
            @SuppressWarnings("unchecked")
            List<Object> tags = (List<Object>) data.get("tags");
            return tags != null && tags.size() == 1 && tags.get(0).equals(5L);
        }));
    }

    @Test
    void attachTag_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "10");
        arguments.put("tagId", 1L);

        when(monicaClient.post(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactTagService.attachTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/contacts/10/setTags"), any());
    }

    @Test
    void attachTag_IntegerContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10);
        arguments.put("tagId", 1L);

        when(monicaClient.post(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactTagService.attachTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/contacts/10/setTags"), any());
    }

    @Test
    void attachTag_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("tagId", 1L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("tagId", 1L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_MissingTagId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(arguments).block();
        });
        assertEquals("tagId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_NullTagId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("tagId", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(arguments).block();
        });
        assertEquals("tagId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(arguments).block();
        });
        assertEquals("Contact Tag arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(null).block();
        });
        assertEquals("Contact Tag arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_InvalidContactIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "not-a-number");
        arguments.put("tagId", 1L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.attachTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid contact ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void attachTag_RemovesContactIdAndTagIdFromApiRequest() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("tagId", 1L);
        arguments.put("customField", "customValue");

        when(monicaClient.post(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactTagService.attachTag(arguments).block();

        // Then - verify contactId and tagId are not in request body, but custom field is
        verify(monicaClient).post(eq("/contacts/10/setTags"), argThat(data ->
            !data.containsKey("contactId") &&
            !data.containsKey("tagId") &&
            "customValue".equals(data.get("customField"))
        ));
    }

    // ========================================================================================
    // GET CONTACT TAGS TESTS
    // ========================================================================================

    @Test
    void getContactTags_ValidContactId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 10L);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Family").build(),
            tagBuilder().id(2L).name("Friends").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/contacts/10/tags"), isNull())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted tags JSON");

        // When
        Map<String, Object> result = contactTagService.getContactTags(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/contacts/10/tags"), isNull());
    }

    @Test
    void getContactTags_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", "42");

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Work").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/contacts/42/tags"), isNull())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted tags JSON");

        // When
        Map<String, Object> result = contactTagService.getContactTags(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/contacts/42/tags"), isNull());
    }

    @Test
    void getContactTags_IntegerContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 99);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Work").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/contacts/99/tags"), isNull())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted tags JSON");

        // When
        Map<String, Object> result = contactTagService.getContactTags(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/contacts/99/tags"), isNull());
    }

    @Test
    void getContactTags_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.getContactTags(arguments).block();
        });
        assertEquals("Contact ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactTags_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.getContactTags(null).block();
        });
        assertEquals("Contact ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactTags_InvalidContactIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.getContactTags(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid contact ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactTags_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 10L);

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/contacts/10/tags"), isNull())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = contactTagService.getContactTags(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void getContactTags_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 10L);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Family").build()
        );
        Map<String, Object> listResponse = createListResponse(tags, 1, 10, 5);

        when(monicaClient.get(eq("/contacts/10/tags"), isNull())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted tags JSON");

        // When
        Map<String, Object> result = contactTagService.getContactTags(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(5, meta.get("total"));
    }

    // ========================================================================================
    // UPDATE CONTACT TAGS TESTS
    // ========================================================================================

    @Test
    void updateContactTags_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("tagId", 5L);

        when(monicaClient.put(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted contact tag JSON");

        // When
        Map<String, Object> result = contactTagService.updateContactTags(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/contacts/10/setTags"), any());
    }

    @Test
    void updateContactTags_MapsTagIdToTagsArray() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("tagId", 7L);

        when(monicaClient.put(eq("/contacts/10/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactTagService.updateContactTags(arguments).block();

        // Then - verify tagId is converted to tags array format
        verify(monicaClient).put(eq("/contacts/10/setTags"), argThat(data -> {
            @SuppressWarnings("unchecked")
            List<Object> tags = (List<Object>) data.get("tags");
            return tags != null && tags.size() == 1 && tags.get(0).equals(7L);
        }));
    }

    @Test
    void updateContactTags_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "25");
        arguments.put("tagId", 1L);

        when(monicaClient.put(eq("/contacts/25/setTags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactTagService.updateContactTags(arguments).block();

        // Then
        verify(monicaClient).put(eq("/contacts/25/setTags"), any());
    }

    @Test
    void updateContactTags_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("tagId", 1L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.updateContactTags(arguments).block();
        });
        assertEquals("Contact ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateContactTags_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.updateContactTags(null).block();
        });
        assertEquals("Contact ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateContactTags_InvalidContactIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "invalid");
        arguments.put("tagId", 1L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.updateContactTags(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid contact ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // DETACH TAG TESTS
    // ========================================================================================

    @Test
    void detachTag_ValidArgs_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of(
            "contactId", 10L,
            "tagId", 1L
        );
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/contacts/10/unsetTag/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Detach"), eq("Contact Tag"), eq(1L), eq(true), anyString()
        )).thenReturn("Tag detached successfully");

        // When
        Map<String, Object> result = contactTagService.detachTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/contacts/10/unsetTag/1"));
    }

    @Test
    void detachTag_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "contactId", "42",
            "tagId", 5L
        );
        Map<String, Object> deleteResponse = createDeleteResponse(5L);

        when(monicaClient.delete(eq("/contacts/42/unsetTag/5"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Detach"), eq("Contact Tag"), eq(5L), eq(true), anyString()
        )).thenReturn("Tag detached successfully");

        // When
        Map<String, Object> result = contactTagService.detachTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/contacts/42/unsetTag/5"));
    }

    @Test
    void detachTag_StringTagId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "contactId", 10L,
            "tagId", "99"
        );
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/contacts/10/unsetTag/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Detach"), eq("Contact Tag"), eq(99L), eq(true), anyString()
        )).thenReturn("Tag detached successfully");

        // When
        Map<String, Object> result = contactTagService.detachTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/contacts/10/unsetTag/99"));
    }

    @Test
    void detachTag_IntegerIds_ParseCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "contactId", 15,
            "tagId", 7
        );
        Map<String, Object> deleteResponse = createDeleteResponse(7L);

        when(monicaClient.delete(eq("/contacts/15/unsetTag/7"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Detach"), eq("Contact Tag"), eq(7L), eq(true), anyString()
        )).thenReturn("Tag detached successfully");

        // When
        Map<String, Object> result = contactTagService.detachTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/contacts/15/unsetTag/7"));
    }

    @Test
    void detachTag_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", 1L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.detachTag(arguments).block();
        });
        assertEquals("Contact ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void detachTag_MissingTagId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.detachTag(arguments).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void detachTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.detachTag(null).block();
        });
        assertEquals("Contact ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void detachTag_InvalidContactIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of(
            "contactId", "invalid",
            "tagId", 1L
        );

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.detachTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid contact ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void detachTag_InvalidTagIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of(
            "contactId", 10L,
            "tagId", "invalid"
        );

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.detachTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid tag ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST CONTACTS BY TAG TESTS
    // ========================================================================================

    @Test
    void listContactsByTag_ValidArgs_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", 1L);

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").lastName("Doe").build(),
            contactBuilder().id(11L).firstName("Jane").lastName("Smith").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        Map<String, Object> result = contactTagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        // Verify tags query param is included
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "1".equals(params.get("tags"))
        ));
    }

    @Test
    void listContactsByTag_WithPagination_IncludesQueryParams() {
        // Given
        Map<String, Object> arguments = Map.of(
            "tagId", 1L,
            "page", 2,
            "limit", 15
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts, 2, 15, 30);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "2".equals(params.get("page")) &&
            "15".equals(params.get("limit")) &&
            "1".equals(params.get("tags"))
        ));
    }

    @Test
    void listContactsByTag_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", 5L);

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactsByTag_StringTagId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", "42");

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "42".equals(params.get("tags"))
        ));
    }

    @Test
    void listContactsByTag_IntegerTagId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", 99);

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "99".equals(params.get("tags"))
        ));
    }

    @Test
    void listContactsByTag_MissingTagId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.listContactsByTag(arguments).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactsByTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.listContactsByTag(null).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactsByTag_InvalidTagIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactTagService.listContactsByTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid tag ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactsByTag_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of(
            "tagId", 1L,
            "limit", 200
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactsByTag_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of(
            "tagId", 1L,
            "limit", 0
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactsByTag_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of(
            "tagId", 1L,
            "limit", -5
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactsByTag_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", 1L);

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = contactTagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listContactsByTag_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("tagId", 1L, "page", 1, "limit", 10);

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts, 1, 10, 25);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        Map<String, Object> result = contactTagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(25, meta.get("total"));
    }

    @Test
    void listContactsByTag_StringLimitAndPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "tagId", 1L,
            "page", "3",
            "limit", "25"
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts, 3, 25, 100);

        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        contactTagService.listContactsByTag(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts"), argThat(params ->
            "3".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ));
    }
}
