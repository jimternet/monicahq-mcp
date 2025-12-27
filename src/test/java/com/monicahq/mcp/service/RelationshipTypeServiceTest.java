package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.RelationshipTypeFieldMappingConfig;
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
 * Unit tests for RelationshipTypeService covering getRelationshipType, listRelationshipTypes
 * operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class RelationshipTypeServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private RelationshipTypeService relationshipTypeService;

    private Map<String, Object> mockRelationshipTypeData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        RelationshipTypeFieldMappingConfig fieldMappingConfig = new RelationshipTypeFieldMappingConfig();
        relationshipTypeService = new RelationshipTypeService(monicaClient, contentFormatter, fieldMappingConfig);

        mockRelationshipTypeData = relationshipTypeBuilder()
            .id(1L)
            .name("Partner")
            .nameReverse("Partner of")
            .relationshipTypeGroupId(1)
            .build();

        mockApiResponse = createSingleEntityResponse(mockRelationshipTypeData);
    }

    // ========================================================================================
    // GET RELATIONSHIP TYPE TESTS
    // ========================================================================================

    @Test
    void getRelationshipType_ValidLongId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        when(monicaClient.get(eq("/relationshiptypes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type JSON");

        // When
        Map<String, Object> result = relationshipTypeService.getRelationshipType(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted relationship type JSON", content.get(0).get("text"));

        verify(monicaClient).get(eq("/relationshiptypes/1"), isNull());
    }

    @Test
    void getRelationshipType_ValidIntegerId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 42);

        Map<String, Object> typeData = relationshipTypeBuilder()
            .id(42L)
            .name("Friend")
            .nameReverse("Friend of")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(typeData);

        when(monicaClient.get(eq("/relationshiptypes/42"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type JSON");

        // When
        Map<String, Object> result = relationshipTypeService.getRelationshipType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes/42"), isNull());
    }

    @Test
    void getRelationshipType_ValidStringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "123");

        Map<String, Object> typeData = relationshipTypeBuilder()
            .id(123L)
            .name("Sibling")
            .nameReverse("Sibling of")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(typeData);

        when(monicaClient.get(eq("/relationshiptypes/123"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type JSON");

        // When
        Map<String, Object> result = relationshipTypeService.getRelationshipType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes/123"), isNull());
    }

    @Test
    void getRelationshipType_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeService.getRelationshipType(arguments).block();
        });
        assertEquals("Relationship Type ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationshipType_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeService.getRelationshipType(arguments).block();
        });
        assertEquals("Relationship Type ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationshipType_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeService.getRelationshipType(arguments).block();
        });
        assertEquals("Invalid relationship type ID format: not-a-number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationshipType_DirectResponseWithoutDataWrapper_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        // API returns relationship type directly without data wrapper
        Map<String, Object> directResponse = relationshipTypeBuilder()
            .id(1L)
            .name("Parent")
            .nameReverse("Child of")
            .build();

        when(monicaClient.get(eq("/relationshiptypes/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type JSON");

        // When
        Map<String, Object> result = relationshipTypeService.getRelationshipType(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        verify(monicaClient).get(eq("/relationshiptypes/1"), isNull());
    }

    @Test
    void getRelationshipType_ResponseFieldMapping_MapsSnakeCaseToCamelCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> typeData = new HashMap<>();
        typeData.put("id", 1L);
        typeData.put("name", "Partner");
        typeData.put("name_reverse", "Partner of");
        typeData.put("relationship_type_group_id", 1);
        typeData.put("created_at", "2023-01-01T00:00:00Z");
        typeData.put("updated_at", "2023-01-02T00:00:00Z");
        Map<String, Object> apiResponse = createSingleEntityResponse(typeData);

        when(monicaClient.get(eq("/relationshiptypes/1"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type JSON");

        // When
        Map<String, Object> result = relationshipTypeService.getRelationshipType(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("Partner", data.get("name"));
        assertEquals("Partner of", data.get("nameReverse")); // name_reverse -> nameReverse
        assertEquals(1, data.get("relationshipTypeGroupId")); // relationship_type_group_id -> relationshipTypeGroupId
        assertEquals("2023-01-01T00:00:00Z", data.get("createdAt")); // created_at -> createdAt
        assertEquals("2023-01-02T00:00:00Z", data.get("updatedAt")); // updated_at -> updatedAt
    }

    @Test
    void getRelationshipType_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeService.getRelationshipType(arguments).block();
        });
        assertEquals("Relationship Type ID is required", exception.getMessage());
    }

    @Test
    void getRelationshipType_LargeId_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", Long.MAX_VALUE);

        Map<String, Object> typeData = relationshipTypeBuilder()
            .id(Long.MAX_VALUE)
            .name("Custom Type")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(typeData);

        when(monicaClient.get(eq("/relationshiptypes/" + Long.MAX_VALUE), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type JSON");

        // When
        Map<String, Object> result = relationshipTypeService.getRelationshipType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes/" + Long.MAX_VALUE), isNull());
    }

    // ========================================================================================
    // LIST RELATIONSHIP TYPES TESTS
    // ========================================================================================

    @Test
    void listRelationshipTypes_DefaultPagination_UsesDefaultValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Partner").build(),
            relationshipTypeBuilder().id(2L).name("Friend").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 2);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes"), argThat(params ->
            "1".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypes_CustomPagination_UsesProvidedValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", 2);
        arguments.put("limit", 25);

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(3L).name("Sibling").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 2, 25, 26);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes"), argThat(params ->
            "2".equals(params.get("page")) && "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypes_StringPagination_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", "3");
        arguments.put("limit", "15");

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Parent").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 3, 15, 31);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes"), argThat(params ->
            "3".equals(params.get("page")) && "15".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypes_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of();
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 0);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listRelationshipTypes_IncludesMetadata_WhenPresent() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Partner").build(),
            relationshipTypeBuilder().id(2L).name("Friend").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 100);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(100, meta.get("total"));
    }

    @Test
    void listRelationshipTypes_MultipleItems_MapsAllFields() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> type1 = new HashMap<>();
        type1.put("id", 1L);
        type1.put("name", "Partner");
        type1.put("name_reverse", "Partner of");
        type1.put("relationship_type_group_id", 1);

        Map<String, Object> type2 = new HashMap<>();
        type2.put("id", 2L);
        type2.put("name", "Child");
        type2.put("name_reverse", "Parent of");
        type2.put("relationship_type_group_id", 2);

        List<Map<String, Object>> types = List.of(type1, type2);
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 2);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
        assertEquals("Partner", data.get(0).get("name"));
        assertEquals("Partner of", data.get(0).get("nameReverse")); // name_reverse -> nameReverse
        assertEquals(1, data.get(0).get("relationshipTypeGroupId")); // relationship_type_group_id -> relationshipTypeGroupId
        assertEquals("Child", data.get(1).get("name"));
        assertEquals("Parent of", data.get(1).get("nameReverse"));
    }

    @Test
    void listRelationshipTypes_ContentFieldFormat_IsCorrect() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Partner").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 1);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list content");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted list content", content.get(0).get("text"));
    }

    @Test
    void listRelationshipTypes_NullArgs_UsesDefaults() {
        // Given - null arguments should not throw, will use defaults
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Partner").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 1);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypes"), argThat(params ->
            "1".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypes_NoMetaResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Partner").build()
        );
        Map<String, Object> apiResponse = new HashMap<>();
        apiResponse.put("data", types);
        // No meta key

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        Map<String, Object> result = relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listRelationshipTypes_FormatterInvoked_WithApiResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> types = List.of(
            relationshipTypeBuilder().id(1L).name("Partner").build()
        );
        Map<String, Object> apiResponse = createListResponse(types, 1, 10, 1);

        when(monicaClient.get(eq("/relationshiptypes"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship types JSON");

        // When
        relationshipTypeService.listRelationshipTypes(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(apiResponse);
    }

    // ========================================================================================
    // RELATIONSHIP TYPE DATA BUILDER
    // ========================================================================================

    /**
     * Builder for creating mock relationship type data.
     */
    protected RelationshipTypeDataBuilder relationshipTypeBuilder() {
        return new RelationshipTypeDataBuilder();
    }

    public static class RelationshipTypeDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public RelationshipTypeDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "Partner");
            data.put("name_reverse", "Partner of");
            data.put("relationship_type_group_id", 1);
        }

        public RelationshipTypeDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public RelationshipTypeDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public RelationshipTypeDataBuilder nameReverse(String nameReverse) {
            data.put("name_reverse", nameReverse);
            return this;
        }

        public RelationshipTypeDataBuilder relationshipTypeGroupId(Integer groupId) {
            data.put("relationship_type_group_id", groupId);
            return this;
        }

        public RelationshipTypeDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
