package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.RelationshipTypeGroupFieldMappingConfig;
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
 * Unit tests for RelationshipTypeGroupService covering getRelationshipTypeGroup,
 * listRelationshipTypeGroups operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class RelationshipTypeGroupServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private RelationshipTypeGroupService relationshipTypeGroupService;

    private Map<String, Object> mockRelationshipTypeGroupData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        RelationshipTypeGroupFieldMappingConfig fieldMappingConfig = new RelationshipTypeGroupFieldMappingConfig();
        relationshipTypeGroupService = new RelationshipTypeGroupService(monicaClient, contentFormatter, fieldMappingConfig);

        mockRelationshipTypeGroupData = relationshipTypeGroupBuilder()
            .id(1L)
            .name("Love")
            .build();

        mockApiResponse = createSingleEntityResponse(mockRelationshipTypeGroupData);
    }

    // ========================================================================================
    // GET RELATIONSHIP TYPE GROUP TESTS
    // ========================================================================================

    @Test
    void getRelationshipTypeGroup_ValidLongId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        when(monicaClient.get(eq("/relationshiptypegroups/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted relationship type group JSON", content.get(0).get("text"));

        verify(monicaClient).get(eq("/relationshiptypegroups/1"), isNull());
    }

    @Test
    void getRelationshipTypeGroup_ValidIntegerId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 42);

        Map<String, Object> groupData = relationshipTypeGroupBuilder()
            .id(42L)
            .name("Family")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(groupData);

        when(monicaClient.get(eq("/relationshiptypegroups/42"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups/42"), isNull());
    }

    @Test
    void getRelationshipTypeGroup_ValidStringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "123");

        Map<String, Object> groupData = relationshipTypeGroupBuilder()
            .id(123L)
            .name("Friends")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(groupData);

        when(monicaClient.get(eq("/relationshiptypegroups/123"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups/123"), isNull());
    }

    @Test
    void getRelationshipTypeGroup_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();
        });
        assertEquals("Relationship Type Group ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationshipTypeGroup_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();
        });
        assertEquals("Relationship Type Group ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationshipTypeGroup_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();
        });
        assertEquals("Invalid relationship type group ID format: not-a-number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getRelationshipTypeGroup_DirectResponseWithoutDataWrapper_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        // API returns relationship type group directly without data wrapper
        Map<String, Object> directResponse = relationshipTypeGroupBuilder()
            .id(1L)
            .name("Work")
            .build();

        when(monicaClient.get(eq("/relationshiptypegroups/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        verify(monicaClient).get(eq("/relationshiptypegroups/1"), isNull());
    }

    @Test
    void getRelationshipTypeGroup_ResponseFieldMapping_MapsSnakeCaseToCamelCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> groupData = new HashMap<>();
        groupData.put("id", 1L);
        groupData.put("name", "Love");
        groupData.put("created_at", "2023-01-01T00:00:00Z");
        groupData.put("updated_at", "2023-01-02T00:00:00Z");
        Map<String, Object> apiResponse = createSingleEntityResponse(groupData);

        when(monicaClient.get(eq("/relationshiptypegroups/1"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("Love", data.get("name"));
        assertEquals("2023-01-01T00:00:00Z", data.get("createdAt")); // created_at -> createdAt
        assertEquals("2023-01-02T00:00:00Z", data.get("updatedAt")); // updated_at -> updatedAt
    }

    @Test
    void getRelationshipTypeGroup_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();
        });
        assertEquals("Relationship Type Group ID is required", exception.getMessage());
    }

    @Test
    void getRelationshipTypeGroup_LargeId_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", Long.MAX_VALUE);

        Map<String, Object> groupData = relationshipTypeGroupBuilder()
            .id(Long.MAX_VALUE)
            .name("Custom Group")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(groupData);

        when(monicaClient.get(eq("/relationshiptypegroups/" + Long.MAX_VALUE), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups/" + Long.MAX_VALUE), isNull());
    }

    @Test
    void getRelationshipTypeGroup_FormatterInvoked_WithRawData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> groupData = relationshipTypeGroupBuilder()
            .id(1L)
            .name("Love")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(groupData);

        when(monicaClient.get(eq("/relationshiptypegroups/1"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted relationship type group JSON");

        // When
        relationshipTypeGroupService.getRelationshipTypeGroup(arguments).block();

        // Then
        verify(contentFormatter).formatAsEscapedJson(groupData);
    }

    // ========================================================================================
    // LIST RELATIONSHIP TYPE GROUPS TESTS
    // ========================================================================================

    @Test
    void listRelationshipTypeGroups_DefaultPagination_UsesDefaultValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build(),
            relationshipTypeGroupBuilder().id(2L).name("Family").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 2);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups"), argThat(params ->
            "1".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypeGroups_CustomPagination_UsesProvidedValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", 2);
        arguments.put("limit", 25);

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(3L).name("Friends").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 2, 25, 26);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups"), argThat(params ->
            "2".equals(params.get("page")) && "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypeGroups_StringPagination_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", "3");
        arguments.put("limit", "15");

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Work").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 3, 15, 31);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups"), argThat(params ->
            "3".equals(params.get("page")) && "15".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypeGroups_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of();
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 0);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listRelationshipTypeGroups_IncludesMetadata_WhenPresent() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build(),
            relationshipTypeGroupBuilder().id(2L).name("Family").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 100);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

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
    void listRelationshipTypeGroups_MultipleItems_MapsAllFields() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> group1 = new HashMap<>();
        group1.put("id", 1L);
        group1.put("name", "Love");
        group1.put("created_at", "2023-01-01T00:00:00Z");
        group1.put("updated_at", "2023-01-02T00:00:00Z");

        Map<String, Object> group2 = new HashMap<>();
        group2.put("id", 2L);
        group2.put("name", "Family");
        group2.put("created_at", "2023-02-01T00:00:00Z");
        group2.put("updated_at", "2023-02-02T00:00:00Z");

        List<Map<String, Object>> groups = List.of(group1, group2);
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 2);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
        assertEquals("Love", data.get(0).get("name"));
        assertEquals("2023-01-01T00:00:00Z", data.get(0).get("createdAt")); // created_at -> createdAt
        assertEquals("2023-01-02T00:00:00Z", data.get(0).get("updatedAt")); // updated_at -> updatedAt
        assertEquals("Family", data.get(1).get("name"));
    }

    @Test
    void listRelationshipTypeGroups_ContentFieldFormat_IsCorrect() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 1);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list content");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

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
    void listRelationshipTypeGroups_NullArgs_UsesDefaults() {
        // Given - null arguments should not throw, will use defaults
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 1);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups"), argThat(params ->
            "1".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listRelationshipTypeGroups_NoMetaResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build()
        );
        Map<String, Object> apiResponse = new HashMap<>();
        apiResponse.put("data", groups);
        // No meta key

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listRelationshipTypeGroups_FormatterInvoked_WithApiResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 1, 10, 1);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(apiResponse);
    }

    @Test
    void listRelationshipTypeGroups_IntegerPaginationValues_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", 5);
        arguments.put("limit", 50);

        List<Map<String, Object>> groups = List.of(
            relationshipTypeGroupBuilder().id(1L).name("Love").build()
        );
        Map<String, Object> apiResponse = createListResponse(groups, 5, 50, 201);

        when(monicaClient.get(eq("/relationshiptypegroups"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted relationship type groups JSON");

        // When
        Map<String, Object> result = relationshipTypeGroupService.listRelationshipTypeGroups(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/relationshiptypegroups"), argThat(params ->
            "5".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    // ========================================================================================
    // RELATIONSHIP TYPE GROUP DATA BUILDER
    // ========================================================================================

    /**
     * Builder for creating mock relationship type group data.
     */
    protected RelationshipTypeGroupDataBuilder relationshipTypeGroupBuilder() {
        return new RelationshipTypeGroupDataBuilder();
    }

    public static class RelationshipTypeGroupDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public RelationshipTypeGroupDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "Love");
        }

        public RelationshipTypeGroupDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public RelationshipTypeGroupDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public RelationshipTypeGroupDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
