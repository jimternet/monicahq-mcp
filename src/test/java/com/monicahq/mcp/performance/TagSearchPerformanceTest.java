package com.monicahq.mcp.performance;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.TagService;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Performance tests for tag-based search operations to ensure <500ms response times.
 */
@ExtendWith(MockitoExtension.class)
class TagSearchPerformanceTest {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private TagService tagService;

    private Map<String, Object> mockContactsByTagResponse;

    @BeforeEach
    void setUp() {
        tagService = new TagService(monicaClient, contentFormatter);
        
        // Mock contacts by tag response
        mockContactsByTagResponse = Map.of(
            "data", List.of(
                Map.of("id", 1L, "first_name", "John", "last_name", "Doe"),
                Map.of("id", 2L, "first_name", "Jane", "last_name", "Smith")
            ),
            "meta", Map.of("total", 2, "current_page", 1, "per_page", 10)
        );

        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");
    }

    @Test
    void contactsByTag_ShouldCompleteWithin500ms() {
        // Given
        Map<String, Object> arguments = Map.of(
            "id", 1L,
            "limit", 20
        );
        
        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(mockContactsByTagResponse));

        // When & Then
        long startTime = System.currentTimeMillis();
        
        Map<String, Object> result = tagService.listContactsByTag(arguments).block();
        
        long executionTime = System.currentTimeMillis() - startTime;
        
        assertTrue(executionTime < 500, 
            String.format("Contacts by tag search took %dms, should be under 500ms", executionTime));
        
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
    }

    @Test
    void contactsByTag_LargeResultSet_ShouldCompleteWithin500ms() {
        // Given - Simulate larger result set (50 contacts per tag)
        List<Map<String, Object>> largeContactList = generateMockContacts(50);
        Map<String, Object> largeResponse = Map.of(
            "data", largeContactList,
            "meta", Map.of("total", 50, "current_page", 1, "per_page", 50)
        );
        
        Map<String, Object> arguments = Map.of(
            "id", 2L,
            "limit", 50
        );
        
        when(monicaClient.get(eq("/tags/2/contacts"), any())).thenReturn(Mono.just(largeResponse));

        // When & Then
        long startTime = System.currentTimeMillis();
        
        Map<String, Object> result = tagService.listContactsByTag(arguments).block();
        
        long executionTime = System.currentTimeMillis() - startTime;
        
        assertTrue(executionTime < 500, 
            String.format("Large contacts by tag search took %dms, should be under 500ms", executionTime));
        
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size()); // Formatted as single text block
    }

    @Test
    void contactsByTag_MultipleTagsConcurrent_ShouldHandleLoad() {
        // Given
        Map<String, Object> arguments1 = Map.of("id", 1L, "limit", 10);
        Map<String, Object> arguments2 = Map.of("id", 2L, "limit", 10);
        Map<String, Object> arguments3 = Map.of("id", 3L, "limit", 10);
        
        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(mockContactsByTagResponse));
        when(monicaClient.get(eq("/tags/2/contacts"), any())).thenReturn(Mono.just(mockContactsByTagResponse));
        when(monicaClient.get(eq("/tags/3/contacts"), any())).thenReturn(Mono.just(mockContactsByTagResponse));

        // When - Simulate concurrent requests for different tags
        long startTime = System.currentTimeMillis();
        
        Mono<Map<String, Object>> request1 = tagService.listContactsByTag(arguments1);
        Mono<Map<String, Object>> request2 = tagService.listContactsByTag(arguments2);
        Mono<Map<String, Object>> request3 = tagService.listContactsByTag(arguments3);
        
        // Execute all requests concurrently
        List<Map<String, Object>> results = Mono.zip(request1, request2, request3)
            .map(tuple -> List.of(tuple.getT1(), tuple.getT2(), tuple.getT3()))
            .block();
        
        long executionTime = System.currentTimeMillis() - startTime;
        
        // Then
        assertTrue(executionTime < 1000, 
            String.format("Concurrent tag searches took %dms, should be under 1000ms", executionTime));
        
        assertNotNull(results);
        assertEquals(3, results.size());
        results.forEach(result -> {
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
        });
    }

    /**
     * Helper method to generate mock contact data for performance testing.
     */
    private List<Map<String, Object>> generateMockContacts(int count) {
        return java.util.stream.IntStream.range(1, count + 1)
            .mapToObj(i -> Map.<String, Object>of(
                "id", (long) i,
                "first_name", "Contact" + i,
                "last_name", "Test" + i,
                "email", "contact" + i + "@example.com"
            ))
            .toList();
    }
}