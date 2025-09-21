package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for discovering available contact field types from MonicaHQ API.
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ContactFieldTypeService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    /**
     * Lists all available contact field types from MonicaHQ API.
     * This discovery tool shows valid contactFieldTypeId values for contact field creation.
     */
    public Mono<Map<String, Object>> listContactFieldTypes(Map<String, Object> arguments) {
        log.info("Listing available contact field types");
        
        return monicaClient.get("/contactfieldtypes", null)
            .map(this::formatContactFieldTypesResponse)
            .doOnSuccess(result -> log.info("Contact field types listed successfully"))
            .doOnError(error -> log.error("Failed to list contact field types: {}", error.getMessage()));
    }

    private Map<String, Object> formatContactFieldTypesResponse(Map<String, Object> apiResponse) {
        // Format content as escaped JSON for Claude Desktop accessibility as per Constitutional Principle VI
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", apiResponse.get("data"));
        
        // Extract and preserve meta from API response
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        // Add content field for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }
}