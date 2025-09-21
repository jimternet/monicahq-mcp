package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for discovering available gender options from MonicaHQ API.
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GenderService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    /**
     * Lists all available genders from MonicaHQ API.
     * This discovery tool eliminates the need to hardcode gender values.
     */
    public Mono<Map<String, Object>> listGenders(Map<String, Object> arguments) {
        log.info("Listing available genders");
        
        return monicaClient.get("/genders", null)
            .map(this::formatGendersResponse)
            .doOnSuccess(result -> log.info("Genders listed successfully"))
            .doOnError(error -> log.error("Failed to list genders: {}", error.getMessage()));
    }

    private Map<String, Object> formatGendersResponse(Map<String, Object> apiResponse) {
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