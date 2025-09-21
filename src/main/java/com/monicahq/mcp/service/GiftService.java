package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class GiftService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createGift(Map<String, Object> arguments) {
        log.info("Creating gift with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateGiftCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/gifts", apiRequest)
                .map(this::formatGiftResponse)
                .doOnSuccess(result -> log.info("Gift created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create gift: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for gift creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getGift(Map<String, Object> arguments) {
        log.info("Getting gift with arguments: {}", arguments);
        
        try {
            Long giftId = extractGiftId(arguments);
            
            return monicaClient.get("/gifts/" + giftId, null)
                .map(this::formatGiftResponse)
                .doOnSuccess(result -> log.info("Gift retrieved successfully: {}", giftId))
                .doOnError(error -> log.error("Failed to get gift {}: {}", giftId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for gift retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateGift(Map<String, Object> arguments) {
        log.info("Updating gift with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long giftId = extractGiftId(mutableArguments);
            validateGiftUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/gifts/" + giftId, apiRequest)
                .map(this::formatGiftResponse)
                .doOnSuccess(result -> log.info("Gift updated successfully: {}", giftId))
                .doOnError(error -> log.error("Failed to update gift {}: {}", giftId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for gift update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteGift(Map<String, Object> arguments) {
        log.info("Deleting gift with arguments: {}", arguments);
        
        try {
            Long giftId = extractGiftId(arguments);
            
            return monicaClient.delete("/gifts/" + giftId)
                .map(response -> {
                    String deletionMessage = String.format("Gift %d deleted successfully", giftId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", giftId)
                    );
                })
                .doOnSuccess(result -> log.info("Gift deleted successfully: {}", giftId))
                .doOnError(error -> log.error("Failed to delete gift {}: {}", giftId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for gift deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listGifts(Map<String, Object> arguments) {
        log.info("Listing gifts with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/gifts", queryParams)
                .map(this::formatGiftsListResponse)
                .doOnSuccess(result -> log.info("Gifts listed successfully"))
                .doOnError(error -> log.error("Failed to list gifts: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing gifts: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateGiftCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        if (!arguments.containsKey("name") || arguments.get("name") == null || 
            arguments.get("name").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private void validateGiftUpdateArguments(Map<String, Object> arguments) {
        // For updates, fields are optional
    }

    private Long extractGiftId(Map<String, Object> arguments) {
        Object idObj = arguments.get("id");
        if (idObj == null) {
            throw new IllegalArgumentException("id is required");
        }
        
        if (idObj instanceof Number) {
            return ((Number) idObj).longValue();
        }
        
        try {
            return Long.parseLong(idObj.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("id must be a valid number");
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        if (arguments.containsKey("contactId")) {
            apiRequest.put("contact_id", arguments.get("contactId"));
        }
        if (arguments.containsKey("name")) {
            apiRequest.put("name", arguments.get("name"));
        }
        if (arguments.containsKey("comment")) {
            apiRequest.put("comment", arguments.get("comment"));
        }
        if (arguments.containsKey("url")) {
            apiRequest.put("url", arguments.get("url"));
        }
        if (arguments.containsKey("value")) {
            apiRequest.put("value", arguments.get("value"));
        }
        if (arguments.containsKey("status")) {
            apiRequest.put("status", arguments.get("status"));
        }
        if (arguments.containsKey("date")) {
            apiRequest.put("date", arguments.get("date"));
        }
        if (arguments.containsKey("isFor")) {
            apiRequest.put("is_for", arguments.get("isFor"));
        }
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        if (arguments.containsKey("limit")) {
            queryParams.put("limit", arguments.get("limit").toString());
        } else {
            queryParams.put("limit", "10");
        }
        
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }
        
        return queryParams;
    }

    private Map<String, Object> formatGiftResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> giftData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            giftData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            giftData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", giftData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatGiftsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> gifts = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedGifts = gifts.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedGifts);
        
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        apiData.forEach((key, value) -> {
            switch (key) {
                case "contact_id" -> result.put("contactId", value);
                case "value_in_base_currency" -> result.put("valueInBaseCurrency", value);
                case "is_for" -> result.put("isFor", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}