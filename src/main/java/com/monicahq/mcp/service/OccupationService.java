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
public class OccupationService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createOccupation(Map<String, Object> arguments) {
        log.info("Creating occupation with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateOccupationCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/occupations", apiRequest)
                .map(this::formatOccupationResponse)
                .doOnSuccess(result -> log.info("Occupation created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create occupation: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for occupation creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getOccupation(Map<String, Object> arguments) {
        log.info("Getting occupation with arguments: {}", arguments);
        
        try {
            Long occupationId = extractOccupationId(arguments);
            
            return monicaClient.get("/occupations/" + occupationId, null)
                .map(this::formatOccupationResponse)
                .doOnSuccess(result -> log.info("Occupation retrieved successfully: {}", occupationId))
                .doOnError(error -> log.error("Failed to get occupation {}: {}", occupationId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for occupation retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateOccupation(Map<String, Object> arguments) {
        log.info("Updating occupation with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long occupationId = extractOccupationId(mutableArguments);
            validateOccupationUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/occupations/" + occupationId, apiRequest)
                .map(this::formatOccupationResponse)
                .doOnSuccess(result -> log.info("Occupation updated successfully: {}", occupationId))
                .doOnError(error -> log.error("Failed to update occupation {}: {}", occupationId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for occupation update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteOccupation(Map<String, Object> arguments) {
        log.info("Deleting occupation with arguments: {}", arguments);
        
        try {
            Long occupationId = extractOccupationId(arguments);
            
            return monicaClient.delete("/occupations/" + occupationId)
                .map(response -> {
                    String deletionMessage = String.format("Occupation %d deleted successfully", occupationId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", occupationId)
                    );
                })
                .doOnSuccess(result -> log.info("Occupation deleted successfully: {}", occupationId))
                .doOnError(error -> log.error("Failed to delete occupation {}: {}", occupationId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for occupation deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listOccupations(Map<String, Object> arguments) {
        log.info("Listing occupations with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/occupations", queryParams)
                .map(this::formatOccupationsListResponse)
                .doOnSuccess(result -> log.info("Occupations listed successfully"))
                .doOnError(error -> log.error("Failed to list occupations: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing occupations: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateOccupationCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        if (!arguments.containsKey("title") || arguments.get("title") == null || 
            arguments.get("title").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("title is required");
        }
    }

    private void validateOccupationUpdateArguments(Map<String, Object> arguments) {
        if (arguments.containsKey("title") && (arguments.get("title") == null || 
            arguments.get("title").toString().trim().isEmpty())) {
            throw new IllegalArgumentException("title cannot be empty");
        }
    }

    private Long extractOccupationId(Map<String, Object> arguments) {
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
        if (arguments.containsKey("companyId")) {
            apiRequest.put("company_id", arguments.get("companyId"));
        }
        if (arguments.containsKey("title")) {
            apiRequest.put("title", arguments.get("title"));
        }
        if (arguments.containsKey("description")) {
            apiRequest.put("description", arguments.get("description"));
        }
        if (arguments.containsKey("salary")) {
            apiRequest.put("salary", arguments.get("salary"));
        }
        if (arguments.containsKey("salaryUnit")) {
            apiRequest.put("salary_unit", arguments.get("salaryUnit"));
        }
        if (arguments.containsKey("currentlyWorksHere")) {
            apiRequest.put("currently_works_here", arguments.get("currentlyWorksHere"));
        }
        if (arguments.containsKey("startDate")) {
            apiRequest.put("start_date", arguments.get("startDate"));
        }
        if (arguments.containsKey("endDate")) {
            apiRequest.put("end_date", arguments.get("endDate"));
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

    private Map<String, Object> formatOccupationResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> occupationData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            occupationData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            occupationData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", occupationData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatOccupationsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> occupations = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedOccupations = occupations.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedOccupations);
        
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
                case "company_id" -> result.put("companyId", value);
                case "salary_unit" -> result.put("salaryUnit", value);
                case "currently_works_here" -> result.put("currentlyWorksHere", value);
                case "start_date" -> result.put("startDate", value);
                case "end_date" -> result.put("endDate", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}