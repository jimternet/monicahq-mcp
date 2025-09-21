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
public class DebtService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createDebt(Map<String, Object> arguments) {
        log.info("Creating debt with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateDebtCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/debts", apiRequest)
                .map(this::formatDebtResponse)
                .doOnSuccess(result -> log.info("Debt created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create debt: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for debt creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getDebt(Map<String, Object> arguments) {
        log.info("Getting debt with arguments: {}", arguments);
        
        try {
            Long debtId = extractDebtId(arguments);
            
            return monicaClient.get("/debts/" + debtId, null)
                .map(this::formatDebtResponse)
                .doOnSuccess(result -> log.info("Debt retrieved successfully: {}", debtId))
                .doOnError(error -> log.error("Failed to get debt {}: {}", debtId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for debt retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateDebt(Map<String, Object> arguments) {
        log.info("Updating debt with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long debtId = extractDebtId(mutableArguments);
            validateDebtUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/debts/" + debtId, apiRequest)
                .map(this::formatDebtResponse)
                .doOnSuccess(result -> log.info("Debt updated successfully: {}", debtId))
                .doOnError(error -> log.error("Failed to update debt {}: {}", debtId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for debt update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteDebt(Map<String, Object> arguments) {
        log.info("Deleting debt with arguments: {}", arguments);
        
        try {
            Long debtId = extractDebtId(arguments);
            
            return monicaClient.delete("/debts/" + debtId)
                .map(response -> {
                    String deletionMessage = String.format("Debt %d deleted successfully", debtId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", debtId)
                    );
                })
                .doOnSuccess(result -> log.info("Debt deleted successfully: {}", debtId))
                .doOnError(error -> log.error("Failed to delete debt {}: {}", debtId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for debt deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listDebts(Map<String, Object> arguments) {
        log.info("Listing debts with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/debts", queryParams)
                .map(this::formatDebtsListResponse)
                .doOnSuccess(result -> log.info("Debts listed successfully"))
                .doOnError(error -> log.error("Failed to list debts: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing debts: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateDebtCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        if (!arguments.containsKey("amount") || arguments.get("amount") == null) {
            throw new IllegalArgumentException("amount is required");
        }
    }

    private void validateDebtUpdateArguments(Map<String, Object> arguments) {
        // For updates, fields are optional
    }

    private Long extractDebtId(Map<String, Object> arguments) {
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
        if (arguments.containsKey("amount")) {
            apiRequest.put("amount", arguments.get("amount"));
        }
        if (arguments.containsKey("currency")) {
            apiRequest.put("currency", arguments.get("currency"));
        }
        if (arguments.containsKey("inDebt")) {
            apiRequest.put("in_debt", arguments.get("inDebt"));
        }
        if (arguments.containsKey("status")) {
            apiRequest.put("status", arguments.get("status"));
        }
        if (arguments.containsKey("reason")) {
            apiRequest.put("reason", arguments.get("reason"));
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

    private Map<String, Object> formatDebtResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> debtData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            debtData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            debtData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", debtData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatDebtsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> debts = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedDebts = debts.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedDebts);
        
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
                case "in_debt" -> result.put("inDebt", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}