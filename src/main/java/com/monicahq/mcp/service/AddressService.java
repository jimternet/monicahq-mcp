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
public class AddressService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createAddress(Map<String, Object> arguments) {
        log.info("Creating address with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateAddressCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/addresses", apiRequest)
                .map(this::formatAddressResponse)
                .doOnSuccess(result -> log.info("Address created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create address: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for address creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getAddress(Map<String, Object> arguments) {
        log.info("Getting address with arguments: {}", arguments);
        
        try {
            Long addressId = extractAddressId(arguments);
            
            return monicaClient.get("/addresses/" + addressId, null)
                .map(this::formatAddressResponse)
                .doOnSuccess(result -> log.info("Address retrieved successfully: {}", addressId))
                .doOnError(error -> log.error("Failed to get address {}: {}", addressId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for address retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateAddress(Map<String, Object> arguments) {
        log.info("Updating address with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long addressId = extractAddressId(mutableArguments);
            validateAddressUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/addresses/" + addressId, apiRequest)
                .map(this::formatAddressResponse)
                .doOnSuccess(result -> log.info("Address updated successfully: {}", addressId))
                .doOnError(error -> log.error("Failed to update address {}: {}", addressId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for address update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteAddress(Map<String, Object> arguments) {
        log.info("Deleting address with arguments: {}", arguments);
        
        try {
            Long addressId = extractAddressId(arguments);
            
            return monicaClient.delete("/addresses/" + addressId)
                .map(response -> {
                    String deletionMessage = String.format("Address %d deleted successfully", addressId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", addressId)
                    );
                })
                .doOnSuccess(result -> log.info("Address deleted successfully: {}", addressId))
                .doOnError(error -> log.error("Failed to delete address {}: {}", addressId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for address deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listAddresses(Map<String, Object> arguments) {
        log.info("Listing addresses with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/addresses", queryParams)
                .map(this::formatAddressesListResponse)
                .doOnSuccess(result -> log.info("Addresses listed successfully"))
                .doOnError(error -> log.error("Failed to list addresses: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing addresses: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateAddressCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
    }

    private void validateAddressUpdateArguments(Map<String, Object> arguments) {
        // For updates, we just need the ID, other fields are optional
    }

    private Long extractAddressId(Map<String, Object> arguments) {
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
        if (arguments.containsKey("street")) {
            apiRequest.put("street", arguments.get("street"));
        }
        if (arguments.containsKey("city")) {
            apiRequest.put("city", arguments.get("city"));
        }
        if (arguments.containsKey("province")) {
            apiRequest.put("province", arguments.get("province"));
        }
        if (arguments.containsKey("postalCode")) {
            apiRequest.put("postal_code", arguments.get("postalCode"));
        }
        if (arguments.containsKey("country")) {
            apiRequest.put("country", arguments.get("country"));
        }
        if (arguments.containsKey("latitude")) {
            apiRequest.put("latitude", arguments.get("latitude"));
        }
        if (arguments.containsKey("longitude")) {
            apiRequest.put("longitude", arguments.get("longitude"));
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

    private Map<String, Object> formatAddressResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> addressData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            addressData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            addressData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", addressData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatAddressesListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> addresses = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedAddresses = addresses.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedAddresses);
        
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
                case "postal_code" -> result.put("postalCode", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}