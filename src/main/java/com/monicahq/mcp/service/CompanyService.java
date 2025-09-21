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
public class CompanyService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createCompany(Map<String, Object> arguments) {
        log.info("Creating company with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateCompanyCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/companies", apiRequest)
                .map(this::formatCompanyResponse)
                .doOnSuccess(result -> log.info("Company created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create company: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for company creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getCompany(Map<String, Object> arguments) {
        log.info("Getting company with arguments: {}", arguments);
        
        try {
            Long companyId = extractCompanyId(arguments);
            
            return monicaClient.get("/companies/" + companyId, null)
                .map(this::formatCompanyResponse)
                .doOnSuccess(result -> log.info("Company retrieved successfully: {}", companyId))
                .doOnError(error -> log.error("Failed to get company {}: {}", companyId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for company retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateCompany(Map<String, Object> arguments) {
        log.info("Updating company with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long companyId = extractCompanyId(mutableArguments);
            validateCompanyUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/companies/" + companyId, apiRequest)
                .map(this::formatCompanyResponse)
                .doOnSuccess(result -> log.info("Company updated successfully: {}", companyId))
                .doOnError(error -> log.error("Failed to update company {}: {}", companyId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for company update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteCompany(Map<String, Object> arguments) {
        log.info("Deleting company with arguments: {}", arguments);
        
        try {
            Long companyId = extractCompanyId(arguments);
            
            return monicaClient.delete("/companies/" + companyId)
                .map(response -> {
                    String deletionMessage = String.format("Company %d deleted successfully", companyId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", companyId)
                    );
                })
                .doOnSuccess(result -> log.info("Company deleted successfully: {}", companyId))
                .doOnError(error -> log.error("Failed to delete company {}: {}", companyId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for company deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listCompanies(Map<String, Object> arguments) {
        log.info("Listing companies with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/companies", queryParams)
                .map(this::formatCompaniesListResponse)
                .doOnSuccess(result -> log.info("Companies listed successfully"))
                .doOnError(error -> log.error("Failed to list companies: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing companies: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateCompanyCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("name") || arguments.get("name") == null || 
            arguments.get("name").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private void validateCompanyUpdateArguments(Map<String, Object> arguments) {
        validateCompanyCreateArguments(arguments);
    }

    private Long extractCompanyId(Map<String, Object> arguments) {
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
        
        if (arguments.containsKey("name")) {
            apiRequest.put("name", arguments.get("name"));
        }
        if (arguments.containsKey("website")) {
            apiRequest.put("website", arguments.get("website"));
        }
        if (arguments.containsKey("numberOfEmployees")) {
            apiRequest.put("number_of_employees", arguments.get("numberOfEmployees"));
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

    private Map<String, Object> formatCompanyResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> companyData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            companyData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            companyData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", companyData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatCompaniesListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> companies = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedCompanies = companies.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedCompanies);
        
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
                case "number_of_employees" -> result.put("numberOfEmployees", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}