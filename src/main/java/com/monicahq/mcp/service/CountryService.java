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
public class CountryService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> getCountry(Map<String, Object> arguments) {
        log.info("Getting country with arguments: {}", arguments);
        
        try {
            Long countryId = extractCountryId(arguments);
            
            return monicaClient.get("/countries/" + countryId, null)
                .map(this::formatCountryResponse)
                .doOnSuccess(result -> log.info("Country retrieved successfully: {}", countryId))
                .doOnError(error -> log.error("Failed to get country {}: {}", countryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for country retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listCountries(Map<String, Object> arguments) {
        log.info("Listing countries with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/countries", queryParams)
                .map(this::formatCountriesListResponse)
                .doOnSuccess(result -> log.info("Countries listed successfully"))
                .doOnError(error -> log.error("Failed to list countries: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing countries: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> searchCountries(Map<String, Object> arguments) {
        log.info("Searching countries with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildSearchQueryParams(arguments);
            
            return monicaClient.get("/countries", queryParams)
                .map(this::formatCountriesListResponse)
                .doOnSuccess(result -> log.info("Countries searched successfully"))
                .doOnError(error -> log.error("Failed to search countries: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error searching countries: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private Long extractCountryId(Map<String, Object> arguments) {
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

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        if (arguments.containsKey("limit")) {
            queryParams.put("limit", arguments.get("limit").toString());
        } else {
            queryParams.put("limit", "50");
        }
        
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }
        
        return queryParams;
    }

    private Map<String, String> buildSearchQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = buildListQueryParams(arguments);
        
        if (arguments.containsKey("search")) {
            queryParams.put("search", arguments.get("search").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatCountryResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> countryData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            countryData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            countryData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", countryData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatCountriesListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> countries = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedCountries = countries.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedCountries);
        
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
                case "country_code" -> result.put("countryCode", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}