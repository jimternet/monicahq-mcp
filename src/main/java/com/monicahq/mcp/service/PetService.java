package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing pets associated with contacts in MonicaHQ.
 *
 * <p>This service uses root-level POST endpoint (/pets) instead of nested endpoint
 * (/contacts/{id}/pets) to avoid HTTP 405 errors. The contact_id is passed in the
 * request body as required by the MonicaHQ API.</p>
 *
 * @see <a href="docs/API-LIMITATIONS.md">API Limitations documentation</a>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class PetService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    /**
     * Creates a new pet for a contact.
     *
     * <p>Uses POST /pets with contact_id in the request body (not the nested
     * /contacts/{id}/pets endpoint which returns HTTP 405).</p>
     *
     * @param arguments Map containing contactId, petCategoryId, and optionally name
     * @return Mono containing the created pet data
     */
    public Mono<Map<String, Object>> createPet(Map<String, Object> arguments) {
        log.info("Creating pet with arguments: {}", arguments);

        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validatePetCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);

            // Use root-level POST /pets endpoint to avoid HTTP 405 error
            return monicaClient.post("/pets", apiRequest)
                .map(this::formatPetResponse)
                .doOnSuccess(result -> log.info("Pet created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create pet: {}", error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for pet creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getPet(Map<String, Object> arguments) {
        log.info("Getting pet with arguments: {}", arguments);

        try {
            Long petId = extractPetId(arguments);

            return monicaClient.get("/pets/" + petId, null)
                .map(this::formatPetResponse)
                .doOnSuccess(result -> log.info("Pet retrieved successfully: {}", petId))
                .doOnError(error -> log.error("Failed to get pet {}: {}", petId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for pet retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updatePet(Map<String, Object> arguments) {
        log.info("Updating pet with arguments: {}", arguments);

        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long petId = extractPetId(mutableArguments);
            validatePetUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);

            return monicaClient.put("/pets/" + petId, apiRequest)
                .map(this::formatPetResponse)
                .doOnSuccess(result -> log.info("Pet updated successfully: {}", petId))
                .doOnError(error -> log.error("Failed to update pet {}: {}", petId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for pet update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deletePet(Map<String, Object> arguments) {
        log.info("Deleting pet with arguments: {}", arguments);

        try {
            Long petId = extractPetId(arguments);

            return monicaClient.delete("/pets/" + petId)
                .map(response -> {
                    String deletionMessage = String.format("Pet %d deleted successfully", petId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", petId)
                    );
                })
                .doOnSuccess(result -> log.info("Pet deleted successfully: {}", petId))
                .doOnError(error -> log.error("Failed to delete pet {}: {}", petId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for pet deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listPets(Map<String, Object> arguments) {
        log.info("Listing pets with arguments: {}", arguments);

        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);

            return monicaClient.get("/pets", queryParams)
                .map(this::formatPetsListResponse)
                .doOnSuccess(result -> log.info("Pets listed successfully"))
                .doOnError(error -> log.error("Failed to list pets: {}", error.getMessage()));

        } catch (Exception e) {
            log.error("Error listing pets: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validatePetCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        if (!arguments.containsKey("petCategoryId") || arguments.get("petCategoryId") == null) {
            throw new IllegalArgumentException("petCategoryId is required");
        }
    }

    private void validatePetUpdateArguments(Map<String, Object> arguments) {
        // For updates, petCategoryId is required by the API
        if (!arguments.containsKey("petCategoryId") || arguments.get("petCategoryId") == null) {
            throw new IllegalArgumentException("petCategoryId is required for update");
        }
    }

    private Long extractPetId(Map<String, Object> arguments) {
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

        // Map contactId to contact_id (required for create)
        if (arguments.containsKey("contactId")) {
            apiRequest.put("contact_id", arguments.get("contactId"));
        }

        // Map petCategoryId to pet_category_id (required)
        if (arguments.containsKey("petCategoryId")) {
            apiRequest.put("pet_category_id", arguments.get("petCategoryId"));
        }

        // Map optional name field
        if (arguments.containsKey("name")) {
            apiRequest.put("name", arguments.get("name"));
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

    private Map<String, Object> formatPetResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> petData;

        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            petData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            petData = mapFromApiFormat(apiResponse);
        }

        // Use raw API data as escaped JSON for complete field coverage
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);

        Map<String, Object> result = new HashMap<>();
        result.put("data", petData);

        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }

    private Map<String, Object> formatPetsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> pets = (List<Map<String, Object>>) apiResponse.get("data");

        List<Map<String, Object>> formattedPets = pets.stream()
            .map(this::mapFromApiFormat)
            .toList();

        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);

        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedPets);

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

        // Map snake_case to camelCase
        apiData.forEach((key, value) -> {
            switch (key) {
                case "contact_id" -> result.put("contactId", value);
                case "pet_category_id" -> result.put("petCategoryId", value);
                case "pet_category" -> result.put("petCategory", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });

        return result;
    }
}
