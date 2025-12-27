package com.monicahq.mcp.service.base;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Abstract base class that implements common CRUD patterns for Monica API services.
 * <p>
 * This class eliminates duplicated boilerplate code across 34+ service classes by providing
 * standardized implementations of:
 * <ul>
 *   <li>ID extraction and validation ({@link #extractId(Map)})</li>
 *   <li>Query parameter building for list operations ({@link #buildListQueryParams(Map)})</li>
 *   <li>Response formatting for single entities ({@link #formatSingleResponse(Map)})</li>
 *   <li>Response formatting for lists ({@link #formatListResponse(Map)})</li>
 *   <li>Delete response formatting ({@link #formatDeleteResponse(Long)})</li>
 *   <li>Field mapping between camelCase and snake_case ({@link #mapToApiFormat(Map)}, {@link #mapFromApiFormat(Map)})</li>
 *   <li>Required field validation ({@link #validateRequiredFields(Map, Set)})</li>
 * </ul>
 * </p>
 * <p>
 * Subclasses must implement {@link #getFieldMappingConfig()} to provide entity-specific
 * configuration. The Template Method pattern allows services to override specific methods
 * when custom behavior is needed while inheriting standard implementations for common operations.
 * </p>
 *
 * @param <T> The entity type this service manages (used for type safety in subclasses)
 * @see CrudOperations
 * @see FieldMappingConfig
 */
@Slf4j
public abstract class AbstractCrudService<T> implements CrudOperations {

    protected final MonicaHqClient monicaClient;
    protected final ContentFormatter contentFormatter;

    /**
     * Constructs an AbstractCrudService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     */
    protected AbstractCrudService(MonicaHqClient monicaClient, ContentFormatter contentFormatter) {
        this.monicaClient = monicaClient;
        this.contentFormatter = contentFormatter;
    }

    /**
     * Returns the field mapping configuration for this entity.
     * <p>
     * Subclasses must implement this method to provide entity-specific field mappings,
     * validation rules, and API endpoint paths.
     * </p>
     *
     * @return the field mapping configuration for this entity type
     */
    protected abstract FieldMappingConfig getFieldMappingConfig();

    // ========================================================================================
    // STANDARD CRUD OPERATIONS
    // ========================================================================================

    /**
     * Creates a new entity using the provided arguments.
     * <p>
     * This method validates required fields, maps field names to API format,
     * applies default values, and formats the response for Claude Desktop visibility.
     * </p>
     *
     * @param arguments the creation arguments containing entity data
     * @return a Mono containing the formatted response with the created entity
     */
    @Override
    public Mono<Map<String, Object>> create(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();
        log.info("Creating {} with arguments: {}", entityName, arguments);

        try {
            if (!config.supportsCreate()) {
                return Mono.error(new UnsupportedOperationException(
                    entityName + " does not support create operations"));
            }

            validateRequiredFields(arguments, config.getRequiredCreateFields());

            // Apply defaults before mapping to API format
            Map<String, Object> dataWithDefaults = applyDefaults(arguments, config.getCreateDefaults());
            Map<String, Object> apiRequest = mapToApiFormat(dataWithDefaults);

            return monicaClient.post(config.getEndpointPath(), apiRequest)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("{} created successfully", entityName))
                .doOnError(error -> log.error("Failed to create {}: {}", entityName, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for {} creation: {}", entityName, e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Retrieves a single entity by its ID.
     *
     * @param arguments the arguments containing the entity ID
     * @return a Mono containing the formatted response with the entity
     */
    @Override
    public Mono<Map<String, Object>> get(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();
        log.info("Getting {} with arguments: {}", entityName, arguments);

        try {
            Long id = extractId(arguments);
            String endpoint = config.getEndpointPath() + "/" + id;

            return monicaClient.get(endpoint, null)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("{} retrieved successfully: {}", entityName, id))
                .doOnError(error -> log.error("Failed to get {} {}: {}", entityName, id, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for {} retrieval: {}", entityName, e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Updates an existing entity.
     * <p>
     * The ID is extracted from arguments and removed before sending the update payload.
     * Field names are mapped to API format before the request.
     * </p>
     *
     * @param arguments the arguments containing entity ID and update data
     * @return a Mono containing the formatted response with the updated entity
     */
    @Override
    public Mono<Map<String, Object>> update(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();
        log.info("Updating {} with arguments: {}", entityName, arguments);

        try {
            if (!config.supportsUpdate()) {
                return Mono.error(new UnsupportedOperationException(
                    entityName + " does not support update operations"));
            }

            Long id = extractId(arguments);

            // Remove ID from update payload
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove(config.getIdFieldName());

            // Validate required update fields if any
            Set<String> requiredUpdateFields = config.getRequiredUpdateFields();
            if (!requiredUpdateFields.isEmpty()) {
                validateRequiredFields(updateData, requiredUpdateFields);
            }

            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            String endpoint = config.getEndpointPath() + "/" + id;

            return monicaClient.put(endpoint, apiRequest)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("{} updated successfully: {}", entityName, id))
                .doOnError(error -> log.error("Failed to update {} {}: {}", entityName, id, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for {} update: {}", entityName, e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Deletes an entity by its ID.
     *
     * @param arguments the arguments containing the entity ID
     * @return a Mono containing the formatted delete confirmation response
     */
    @Override
    public Mono<Map<String, Object>> delete(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();
        log.info("Deleting {} with arguments: {}", entityName, arguments);

        try {
            if (!config.supportsDelete()) {
                return Mono.error(new UnsupportedOperationException(
                    entityName + " does not support delete operations"));
            }

            Long id = extractId(arguments);
            String endpoint = config.getEndpointPath() + "/" + id;

            return monicaClient.delete(endpoint)
                .map(response -> formatDeleteResponse(id))
                .doOnSuccess(result -> log.info("{} deleted successfully: {}", entityName, id))
                .doOnError(error -> log.error("Failed to delete {} {}: {}", entityName, id, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for {} deletion: {}", entityName, e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Lists entities with optional filtering and pagination.
     *
     * @param arguments the arguments containing optional filters and pagination params
     * @return a Mono containing the formatted list response with entities and metadata
     */
    @Override
    public Mono<Map<String, Object>> list(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();
        log.info("Listing {}s with arguments: {}", entityName, arguments);

        try {
            if (!config.supportsList()) {
                return Mono.error(new UnsupportedOperationException(
                    entityName + " does not support list operations"));
            }

            Map<String, String> queryParams = buildListQueryParams(arguments);

            return monicaClient.get(config.getEndpointPath(), queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("{}s listed successfully", entityName))
                .doOnError(error -> log.error("Failed to list {}s: {}", entityName, error.getMessage()));

        } catch (Exception e) {
            log.error("Error building query parameters for {} list: {}", entityName, e.getMessage());
            return Mono.error(e);
        }
    }

    // ========================================================================================
    // ID EXTRACTION
    // ========================================================================================

    /**
     * Extracts and validates the entity ID from the provided arguments.
     * <p>
     * Supports both numeric IDs and string representations of numbers.
     * Uses the ID field name specified in the field mapping config.
     * </p>
     *
     * @param arguments the arguments map that should contain the ID
     * @return the extracted ID as a Long
     * @throws IllegalArgumentException if ID is missing or has invalid format
     */
    protected Long extractId(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        String idFieldName = config.getIdFieldName();
        String entityName = config.getEntityName();

        if (arguments == null || !arguments.containsKey(idFieldName)) {
            throw new IllegalArgumentException(entityName + " ID is required");
        }

        Object idValue = arguments.get(idFieldName);
        if (idValue == null) {
            throw new IllegalArgumentException(entityName + " ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(
                "Invalid " + entityName.toLowerCase() + " ID format: " + idValue);
        }
    }

    // ========================================================================================
    // QUERY PARAMETER BUILDING
    // ========================================================================================

    /**
     * Builds query parameters for list operations from the provided arguments.
     * <p>
     * Handles pagination parameters (page, limit) with sensible defaults and validation.
     * Maps filter fields according to the query parameter mappings defined in the config.
     * </p>
     *
     * @param arguments the arguments containing filter and pagination values (can be null)
     * @return a map of query parameter names to their string values
     */
    protected Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        Map<String, String> queryParams = new HashMap<>();

        // Handle null arguments by using empty map
        Map<String, Object> args = arguments != null ? arguments : Map.of();

        // Handle pagination - page
        if (args.containsKey("page")) {
            queryParams.put("page", args.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }

        // Handle pagination - limit with bounds validation
        if (args.containsKey("limit")) {
            int limit = parseLimit(args.get("limit"));
            queryParams.put("limit", String.valueOf(limit));
        } else {
            queryParams.put("limit", "10");
        }

        // Handle entity-specific filter fields
        Map<String, String> queryParamMappings = config.getQueryParamMappings();
        List<String> filterFields = config.getListFilterFields();

        for (String filterField : filterFields) {
            if (args.containsKey(filterField) && args.get(filterField) != null) {
                String apiParamName = queryParamMappings.getOrDefault(filterField, filterField);
                queryParams.put(apiParamName, args.get(filterField).toString());
            }
        }

        return queryParams;
    }

    /**
     * Parses and validates the limit parameter for list queries.
     * Ensures the limit is between 1 and 100.
     *
     * @param limitValue the limit value to parse
     * @return a valid limit value within bounds
     */
    protected int parseLimit(Object limitValue) {
        try {
            int limit = Integer.parseInt(limitValue.toString());
            return Math.min(100, Math.max(1, limit));
        } catch (NumberFormatException e) {
            return 10; // default limit
        }
    }

    // ========================================================================================
    // RESPONSE FORMATTING
    // ========================================================================================

    /**
     * Formats a single entity response for Claude Desktop visibility.
     * <p>
     * Extracts the entity data from the API response, maps field names from snake_case
     * to camelCase, and formats the content as escaped JSON per Constitutional Principle VI.
     * </p>
     *
     * @param apiResponse the raw API response containing the entity data
     * @return a formatted response map with data and content fields
     */
    @SuppressWarnings("unchecked")
    protected Map<String, Object> formatSingleResponse(Map<String, Object> apiResponse) {
        Map<String, Object> entityData;
        Map<String, Object> rawApiData;

        if (apiResponse.containsKey("data")) {
            rawApiData = (Map<String, Object>) apiResponse.get("data");
            entityData = mapFromApiFormat(rawApiData);
        } else {
            rawApiData = apiResponse;
            entityData = mapFromApiFormat(apiResponse);
        }

        // Format content as escaped JSON per Constitutional Principle VI
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);

        Map<String, Object> result = new HashMap<>();
        result.put("data", entityData);

        // Format content for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }

    /**
     * Formats a list response for Claude Desktop visibility.
     * <p>
     * Extracts the entity list from the API response, maps field names for each entity,
     * preserves pagination metadata, and formats the content as escaped JSON.
     * </p>
     *
     * @param apiResponse the raw API response containing the list and metadata
     * @return a formatted response map with data, meta, and content fields
     */
    @SuppressWarnings("unchecked")
    protected Map<String, Object> formatListResponse(Map<String, Object> apiResponse) {
        List<Map<String, Object>> entities = (List<Map<String, Object>>) apiResponse.get("data");

        List<Map<String, Object>> formattedEntities = entities.stream()
            .map(this::mapFromApiFormat)
            .toList();

        // Format content for Claude Desktop visibility using raw API response
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);

        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedEntities);

        // Preserve pagination metadata
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

    /**
     * Formats a delete operation response for Claude Desktop visibility.
     * <p>
     * Creates a standardized success message using the ContentFormatter.
     * </p>
     *
     * @param entityId the ID of the deleted entity
     * @return a formatted response map with the delete confirmation content
     */
    protected Map<String, Object> formatDeleteResponse(Long entityId) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();

        String formattedContent = contentFormatter.formatOperationResult(
            "Delete", entityName, entityId, true,
            entityName + " with ID " + entityId + " has been deleted successfully"
        );

        Map<String, Object> result = new HashMap<>();
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }

    // ========================================================================================
    // FIELD MAPPING
    // ========================================================================================

    /**
     * Maps field names from camelCase (client) to snake_case (API) format.
     * <p>
     * Uses the mappings defined in the field mapping config. Fields not in the
     * mapping are passed through unchanged.
     * </p>
     *
     * @param arguments the input data with camelCase field names
     * @return a new map with snake_case field names for API consumption
     */
    protected Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        FieldMappingConfig config = getFieldMappingConfig();
        Map<String, String> mappings = config.getToApiMappings();
        Map<String, Object> apiRequest = new HashMap<>();

        arguments.forEach((key, value) -> {
            String apiKey = mappings.getOrDefault(key, key);
            apiRequest.put(apiKey, value);
        });

        return apiRequest;
    }

    /**
     * Maps field names from snake_case (API) to camelCase (client) format.
     * <p>
     * Uses the mappings defined in the field mapping config. Common timestamp
     * fields (created_at, updated_at) are always mapped. Fields not in the
     * mapping are passed through unchanged.
     * </p>
     *
     * @param apiData the API response data with snake_case field names
     * @return a new map with camelCase field names for client consumption
     */
    protected Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        FieldMappingConfig config = getFieldMappingConfig();
        Map<String, String> mappings = config.getFromApiMappings();
        Map<String, Object> result = new HashMap<>();

        apiData.forEach((key, value) -> {
            // Check configured mappings first
            String clientKey = mappings.getOrDefault(key, key);

            // Always map common timestamp fields even if not explicitly configured
            if ("created_at".equals(key) && !mappings.containsKey(key)) {
                clientKey = "createdAt";
            } else if ("updated_at".equals(key) && !mappings.containsKey(key)) {
                clientKey = "updatedAt";
            }

            result.put(clientKey, value);
        });

        return result;
    }

    // ========================================================================================
    // VALIDATION
    // ========================================================================================

    /**
     * Validates that all required fields are present in the arguments.
     *
     * @param arguments the arguments to validate
     * @param requiredFields the set of required field names
     * @throws IllegalArgumentException if any required field is missing or null
     */
    protected void validateRequiredFields(Map<String, Object> arguments, Set<String> requiredFields) {
        FieldMappingConfig config = getFieldMappingConfig();
        String entityName = config.getEntityName();

        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException(
                entityName + " arguments cannot be empty");
        }

        for (String field : requiredFields) {
            if (!arguments.containsKey(field) || arguments.get(field) == null) {
                throw new IllegalArgumentException(field + " is required");
            }
        }
    }

    /**
     * Validates that a field value is a non-empty string.
     *
     * @param arguments the arguments containing the field
     * @param fieldName the name of the field to validate
     * @throws IllegalArgumentException if the field is missing, null, or empty
     */
    protected void validateRequiredString(Map<String, Object> arguments, String fieldName) {
        if (!arguments.containsKey(fieldName) ||
            arguments.get(fieldName) == null ||
            arguments.get(fieldName).toString().trim().isEmpty()) {
            throw new IllegalArgumentException(fieldName + " is required");
        }
    }

    /**
     * Validates that a field value is a valid numeric ID.
     *
     * @param arguments the arguments containing the field
     * @param fieldName the name of the field to validate
     * @throws IllegalArgumentException if the field is not a valid number
     */
    protected void validateNumericId(Map<String, Object> arguments, String fieldName) {
        if (!arguments.containsKey(fieldName) || arguments.get(fieldName) == null) {
            throw new IllegalArgumentException(fieldName + " is required");
        }

        Object value = arguments.get(fieldName);
        if (!(value instanceof Number)) {
            try {
                Long.parseLong(value.toString().trim());
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(fieldName + " must be a valid number");
            }
        }
    }

    // ========================================================================================
    // UTILITY METHODS
    // ========================================================================================

    /**
     * Applies default values to arguments for fields that are not already present.
     *
     * @param arguments the original arguments
     * @param defaults the default values to apply
     * @return a new map with defaults applied
     */
    protected Map<String, Object> applyDefaults(Map<String, Object> arguments, Map<String, Object> defaults) {
        if (defaults == null || defaults.isEmpty()) {
            return arguments;
        }

        Map<String, Object> result = new HashMap<>(arguments);
        defaults.forEach((key, defaultValue) -> {
            if (!result.containsKey(key) || result.get(key) == null) {
                result.put(key, defaultValue);
            }
        });

        return result;
    }

    /**
     * Returns the entity name from the field mapping config.
     * Utility method for use in subclasses.
     *
     * @return the entity name
     */
    protected String getEntityName() {
        return getFieldMappingConfig().getEntityName();
    }

    /**
     * Returns the endpoint path from the field mapping config.
     * Utility method for use in subclasses.
     *
     * @return the API endpoint path
     */
    protected String getEndpointPath() {
        return getFieldMappingConfig().getEndpointPath();
    }
}
