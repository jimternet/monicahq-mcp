package com.monicahq.mcp.framework;

import com.monicahq.mcp.client.MonicaHqClient;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Mono;

import java.util.*;

@RequiredArgsConstructor
@Slf4j
public abstract class AbstractEntityPlugin implements EntityPlugin {

    protected final MonicaHqClient monicaClient;
    protected final Map<String, EntityOperationMetadata> operationMetadata = new HashMap<>();
    protected boolean initialized = false;

    @Override
    public Set<String> getSupportedOperations() {
        return Set.of("create", "get", "update", "delete", "list");
    }

    @Override
    public Mono<Map<String, Object>> executeOperation(String operation, Map<String, Object> arguments) {
        if (!initialized) {
            return Mono.error(new IllegalStateException("Plugin not initialized for entity type: " + getEntityType()));
        }

        if (!getSupportedOperations().contains(operation)) {
            return Mono.error(new UnsupportedOperationException(
                "Operation '" + operation + "' not supported for entity type: " + getEntityType()));
        }

        return switch (operation) {
            case "create" -> executeCreate(arguments);
            case "get" -> executeGet(arguments);
            case "update" -> executeUpdate(arguments);
            case "delete" -> executeDelete(arguments);
            case "list" -> executeList(arguments);
            default -> Mono.error(new UnsupportedOperationException(
                "Unknown operation: " + operation));
        };
    }

    protected abstract Mono<Map<String, Object>> executeCreate(Map<String, Object> arguments);
    protected abstract Mono<Map<String, Object>> executeGet(Map<String, Object> arguments);
    protected abstract Mono<Map<String, Object>> executeUpdate(Map<String, Object> arguments);
    protected abstract Mono<Map<String, Object>> executeDelete(Map<String, Object> arguments);
    protected abstract Mono<Map<String, Object>> executeList(Map<String, Object> arguments);

    @Override
    public EntityOperationMetadata getOperationMetadata(String operation) {
        return operationMetadata.get(operation);
    }

    @Override
    public boolean validateArguments(String operation, Map<String, Object> arguments) {
        List<String> errors = getValidationErrors(operation, arguments);
        return errors.isEmpty();
    }

    @Override
    public List<String> getValidationErrors(String operation, Map<String, Object> arguments) {
        List<String> errors = new ArrayList<>();
        
        if (arguments == null) {
            errors.add("Arguments cannot be null");
            return errors;
        }

        EntityOperationMetadata metadata = getOperationMetadata(operation);
        if (metadata != null && metadata.getRequiredFields() != null) {
            for (String requiredField : metadata.getRequiredFields()) {
                if (!arguments.containsKey(requiredField) || arguments.get(requiredField) == null) {
                    errors.add("Required field '" + requiredField + "' is missing or null");
                }
            }
        }

        return errors;
    }

    @Override
    public void initialize() {
        if (initialized) {
            log.warn("Plugin for entity type {} is already initialized", getEntityType());
            return;
        }

        log.info("Initializing plugin for entity type: {}", getEntityType());
        initializeOperationMetadata();
        initialized = true;
        log.info("Successfully initialized plugin for entity type: {}", getEntityType());
    }

    @Override
    public void shutdown() {
        log.info("Shutting down plugin for entity type: {}", getEntityType());
        initialized = false;
        operationMetadata.clear();
    }

    @Override
    public boolean isHealthy() {
        return initialized;
    }

    protected abstract void initializeOperationMetadata();

    protected void registerOperation(String operation, String description, 
                                   List<String> requiredFields, List<String> optionalFields,
                                   String httpMethod, String endpoint) {
        EntityOperationMetadata metadata = EntityOperationMetadata.builder()
            .operation(operation)
            .description(description)
            .requiredFields(requiredFields != null ? requiredFields : List.of())
            .optionalFields(optionalFields != null ? optionalFields : List.of())
            .requiresAuth(true)
            .httpMethod(httpMethod)
            .endpoint(endpoint)
            .build();

        operationMetadata.put(operation, metadata);
    }

    protected Long extractId(Map<String, Object> arguments, String fieldName) {
        if (arguments == null || !arguments.containsKey(fieldName)) {
            throw new IllegalArgumentException(fieldName + " is required");
        }
        
        Object idValue = arguments.get(fieldName);
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid " + fieldName + " format: " + idValue);
        }
    }

    protected Map<String, String> buildPaginationParams(Map<String, Object> arguments) {
        Map<String, String> params = new HashMap<>();
        
        if (arguments.containsKey("page")) {
            params.put("page", arguments.get("page").toString());
        } else {
            params.put("page", "1");
        }
        
        if (arguments.containsKey("limit")) {
            int limit = Math.min(100, Math.max(1, Integer.parseInt(arguments.get("limit").toString())));
            params.put("limit", String.valueOf(limit));
        } else {
            params.put("limit", "10");
        }
        
        return params;
    }

    protected Map<String, Object> formatResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            return Map.of("data", apiResponse.get("data"));
        }
        return Map.of("data", apiResponse);
    }

    protected Map<String, Object> formatListResponse(Map<String, Object> apiResponse) {
        Map<String, Object> result = new HashMap<>();
        result.put("data", apiResponse.get("data") != null ? apiResponse.get("data") : List.of());
        
        // Add meta fields directly to result for MCP protocol
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        return result;
    }

    protected Map<String, Object> createDeleteResponse(Long id, String entityType) {
        Map<String, Object> result = new HashMap<>();
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", entityType + " with ID " + id + " has been deleted successfully"
            )
        );
        result.put("content", content);
        return result;
    }
}