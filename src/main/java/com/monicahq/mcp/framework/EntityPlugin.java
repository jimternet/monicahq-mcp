package com.monicahq.mcp.framework;

import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface EntityPlugin {
    
    String getEntityType();
    
    Set<String> getSupportedOperations();
    
    Mono<Map<String, Object>> executeOperation(String operation, Map<String, Object> arguments);
    
    EntityOperationMetadata getOperationMetadata(String operation);
    
    Map<String, Object> getEntitySchema();
    
    boolean validateArguments(String operation, Map<String, Object> arguments);
    
    List<String> getValidationErrors(String operation, Map<String, Object> arguments);
    
    void initialize();
    
    void shutdown();
    
    default String getVersion() {
        return "1.0.0";
    }
    
    default String getDescription() {
        return "Entity plugin for " + getEntityType();
    }
    
    default boolean isHealthy() {
        return true;
    }
    
    default Map<String, Object> getHealthInfo() {
        return Map.of(
            "entityType", getEntityType(),
            "version", getVersion(),
            "healthy", isHealthy(),
            "operations", getSupportedOperations().size()
        );
    }
}