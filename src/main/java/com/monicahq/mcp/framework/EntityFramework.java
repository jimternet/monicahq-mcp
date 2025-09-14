package com.monicahq.mcp.framework;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
@Slf4j
public class EntityFramework {

    private final Map<String, EntityPlugin> plugins = new ConcurrentHashMap<>();
    private final Map<String, EntityOperationConfig> operationConfigs = new ConcurrentHashMap<>();

    public void registerPlugin(String entityType, EntityPlugin plugin) {
        log.info("Registering entity plugin for type: {}", entityType);
        plugins.put(entityType, plugin);
        
        plugin.getSupportedOperations().forEach(operation -> {
            String key = entityType + "_" + operation;
            operationConfigs.put(key, new EntityOperationConfig(entityType, operation, plugin));
        });
        
        log.info("Registered {} operations for entity type {}", 
            plugin.getSupportedOperations().size(), entityType);
    }

    public void unregisterPlugin(String entityType) {
        log.info("Unregistering entity plugin for type: {}", entityType);
        EntityPlugin plugin = plugins.remove(entityType);
        
        if (plugin != null) {
            plugin.getSupportedOperations().forEach(operation -> {
                String key = entityType + "_" + operation;
                operationConfigs.remove(key);
            });
        }
    }

    public boolean isOperationSupported(String entityType, String operation) {
        String key = entityType + "_" + operation;
        return operationConfigs.containsKey(key);
    }

    public Mono<Map<String, Object>> executeOperation(String entityType, String operation, Map<String, Object> arguments) {
        String key = entityType + "_" + operation;
        EntityOperationConfig config = operationConfigs.get(key);
        
        if (config == null) {
            return Mono.error(new UnsupportedOperationException(
                "Operation '" + operation + "' not supported for entity type '" + entityType + "'"));
        }
        
        log.debug("Executing {} operation for entity type {} with arguments: {}", 
            operation, entityType, arguments);
        
        try {
            return config.getPlugin().executeOperation(operation, arguments)
                .doOnSuccess(result -> log.debug("Operation {} completed successfully for entity type {}", 
                    operation, entityType))
                .doOnError(error -> log.error("Operation {} failed for entity type {}: {}", 
                    operation, entityType, error.getMessage()));
                    
        } catch (Exception e) {
            log.error("Error executing {} operation for entity type {}: {}", 
                operation, entityType, e.getMessage(), e);
            return Mono.error(e);
        }
    }

    public Set<String> getRegisteredEntityTypes() {
        return new HashSet<>(plugins.keySet());
    }

    public Set<String> getSupportedOperations(String entityType) {
        EntityPlugin plugin = plugins.get(entityType);
        return plugin != null ? new HashSet<>(plugin.getSupportedOperations()) : Set.of();
    }

    public Map<String, Set<String>> getAllSupportedOperations() {
        Map<String, Set<String>> allOperations = new HashMap<>();
        
        plugins.forEach((entityType, plugin) -> {
            allOperations.put(entityType, new HashSet<>(plugin.getSupportedOperations()));
        });
        
        return allOperations;
    }

    public EntityOperationMetadata getOperationMetadata(String entityType, String operation) {
        EntityPlugin plugin = plugins.get(entityType);
        return plugin != null ? plugin.getOperationMetadata(operation) : null;
    }

    public Map<String, Object> getEntitySchema(String entityType) {
        EntityPlugin plugin = plugins.get(entityType);
        return plugin != null ? plugin.getEntitySchema() : Map.of();
    }

    public boolean validateArguments(String entityType, String operation, Map<String, Object> arguments) {
        EntityPlugin plugin = plugins.get(entityType);
        if (plugin == null) {
            return false;
        }
        
        try {
            return plugin.validateArguments(operation, arguments);
        } catch (Exception e) {
            log.warn("Validation error for {} {} operation: {}", entityType, operation, e.getMessage());
            return false;
        }
    }

    public List<String> getValidationErrors(String entityType, String operation, Map<String, Object> arguments) {
        EntityPlugin plugin = plugins.get(entityType);
        if (plugin == null) {
            return List.of("Entity type '" + entityType + "' is not registered");
        }
        
        return plugin.getValidationErrors(operation, arguments);
    }

    public void initializeAllPlugins() {
        log.info("Initializing all registered plugins");
        
        plugins.forEach((entityType, plugin) -> {
            try {
                plugin.initialize();
                log.info("Successfully initialized plugin for entity type: {}", entityType);
            } catch (Exception e) {
                log.error("Failed to initialize plugin for entity type {}: {}", entityType, e.getMessage(), e);
            }
        });
        
        log.info("Plugin initialization completed. Active plugins: {}", plugins.keySet());
    }

    public void shutdownAllPlugins() {
        log.info("Shutting down all registered plugins");
        
        plugins.forEach((entityType, plugin) -> {
            try {
                plugin.shutdown();
                log.info("Successfully shut down plugin for entity type: {}", entityType);
            } catch (Exception e) {
                log.error("Error shutting down plugin for entity type {}: {}", entityType, e.getMessage(), e);
            }
        });
        
        plugins.clear();
        operationConfigs.clear();
        log.info("All plugins have been shut down");
    }

    public FrameworkStats getStats() {
        int totalOperations = operationConfigs.size();
        Map<String, Integer> operationsByType = new HashMap<>();
        
        plugins.forEach((entityType, plugin) -> {
            operationsByType.put(entityType, plugin.getSupportedOperations().size());
        });
        
        return new FrameworkStats(
            plugins.size(),
            totalOperations,
            operationsByType,
            getRegisteredEntityTypes()
        );
    }

    private static class EntityOperationConfig {
        private final String entityType;
        private final String operation;
        private final EntityPlugin plugin;

        public EntityOperationConfig(String entityType, String operation, EntityPlugin plugin) {
            this.entityType = entityType;
            this.operation = operation;
            this.plugin = plugin;
        }

        public String getEntityType() { return entityType; }
        public String getOperation() { return operation; }
        public EntityPlugin getPlugin() { return plugin; }
    }

    public static class FrameworkStats {
        private final int registeredPlugins;
        private final int totalOperations;
        private final Map<String, Integer> operationsByType;
        private final Set<String> entityTypes;

        public FrameworkStats(int registeredPlugins, int totalOperations, 
                            Map<String, Integer> operationsByType, Set<String> entityTypes) {
            this.registeredPlugins = registeredPlugins;
            this.totalOperations = totalOperations;
            this.operationsByType = new HashMap<>(operationsByType);
            this.entityTypes = new HashSet<>(entityTypes);
        }

        public int getRegisteredPlugins() { return registeredPlugins; }
        public int getTotalOperations() { return totalOperations; }
        public Map<String, Integer> getOperationsByType() { return new HashMap<>(operationsByType); }
        public Set<String> getEntityTypes() { return new HashSet<>(entityTypes); }
    }
}