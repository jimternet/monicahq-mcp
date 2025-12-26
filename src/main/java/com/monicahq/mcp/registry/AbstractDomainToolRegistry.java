package com.monicahq.mcp.registry;

import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Mono;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Abstract base class providing common functionality for domain-specific tool registries.
 *
 * This class provides:
 * - Tool registration helpers via registerTool() methods
 * - Access to common schema creation utilities via SchemaBuilders
 * - Auto-initialization via @PostConstruct
 * - Default implementations of DomainToolRegistry methods
 *
 * Subclasses must implement:
 * - initializeTools() to register their domain-specific tools
 * - getDomain() to return their domain identifier
 * - executeToolInternal() to route tool execution to services
 */
@Slf4j
public abstract class AbstractDomainToolRegistry implements DomainToolRegistry {

    private final Map<String, ToolDefinition> tools = new ConcurrentHashMap<>();

    /**
     * Initializes tools after bean construction.
     * Subclasses should NOT override this method directly - implement initializeTools() instead.
     */
    @PostConstruct
    public void init() {
        log.debug("Initializing {} domain registry", getDomain());
        initializeTools();
        log.info("{} domain registry initialized with {} tools", getDomain(), tools.size());
    }

    /**
     * Template method for subclasses to register their domain-specific tools.
     * Called automatically during bean initialization via @PostConstruct.
     */
    protected abstract void initializeTools();

    /**
     * Internal method for subclasses to implement tool execution routing.
     *
     * @param toolName the name of the tool to execute
     * @param arguments the arguments for the tool
     * @return a Mono containing the result Map
     */
    protected abstract Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments);

    // ========== DomainToolRegistry Implementation ==========

    @Override
    public List<ToolDefinition> getTools() {
        return new ArrayList<>(tools.values());
    }

    @Override
    public Mono<Map<String, Object>> executeTool(String toolName, Map<String, Object> arguments) {
        if (!supportsToolName(toolName)) {
            return Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not supported by " + getDomain() + " domain registry"));
        }

        log.debug("Executing tool {} in {} domain", toolName, getDomain());
        return executeToolInternal(toolName, arguments);
    }

    @Override
    public Set<String> getSupportedToolNames() {
        return Collections.unmodifiableSet(tools.keySet());
    }

    // ========== Tool Registration Helpers ==========

    /**
     * Registers a tool with the specified metadata.
     *
     * @param name the unique tool name
     * @param description the tool description
     * @param inputSchema the input schema
     * @param category the tool category for grouping
     */
    protected void registerTool(String name, String description, Map<String, Object> inputSchema, String category) {
        ToolDefinition tool = new ToolDefinition(name, description, inputSchema, category);
        tools.put(name, tool);
        log.debug("Registered tool: {} (category: {})", name, category);
    }

    /**
     * Registers a tool without a category.
     *
     * @param name the unique tool name
     * @param description the tool description
     * @param inputSchema the input schema
     */
    protected void registerTool(String name, String description, Map<String, Object> inputSchema) {
        registerTool(name, description, inputSchema, getDomain());
    }

    // ========== Schema Builder Convenience Methods ==========

    /**
     * Creates a schema for tools that take only an ID parameter.
     * Delegates to SchemaBuilders.
     *
     * @param description description of what the ID represents
     * @return schema Map
     */
    protected Map<String, Object> createIdSchema(String description) {
        return SchemaBuilders.createIdSchema(description);
    }

    /**
     * Creates a schema for list operations with pagination support.
     * Delegates to SchemaBuilders.
     *
     * @return schema Map with pagination properties
     */
    protected Map<String, Object> createListSchema() {
        return SchemaBuilders.createListSchema();
    }

    /**
     * Creates a schema for discovery tools that take no parameters.
     * Delegates to SchemaBuilders.
     *
     * @return schema Map with no properties
     */
    protected Map<String, Object> createListOnlySchema() {
        return SchemaBuilders.createListOnlySchema();
    }

    /**
     * Creates an update schema by combining an ID field with a create schema's properties.
     * Delegates to SchemaBuilders.
     *
     * @param createSchema the create schema to base the update on
     * @return schema Map with id and update properties
     */
    protected Map<String, Object> createUpdateSchema(Map<String, Object> createSchema) {
        return SchemaBuilders.createUpdateSchema(createSchema);
    }

    /**
     * Returns the number of tools registered in this domain registry.
     *
     * @return the tool count
     */
    public int getToolCount() {
        return tools.size();
    }
}
