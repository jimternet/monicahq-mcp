package com.monicahq.mcp.registry;

import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Interface defining the contract for domain-specific tool registries.
 *
 * Each domain registry is responsible for:
 * - Registering tools specific to its domain (e.g., contacts, activities)
 * - Providing tool definitions for the MCP protocol
 * - Executing tool operations by delegating to appropriate services
 *
 * This interface enables the decomposition of the monolithic McpToolRegistry
 * into smaller, domain-focused registries that follow the Single Responsibility Principle.
 */
public interface DomainToolRegistry {

    /**
     * Returns all tool definitions registered by this domain registry.
     * Each ToolDefinition contains the tool name, description, input schema, and category.
     *
     * @return List of ToolDefinition objects for all tools in this domain
     */
    List<ToolDefinition> getTools();

    /**
     * Executes a tool operation by name with the provided arguments.
     *
     * @param toolName the name of the tool to execute
     * @param arguments the arguments for the tool operation as a Map
     * @return a Mono containing the result Map from the tool execution
     * @throws UnsupportedOperationException if the tool is not supported by this registry
     */
    Mono<Map<String, Object>> executeTool(String toolName, Map<String, Object> arguments);

    /**
     * Returns the domain identifier for this registry.
     * Used for logging, categorization, and debugging purposes.
     *
     * @return the domain name (e.g., "Contact", "Activity", "Relationship")
     */
    String getDomain();

    /**
     * Returns the set of tool names supported by this registry.
     * Used for O(1) lookup during tool routing.
     *
     * @return Set of tool names that this registry can execute
     */
    Set<String> getSupportedToolNames();

    /**
     * Checks if this registry supports a specific tool.
     * Default implementation uses getSupportedToolNames() for lookup.
     *
     * @param toolName the tool name to check
     * @return true if this registry can execute the tool, false otherwise
     */
    default boolean supportsToolName(String toolName) {
        return getSupportedToolNames().contains(toolName);
    }
}
