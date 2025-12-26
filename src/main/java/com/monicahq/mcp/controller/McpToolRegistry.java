package com.monicahq.mcp.controller;

import com.monicahq.mcp.registry.DomainToolRegistry;
import com.monicahq.mcp.registry.ToolDefinition;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Aggregating registry that collects tools from all domain-specific registries.
 *
 * This class serves as the central point for MCP tool operations:
 * - Aggregates tool definitions from all DomainToolRegistry implementations
 * - Routes tool execution to the appropriate domain registry
 * - Provides O(1) tool name lookup via a pre-built routing map
 *
 * The registry pattern delegates all domain-specific logic to specialized registries:
 * - ContactToolRegistry: contact operations
 * - ContactFieldToolRegistry: contact field and tag operations
 * - ContactExtensionToolRegistry: address, group, occupation operations
 * - RelationshipToolRegistry: relationship operations
 * - CompanyToolRegistry: company operations
 * - ActivityToolRegistry: activity and activity type operations
 * - CommunicationToolRegistry: call, conversation, message operations
 * - ProductivityToolRegistry: note, task, reminder, tag operations
 * - ContentToolRegistry: document, photo, pet operations
 * - FinancialToolRegistry: debt, gift operations
 * - ReferenceDataToolRegistry: audit log, country, currency, discovery operations
 * - AdminToolRegistry: user, compliance operations
 */
@Service
@Slf4j
public class McpToolRegistry {

    private final List<DomainToolRegistry> domainRegistries;
    private final Map<String, DomainToolRegistry> toolToRegistryMap = new ConcurrentHashMap<>();
    private final Map<String, ToolDefinition> allTools = new ConcurrentHashMap<>();

    /**
     * Constructs the aggregating registry with all domain registries.
     * Spring automatically injects all DomainToolRegistry implementations.
     *
     * @param domainRegistries list of all domain-specific registries
     */
    public McpToolRegistry(List<DomainToolRegistry> domainRegistries) {
        this.domainRegistries = domainRegistries;
    }

    /**
     * Initializes the tool routing map by aggregating tools from all domain registries.
     * Validates that there are no duplicate tool names across domains.
     */
    @PostConstruct
    public void initializeTools() {
        log.info("Initializing MCP tool registry with {} domain registries", domainRegistries.size());

        Set<String> duplicateTools = new HashSet<>();

        for (DomainToolRegistry registry : domainRegistries) {
            String domain = registry.getDomain();
            List<ToolDefinition> tools = registry.getTools();
            int toolCount = tools.size();

            if (toolCount == 0) {
                log.warn("{} domain registry has 0 tools registered", domain);
            }

            for (ToolDefinition tool : tools) {
                String toolName = tool.name();

                // Check for duplicate tool names
                if (toolToRegistryMap.containsKey(toolName)) {
                    DomainToolRegistry existingRegistry = toolToRegistryMap.get(toolName);
                    log.error("Duplicate tool name '{}' found in {} and {} registries",
                        toolName, existingRegistry.getDomain(), domain);
                    duplicateTools.add(toolName);
                } else {
                    toolToRegistryMap.put(toolName, registry);
                    allTools.put(toolName, tool);
                }
            }

            log.info("  {} domain: {} tools", domain, toolCount);
        }

        // Fail fast if duplicates are found
        if (!duplicateTools.isEmpty()) {
            throw new IllegalStateException(
                "Duplicate tool names found across domain registries: " + duplicateTools);
        }

        log.info("Initialized {} MCP tools from {} domain registries",
            allTools.size(), domainRegistries.size());
    }

    /**
     * Returns all tool definitions from all domain registries.
     * Each tool is converted to a Map for MCP protocol compatibility.
     *
     * @return list of tool definition maps
     */
    public List<Map<String, Object>> getAllTools() {
        return allTools.values().stream()
            .map(ToolDefinition::toMap)
            .toList();
    }

    /**
     * Executes a tool by delegating to the appropriate domain registry.
     *
     * @param toolName the name of the tool to execute
     * @param arguments the arguments for the tool
     * @return the result of the tool execution
     * @throws IllegalArgumentException if the tool is not found
     */
    public Object callTool(String toolName, Map<String, Object> arguments) {
        DomainToolRegistry registry = toolToRegistryMap.get(toolName);
        if (registry == null) {
            throw new IllegalArgumentException("Unknown tool: " + toolName);
        }

        log.info("Executing MCP tool: {} (domain: {}) with arguments: {}",
            toolName, registry.getDomain(), arguments);

        try {
            Mono<Map<String, Object>> result = registry.executeTool(toolName, arguments);
            return result.block(); // Blocking call for synchronous MCP response

        } catch (Exception e) {
            log.error("Error executing tool {} in {} domain: {}",
                toolName, registry.getDomain(), e.getMessage(), e);
            throw e;
        }
    }

    /**
     * Returns the total number of registered tools.
     *
     * @return the tool count
     */
    public int getToolCount() {
        return allTools.size();
    }

    /**
     * Returns the domain registry responsible for a specific tool.
     *
     * @param toolName the tool name
     * @return the domain registry, or null if not found
     */
    public DomainToolRegistry getRegistryForTool(String toolName) {
        return toolToRegistryMap.get(toolName);
    }

    /**
     * Returns the list of domain registries.
     *
     * @return unmodifiable list of domain registries
     */
    public List<DomainToolRegistry> getDomainRegistries() {
        return Collections.unmodifiableList(domainRegistries);
    }
}
