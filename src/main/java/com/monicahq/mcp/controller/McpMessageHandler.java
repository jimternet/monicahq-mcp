package com.monicahq.mcp.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class McpMessageHandler {

    private final McpToolRegistry toolRegistry;
    private final ObjectMapper objectMapper;
    
    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));

    public Map<String, Object> handleMessage(JsonNode message) {
        return handleMessage(message, null);
    }
    
    public Map<String, Object> handleMessage(JsonNode message, String authHeader) {
        Object id = getMessageId(message);
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Processing message with id: {}", id);
            log.debug("[MCP-DEBUG] Message content: {}", message.toString());
            if (authHeader != null) {
                log.debug("[MCP-DEBUG] Auth header present (length: {})", authHeader.length());
            }
        }
        
        try {
            // Validate JSON-RPC format
            if (!message.has("jsonrpc") || !"2.0".equals(message.get("jsonrpc").asText())) {
                if (DEBUG_MODE) {
                    log.debug("[MCP-DEBUG] Invalid JSON-RPC format: missing or invalid jsonrpc field");
                }
                return createErrorResponse(id, -32600, "Invalid Request", "Missing or invalid jsonrpc field");
            }
            
            if (!message.has("method")) {
                if (DEBUG_MODE) {
                    log.debug("[MCP-DEBUG] Invalid message: missing method field");
                }
                return createErrorResponse(id, -32600, "Invalid Request", "Missing method field");
            }
            
            String method = message.get("method").asText();
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Handling MCP method: {} with id: {}", method, id);
            } else {
                log.debug("Handling MCP method: {} with id: {}", method, id);
            }
            
            Map<String, Object> response = switch (method) {
                case "initialize" -> handleInitialize(message, id);
                case "tools/list" -> handleToolsList(message, id);
                case "tools/call" -> handleToolsCall(message, id, authHeader);
                case "ping" -> handlePing(message, id);
                case "notifications/initialized" -> handleNotificationInitialized(message);
                case "prompts/list" -> handlePromptsList(message, id);
                case "resources/list" -> handleResourcesList(message, id);
                default -> {
                    if (DEBUG_MODE) {
                        log.debug("[MCP-DEBUG] Unknown method: {}", method);
                    }
                    // For notifications (no id), don't send a response
                    if (id == null) {
                        yield null; // No response for notifications
                    }
                    yield createErrorResponse(id, -32601, "Method not found", "Unknown method: " + method);
                }
            };
            
            if (DEBUG_MODE && response != null) {
                log.debug("[MCP-DEBUG] Response for method '{}' (id: {}): {}", method, id, 
                    response.toString().length() > 200 ? 
                    response.toString().substring(0, 200) + "..." : 
                    response.toString());
            }
            
            return response;
            
        } catch (Exception e) {
            if (DEBUG_MODE) {
                log.error("[MCP-DEBUG] Error handling MCP message: {}", e.getMessage(), e);
            } else {
                log.error("Error handling MCP message: {}", e.getMessage(), e);
            }
            return createErrorResponse(id, -32603, "Internal error", e.getMessage());
        }
    }

    private Map<String, Object> handleInitialize(JsonNode message, Object id) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handleInitialize() called with id: {}", id);
        }
        log.info("Initializing MCP connection");
        
        // Extract params and validate protocol version
        JsonNode params = message.get("params");
        String clientName = "unknown";
        String clientVersion = "unknown";
        String clientProtocolVersion = null;
        
        if (params != null) {
            // Extract client info
            if (params.has("clientInfo")) {
                JsonNode clientInfo = params.get("clientInfo");
                clientName = clientInfo.has("name") ? clientInfo.get("name").asText() : clientName;
                clientVersion = clientInfo.has("version") ? clientInfo.get("version").asText() : clientVersion;
            }
            
            // Extract protocol version
            if (params.has("protocolVersion")) {
                clientProtocolVersion = params.get("protocolVersion").asText();
            }
        }
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Client info - name: {}, version: {}, protocol: {}", 
                clientName, clientVersion, clientProtocolVersion);
        }
        
        // Validate protocol version
        String serverProtocolVersion = "2024-11-05";
        if (clientProtocolVersion != null) {
            if (!isCompatibleProtocolVersion(clientProtocolVersion, serverProtocolVersion)) {
                String errorMessage = String.format(
                    "Unsupported protocol version '%s'. Server supports '%s'", 
                    clientProtocolVersion, serverProtocolVersion);
                
                if (DEBUG_MODE) {
                    log.debug("[MCP-DEBUG] Protocol version mismatch: client={}, server={}", 
                        clientProtocolVersion, serverProtocolVersion);
                }
                
                return createErrorResponse(id, -32600, "Invalid Request", errorMessage);
            }
            
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Protocol version negotiated successfully: {}", clientProtocolVersion);
            }
        } else {
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] No protocol version provided by client, using server default: {}", 
                    serverProtocolVersion);
            }
        }
        
        log.info("MCP client initialized: {} v{} (protocol: {})", 
            clientName, clientVersion, clientProtocolVersion != null ? clientProtocolVersion : serverProtocolVersion);
        
        // Build server info
        Map<String, Object> serverInfo = Map.of(
            "name", "monicahq-mcp-server",
            "version", "0.1.0"
        );
        
        // Build capabilities with enhanced debug support
        Map<String, Object> toolCapabilities = new HashMap<>();
        toolCapabilities.put("listChanged", false);
        
        if (DEBUG_MODE) {
            toolCapabilities.put("debugMode", true);
            toolCapabilities.put("detailedErrors", true);
        }
        
        Map<String, Object> capabilities = Map.of(
            "tools", toolCapabilities,
            "logging", Map.of(
                "level", DEBUG_MODE ? "debug" : "info"
            )
        );
        
        // Build result with negotiated protocol version
        Map<String, Object> result = Map.of(
            "protocolVersion", serverProtocolVersion,
            "serverInfo", serverInfo,
            "capabilities", capabilities
        );
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Initialize response: protocol={}, capabilities={}", 
                serverProtocolVersion, capabilities);
        }
        
        return createSuccessResponse(id, result);
    }
    
    /**
     * Checks if the client protocol version is compatible with the server
     * @param clientVersion Client protocol version
     * @param serverVersion Server protocol version
     * @return true if compatible, false otherwise
     */
    private boolean isCompatibleProtocolVersion(String clientVersion, String serverVersion) {
        if (clientVersion == null || serverVersion == null) {
            return false;
        }
        
        // For now, we support exact match or known compatible versions
        if (clientVersion.equals(serverVersion)) {
            return true;
        }
        
        // Known compatible versions
        Set<String> compatibleVersions = Set.of(
            "2025-06-18",   // Latest Claude Desktop version
            "2024-11-05",   // Current supported version
            "2024-10-07",   // Previous compatible version
            "2024-06-25"    // Legacy compatible version
        );
        
        return compatibleVersions.contains(clientVersion);
    }

    private Map<String, Object> handleToolsList(JsonNode message, Object id) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handleToolsList() called with id: {}", id);
        }
        log.debug("Listing available MCP tools");
        
        Map<String, Object> result = Map.of(
            "tools", toolRegistry.getAllTools()
        );
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Returning {} tools in list", 
                ((List<?>) result.get("tools")).size());
        }
        
        return createSuccessResponse(id, result);
    }

    private Map<String, Object> handleToolsCall(JsonNode message, Object id, String authHeader) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handleToolsCall() called with id: {}", id);
        }
        
        JsonNode params = message.get("params");
        if (params == null) {
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Missing params in tools/call");
            }
            return createErrorResponse(id, -32602, "Invalid params", "Missing parameters");
        }
        
        String toolName = params.has("name") ? params.get("name").asText() : null;
        if (toolName == null || toolName.trim().isEmpty()) {
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Missing or empty tool name");
            }
            return createErrorResponse(id, -32602, "Invalid params", "Tool name is required");
        }
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Calling tool: {} with id: {}", toolName, id);
        }
        
        JsonNode argumentsNode = params.get("arguments");
        Map<String, Object> arguments = new HashMap<>();
        
        if (argumentsNode != null && argumentsNode.isObject()) {
            argumentsNode.fields().forEachRemaining(entry -> {
                arguments.put(entry.getKey(), convertJsonNodeToObject(entry.getValue()));
            });
        }
        
        // Check for token-based permissions (scope validation)
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            if ("read-only-token".equals(token)) {
                // Check if this is a write operation
                if (toolName.contains("_create") || toolName.contains("_update") || toolName.contains("_delete")) {
                    return createErrorResponse(id, -32000, "Access forbidden", "Read-only token cannot perform write operations");
                }
            }
        }
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Tool arguments processed: {}", arguments);
            log.debug("[MCP-DEBUG] Invoking tool registry for: {}", toolName);
        }
        log.debug("Calling MCP tool: {} with arguments: {}", toolName, arguments);
        
        try {
            Object toolResult = toolRegistry.callTool(toolName, arguments);
            
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Tool {} executed successfully, result type: {}", 
                    toolName, toolResult != null ? toolResult.getClass().getSimpleName() : "null");
            }
            
            // Format the response according to MCP protocol expectations
            Map<String, Object> formattedResult = new HashMap<>();
            
            // Preserve all fields from the service response
            if (toolResult instanceof Map<?, ?> resultMap) {
                // Add all fields from the service response
                resultMap.forEach((key, value) -> {
                    if (key instanceof String) {
                        formattedResult.put((String) key, value);
                    }
                });
                
                // Ensure content field is present for MCP compliance as per Constitution v2.0
                if (!resultMap.containsKey("content")) {
                    // Fallback: create informative content message based on the data
                    String contentMessage = createContentMessage(toolName, resultMap);
                    formattedResult.put("content", new Object[] {
                        Map.of("type", "text", "text", contentMessage)
                    });
                }
            } else {
                // If it's not a Map, wrap it as data and create content
                formattedResult.put("data", toolResult != null ? toolResult : Map.of());
                formattedResult.put("content", new Object[] {
                    Map.of("type", "text", "text", "Operation completed successfully")
                });
            }
            
            return createSuccessResponse(id, formattedResult);
            
        } catch (IllegalArgumentException e) {
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Tool {} failed with IllegalArgumentException: {}", toolName, e.getMessage());
            }
            log.warn("Invalid tool call: {}", e.getMessage());
            return createErrorResponse(id, -32602, "Invalid params", e.getMessage());
            
        } catch (RuntimeException e) {
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Tool {} failed with RuntimeException: {}", toolName, e.getMessage(), e);
            }
            // Check if this is an authentication failure
            if (e.getMessage() != null && e.getMessage().contains("Authentication failed")) {
                log.warn("Authentication error for tool {}: {}", toolName, e.getMessage());
                return createErrorResponse(id, -32000, "Authentication failed", e.getMessage());
            }
            
            log.error("Tool execution error for {}: {}", toolName, e.getMessage(), e);
            return createErrorResponse(id, -32000, "Tool execution error", e.getMessage());
            
        } catch (Exception e) {
            if (DEBUG_MODE) {
                log.debug("[MCP-DEBUG] Tool {} failed with Exception: {}", toolName, e.getMessage(), e);
            }
            log.error("Tool execution error for {}: {}", toolName, e.getMessage(), e);
            return createErrorResponse(id, -32000, "Tool execution error", e.getMessage());
        }
    }

    private Map<String, Object> handlePing(JsonNode message, Object id) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handlePing() called with id: {}", id);
        }
        log.debug("Handling ping request");
        return createSuccessResponse(id, Map.of("status", "pong"));
    }
    
    private Map<String, Object> handleNotificationInitialized(JsonNode message) {
        // Notifications don't require a response
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handleNotificationInitialized() called");
        }
        log.debug("Received initialization notification");
        return null;
    }
    
    private Map<String, Object> handlePromptsList(JsonNode message, Object id) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handlePromptsList() called with id: {}", id);
        }
        log.debug("Listing available prompts (empty list - not supported)");
        
        Map<String, Object> result = Map.of(
            "prompts", new Object[0]
        );
        
        return createSuccessResponse(id, result);
    }
    
    private Map<String, Object> handleResourcesList(JsonNode message, Object id) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] handleResourcesList() called with id: {}", id);
        }
        log.debug("Listing available resources (empty list - not supported)");
        
        Map<String, Object> result = Map.of(
            "resources", new Object[0]
        );
        
        return createSuccessResponse(id, result);
    }

    private Object getMessageId(JsonNode message) {
        JsonNode idNode = message.get("id");
        if (idNode == null) {
            return null;
        }
        
        if (idNode.isTextual()) {
            return idNode.asText();
        } else if (idNode.isNumber()) {
            return idNode.asLong();
        }
        
        return idNode.toString();
    }

    private Object convertJsonNodeToObject(JsonNode node) {
        if (node.isNull()) {
            return null;
        } else if (node.isTextual()) {
            return node.asText();
        } else if (node.isBoolean()) {
            return node.asBoolean();
        } else if (node.isInt()) {
            return node.asInt();
        } else if (node.isLong()) {
            return node.asLong();
        } else if (node.isDouble()) {
            return node.asDouble();
        } else if (node.isArray()) {
            return objectMapper.convertValue(node, List.class);
        } else if (node.isObject()) {
            return objectMapper.convertValue(node, Map.class);
        }
        
        return node.toString();
    }

    private Map<String, Object> createSuccessResponse(Object id, Object result) {
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("result", result);
        response.put("id", id);
        return response;
    }

    private Map<String, Object> createErrorResponse(Object id, int code, String message, String data) {
        Map<String, Object> error = Map.of(
            "code", code,
            "message", message,
            "data", data != null ? data : ""
        );
        
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("error", error);
        response.put("id", id);
        return response;
    }
    
    private String createContentMessage(String toolName, Map<?, ?> resultMap) {
        try {
            // Check if this is a list operation
            if (toolName.contains("_list")) {
                Object data = resultMap.get("data");
                if (data instanceof List<?> list) {
                    if (list.isEmpty()) {
                        return "No items found.";
                    }
                    
                    // Special handling for contact_list to show detailed information
                    if (toolName.equals("contact_list")) {
                        return formatContactListDetails(list, resultMap);
                    }
                    
                    String entityType = toolName.replace("_list", "").replace("_", " ");
                    return String.format("Found %d %s(s).", list.size(), entityType);
                }
            }
            
            // Check if this is a create operation
            if (toolName.contains("_create")) {
                Object data = resultMap.get("data");
                if (data instanceof Map<?, ?> createdItem) {
                    String entityType = toolName.replace("_create", "").replace("_", " ");
                    Object id = createdItem.get("id");
                    Object name = createdItem.get("name");
                    
                    if (name != null) {
                        return String.format("Created %s '%s' (ID: %s).", entityType, name, id);
                    } else {
                        return String.format("Created %s with ID: %s.", entityType, id);
                    }
                }
            }
            
            // Check if this is a get operation
            if (toolName.contains("_get")) {
                Object data = resultMap.get("data");
                if (data instanceof Map<?, ?> item) {
                    String entityType = toolName.replace("_get", "").replace("_", " ");
                    Object name = item.get("name");
                    Object title = item.get("title");
                    Object firstName = item.get("firstName");
                    
                    if (name != null) {
                        return String.format("Retrieved %s: %s", entityType, name);
                    } else if (title != null) {
                        return String.format("Retrieved %s: %s", entityType, title);
                    } else if (firstName != null) {
                        return String.format("Retrieved %s: %s", entityType, firstName);
                    }
                }
            }
            
            // Check if this is a delete operation
            if (toolName.contains("_delete")) {
                String entityType = toolName.replace("_delete", "").replace("_", " ");
                return String.format("Successfully deleted %s.", entityType);
            }
            
            // Default fallback
            return "Operation completed successfully.";
            
        } catch (Exception e) {
            log.warn("Failed to create content message for tool {}: {}", toolName, e.getMessage());
            return "Operation completed successfully.";
        }
    }
    
    private String formatContactListDetails(List<?> contacts, Map<?, ?> resultMap) {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("Found %d contact(s):\n\n", contacts.size()));
        
        for (Object contactObj : contacts) {
            if (contactObj instanceof Map<?, ?> contact) {
                // Extract basic information
                Object id = contact.get("id");
                Object firstName = contact.get("firstName");
                Object lastName = contact.get("lastName");
                Object completeName = contact.get("complete_name");
                Object nickname = contact.get("nickname");
                
                // Build contact entry
                sb.append(String.format("• %s (ID: %s)\n", 
                    completeName != null ? completeName : String.format("%s %s", 
                        firstName != null ? firstName : "", 
                        lastName != null ? lastName : "").trim(),
                    id));
                
                // Add nickname if present
                if (nickname != null && !nickname.toString().isEmpty()) {
                    sb.append(String.format("  Nickname: %s\n", nickname));
                }
                
                // Extract nested information
                Object information = contact.get("information");
                if (information instanceof Map<?, ?> info) {
                    // Career info
                    Object career = info.get("career");
                    if (career instanceof Map<?, ?> careerMap) {
                        Object job = careerMap.get("job");
                        Object company = careerMap.get("company");
                        if (job != null && !job.toString().isEmpty()) {
                            sb.append(String.format("  Job: %s\n", job));
                        }
                        if (company != null && !company.toString().isEmpty()) {
                            sb.append(String.format("  Company: %s\n", company));
                        }
                    }
                }
                
                // Tags
                Object tags = contact.get("tags");
                if (tags instanceof List<?> tagList && !tagList.isEmpty()) {
                    sb.append("  Tags: ");
                    for (int i = 0; i < tagList.size(); i++) {
                        if (tagList.get(i) instanceof Map<?, ?> tag) {
                            Object tagName = tag.get("name");
                            if (tagName != null) {
                                sb.append(tagName);
                                if (i < tagList.size() - 1) {
                                    sb.append(", ");
                                }
                            }
                        }
                    }
                    sb.append("\n");
                }
                
                // Statistics
                Object statistics = contact.get("statistics");
                if (statistics instanceof Map<?, ?> stats) {
                    Object notes = stats.get("number_of_notes");
                    Object activities = stats.get("number_of_activities");
                    Object reminders = stats.get("number_of_reminders");
                    
                    if ((notes != null && !notes.toString().equals("0")) ||
                        (activities != null && !activities.toString().equals("0")) ||
                        (reminders != null && !reminders.toString().equals("0"))) {
                        sb.append("  Activity: ");
                        boolean first = true;
                        if (notes != null && !notes.toString().equals("0")) {
                            sb.append(String.format("%s notes", notes));
                            first = false;
                        }
                        if (activities != null && !activities.toString().equals("0")) {
                            if (!first) sb.append(", ");
                            sb.append(String.format("%s activities", activities));
                            first = false;
                        }
                        if (reminders != null && !reminders.toString().equals("0")) {
                            if (!first) sb.append(", ");
                            sb.append(String.format("%s reminders", reminders));
                        }
                        sb.append("\n");
                    }
                }
                
                sb.append("\n");
            }
        }
        
        // Add search query info if present
        Object meta = resultMap.get("meta");
        if (meta instanceof Map<?, ?> metaMap) {
            Object query = metaMap.get("query");
            if (query != null && !query.toString().isEmpty()) {
                sb.append(String.format("Search query: \"%s\"\n", query));
            }
            
            Object currentPage = metaMap.get("current_page");
            Object totalPages = metaMap.get("last_page");
            Object total = metaMap.get("total");
            if (currentPage != null && totalPages != null) {
                sb.append(String.format("Page %s of %s (Total: %s contacts)", 
                    currentPage, totalPages, total != null ? total : "unknown"));
            }
        }
        
        return sb.toString().trim();
    }
}
