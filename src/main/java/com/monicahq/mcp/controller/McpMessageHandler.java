package com.monicahq.mcp.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class McpMessageHandler {

    private final McpToolRegistry toolRegistry;
    private final ObjectMapper objectMapper;

    public Map<String, Object> handleMessage(JsonNode message) {
        return handleMessage(message, null);
    }
    
    public Map<String, Object> handleMessage(JsonNode message, String authHeader) {
        Object id = getMessageId(message);
        
        try {
            // Validate JSON-RPC format
            if (!message.has("jsonrpc") || !"2.0".equals(message.get("jsonrpc").asText())) {
                return createErrorResponse(id, -32600, "Invalid Request", "Missing or invalid jsonrpc field");
            }
            
            if (!message.has("method")) {
                return createErrorResponse(id, -32600, "Invalid Request", "Missing method field");
            }
            
            String method = message.get("method").asText();
            log.debug("Handling MCP method: {} with id: {}", method, id);
            
            return switch (method) {
                case "initialize" -> handleInitialize(message, id);
                case "tools/list" -> handleToolsList(message, id);
                case "tools/call" -> handleToolsCall(message, id, authHeader);
                case "ping" -> handlePing(message, id);
                case "notifications/initialized" -> handleNotificationInitialized(message);
                case "prompts/list" -> handlePromptsList(message, id);
                case "resources/list" -> handleResourcesList(message, id);
                default -> {
                    // For notifications (no id), don't send a response
                    if (id == null) {
                        yield null; // No response for notifications
                    }
                    yield createErrorResponse(id, -32601, "Method not found", "Unknown method: " + method);
                }
            };
            
        } catch (Exception e) {
            log.error("Error handling MCP message: {}", e.getMessage(), e);
            return createErrorResponse(id, -32603, "Internal error", e.getMessage());
        }
    }

    private Map<String, Object> handleInitialize(JsonNode message, Object id) {
        log.info("Initializing MCP connection");
        
        // Extract client info if available
        JsonNode params = message.get("params");
        String clientName = "unknown";
        String clientVersion = "unknown";
        
        if (params != null && params.has("clientInfo")) {
            JsonNode clientInfo = params.get("clientInfo");
            clientName = clientInfo.has("name") ? clientInfo.get("name").asText() : clientName;
            clientVersion = clientInfo.has("version") ? clientInfo.get("version").asText() : clientVersion;
        }
        
        log.info("MCP client initialized: {} v{}", clientName, clientVersion);
        
        Map<String, Object> serverInfo = Map.of(
            "name", "monicahq-mcp-server",
            "version", "0.1.0"
        );
        
        Map<String, Object> capabilities = Map.of(
            "tools", Map.of(
                "listChanged", false
            )
        );
        
        Map<String, Object> result = Map.of(
            "protocolVersion", "2024-11-05",
            "serverInfo", serverInfo,
            "capabilities", capabilities
        );
        
        return createSuccessResponse(id, result);
    }

    private Map<String, Object> handleToolsList(JsonNode message, Object id) {
        log.debug("Listing available MCP tools");
        
        Map<String, Object> result = Map.of(
            "tools", toolRegistry.getAllTools()
        );
        
        return createSuccessResponse(id, result);
    }

    private Map<String, Object> handleToolsCall(JsonNode message, Object id, String authHeader) {
        JsonNode params = message.get("params");
        if (params == null) {
            return createErrorResponse(id, -32602, "Invalid params", "Missing parameters");
        }
        
        String toolName = params.has("name") ? params.get("name").asText() : null;
        if (toolName == null || toolName.trim().isEmpty()) {
            return createErrorResponse(id, -32602, "Invalid params", "Tool name is required");
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
        
        log.debug("Calling MCP tool: {} with arguments: {}", toolName, arguments);
        
        try {
            Object toolResult = toolRegistry.callTool(toolName, arguments);
            
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
            log.warn("Invalid tool call: {}", e.getMessage());
            return createErrorResponse(id, -32602, "Invalid params", e.getMessage());
            
        } catch (RuntimeException e) {
            // Check if this is an authentication failure
            if (e.getMessage() != null && e.getMessage().contains("Authentication failed")) {
                log.warn("Authentication error for tool {}: {}", toolName, e.getMessage());
                return createErrorResponse(id, -32000, "Authentication failed", e.getMessage());
            }
            
            log.error("Tool execution error for {}: {}", toolName, e.getMessage(), e);
            return createErrorResponse(id, -32000, "Tool execution error", e.getMessage());
            
        } catch (Exception e) {
            log.error("Tool execution error for {}: {}", toolName, e.getMessage(), e);
            return createErrorResponse(id, -32000, "Tool execution error", e.getMessage());
        }
    }

    private Map<String, Object> handlePing(JsonNode message, Object id) {
        log.debug("Handling ping request");
        return createSuccessResponse(id, Map.of("status", "pong"));
    }
    
    private Map<String, Object> handleNotificationInitialized(JsonNode message) {
        // Notifications don't require a response
        log.debug("Received initialization notification");
        return null;
    }
    
    private Map<String, Object> handlePromptsList(JsonNode message, Object id) {
        log.debug("Listing available prompts (empty list - not supported)");
        
        Map<String, Object> result = Map.of(
            "prompts", new Object[0]
        );
        
        return createSuccessResponse(id, result);
    }
    
    private Map<String, Object> handleResourcesList(JsonNode message, Object id) {
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
            return objectMapper.convertValue(node, Object[].class);
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
