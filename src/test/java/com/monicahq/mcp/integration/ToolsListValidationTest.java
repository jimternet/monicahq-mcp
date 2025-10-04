package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.monicahq.mcp.config.StdioMcpBaseTest;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for tools/list endpoint validation.
 * Validates all 122 tools are discoverable and properly formatted.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 * Tool discovery and schema validation isn't complete.
 */
@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ToolsListValidationTest extends StdioMcpBaseTest {

    // Expected 122 tools across 23 entity types (5 operations each + special operations)
    private static final String[] EXPECTED_TOOLS = {
        // Contact operations (5)
        "contact_create", "contact_get", "contact_update", "contact_delete", "contact_list",
        
        // Activity operations (5)  
        "activity_create", "activity_get", "activity_update", "activity_delete", "activity_list",
        
        // Call operations (5)
        "call_create", "call_get", "call_update", "call_delete", "call_list",
        
        // Note operations (5)
        "note_create", "note_get", "note_update", "note_delete", "note_list",
        
        // Task operations (5)
        "task_create", "task_get", "task_update", "task_delete", "task_list",
        
        // Tag operations (5) 
        "tag_create", "tag_get", "tag_update", "tag_delete", "tag_list",
        
        // Reminder operations (5)
        "reminder_create", "reminder_get", "reminder_update", "reminder_delete", "reminder_list",
        
        // Journal Entry operations (5)
        "journal_entry_create", "journal_entry_get", "journal_entry_update", "journal_entry_delete", "journal_entry_list",
        
        // Conversation operations (5)
        "conversation_create", "conversation_get", "conversation_update", "conversation_delete", "conversation_list",
        
        // Conversation Message operations (5)
        "conversation_message_create", "conversation_message_get", "conversation_message_update", "conversation_message_delete", "conversation_message_list",
        
        // Contact Field operations (5)
        "contact_field_create", "contact_field_get", "contact_field_update", "contact_field_delete", "contact_field_list",
        
        // Contact Tag operations (3) - special add/remove operations
        "contact_tag_add", "contact_tag_remove", "contact_tag_list",
        
        // Relationship operations (5)
        "relationship_create", "relationship_get", "relationship_update", "relationship_delete", "relationship_list",
        
        // Company operations (5)
        "company_create", "company_get", "company_update", "company_delete", "company_list",
        
        // Relationship Type operations (3) - reference data
        "relationship_type_get", "relationship_type_list", "relationship_type_search",
        
        // Relationship Type Group operations (2) - reference data  
        "relationship_type_group_list", "relationship_type_group_search",
        
        // Debt operations (5)
        "debt_create", "debt_get", "debt_update", "debt_delete", "debt_list",
        
        // Document operations (5)
        "document_create", "document_get", "document_update", "document_delete", "document_list",
        
        // Photo operations (5)
        "photo_create", "photo_get", "photo_update", "photo_delete", "photo_list",
        
        // Gift operations (5)
        "gift_create", "gift_get", "gift_update", "gift_delete", "gift_list",
        
        // Audit Log operations (2) - read-only
        "audit_log_get", "audit_log_list",
        
        // Country operations (2) - reference data
        "country_list", "country_search",
        
        // Currency operations (2) - reference data
        "currency_list", "currency_search"
    };

    @Test
    void shouldDiscoverAll122Tools() throws Exception {
        // When: List all available tools
        // This will FAIL because not all 122 tools are properly registered yet
        JsonNode toolsResult = mcpClient.listTools(200); // Request more than expected
        
        assertNotNull(toolsResult);
        assertEquals("2.0", toolsResult.get("jsonrpc").asText());
        assertTrue(toolsResult.has("result"));
        
        JsonNode result = toolsResult.get("result");
        assertTrue(result.has("tools"));
        
        JsonNode tools = result.get("tools");
        assertTrue(tools.isArray());
        
        // Then: Verify exact count of 122 tools
        assertEquals(122, tools.size(), 
            "Expected exactly 122 tools but found " + tools.size());
        
        // Verify all expected tools are present
        Set<String> foundTools = new HashSet<>();
        for (JsonNode tool : tools) {
            assertTrue(tool.has("name"), "Each tool should have a name");
            foundTools.add(tool.get("name").asText());
        }
        
        Set<String> expectedToolsSet = new HashSet<>(Arrays.asList(EXPECTED_TOOLS));
        
        // Check for missing tools
        Set<String> missingTools = new HashSet<>(expectedToolsSet);
        missingTools.removeAll(foundTools);
        assertTrue(missingTools.isEmpty(), 
            "Missing expected tools: " + missingTools);
        
        // Check for unexpected tools
        Set<String> unexpectedTools = new HashSet<>(foundTools);
        unexpectedTools.removeAll(expectedToolsSet);
        assertTrue(unexpectedTools.isEmpty(), 
            "Found unexpected tools: " + unexpectedTools);
    }

    @Test
    void shouldValidateToolSchemaStructures() throws Exception {
        // When: List tools and validate their schemas
        JsonNode toolsResult = mcpClient.listTools(200);
        JsonNode tools = toolsResult.get("result").get("tools");
        
        // Then: Each tool should have proper schema structure
        for (JsonNode tool : tools) {
            // This will FAIL because schema validation isn't complete
            validateToolSchema(tool);
        }
    }

    @Test
    void shouldValidateCRUDOperationTools() throws Exception {
        // Given: Expected CRUD entities
        String[] crudEntities = {
            "contact", "activity", "call", "note", "task", "tag", "reminder",
            "journal_entry", "conversation", "conversation_message", "contact_field",
            "relationship", "company", "debt", "document", "photo", "gift"
        };
        
        // When: List tools
        JsonNode toolsResult = mcpClient.listTools(200);
        JsonNode tools = toolsResult.get("result").get("tools");
        
        Set<String> foundTools = new HashSet<>();
        for (JsonNode tool : tools) {
            foundTools.add(tool.get("name").asText());
        }
        
        // Then: Each CRUD entity should have create, get, update, delete, list operations
        for (String entity : crudEntities) {
            // This will FAIL because not all CRUD operations are implemented
            assertTrue(foundTools.contains(entity + "_create"), 
                "Missing create operation for " + entity);
            assertTrue(foundTools.contains(entity + "_get"), 
                "Missing get operation for " + entity);
            assertTrue(foundTools.contains(entity + "_update"), 
                "Missing update operation for " + entity);
            assertTrue(foundTools.contains(entity + "_delete"), 
                "Missing delete operation for " + entity);
            assertTrue(foundTools.contains(entity + "_list"), 
                "Missing list operation for " + entity);
        }
    }

    @Test
    void shouldValidateSpecialOperationTools() throws Exception {
        // When: List tools
        JsonNode toolsResult = mcpClient.listTools(200);
        JsonNode tools = toolsResult.get("result").get("tools");
        
        Set<String> foundTools = new HashSet<>();
        for (JsonNode tool : tools) {
            foundTools.add(tool.get("name").asText());
        }
        
        // Then: Special operations should be present
        // This will FAIL because special operations aren't all implemented
        
        // Contact tag special operations
        assertTrue(foundTools.contains("contact_tag_add"), 
            "Missing contact_tag_add operation");
        assertTrue(foundTools.contains("contact_tag_remove"), 
            "Missing contact_tag_remove operation");
        
        // Reference data search operations
        assertTrue(foundTools.contains("relationship_type_search"), 
            "Missing relationship_type_search operation");
        assertTrue(foundTools.contains("relationship_type_group_search"), 
            "Missing relationship_type_group_search operation");
        assertTrue(foundTools.contains("country_search"), 
            "Missing country_search operation");
        assertTrue(foundTools.contains("currency_search"), 
            "Missing currency_search operation");
        
        // Read-only audit log operations
        assertTrue(foundTools.contains("audit_log_get"), 
            "Missing audit_log_get operation");
        assertTrue(foundTools.contains("audit_log_list"), 
            "Missing audit_log_list operation");
    }

    @Test
    void shouldValidateToolDescriptions() throws Exception {
        // When: List tools
        JsonNode toolsResult = mcpClient.listTools(200);
        JsonNode tools = toolsResult.get("result").get("tools");
        
        // Then: Each tool should have a meaningful description
        for (JsonNode tool : tools) {
            // This will FAIL because tool descriptions aren't properly set
            assertTrue(tool.has("description"), 
                "Tool " + tool.get("name").asText() + " should have description");
            
            String description = tool.get("description").asText();
            assertFalse(description.isEmpty(), 
                "Tool " + tool.get("name").asText() + " should have non-empty description");
            assertTrue(description.length() > 10, 
                "Tool " + tool.get("name").asText() + " should have meaningful description");
        }
    }

    @Test
    void shouldValidateToolInputSchemas() throws Exception {
        // When: List tools  
        JsonNode toolsResult = mcpClient.listTools(200);
        JsonNode tools = toolsResult.get("result").get("tools");
        
        // Then: Each tool should have proper input schema
        for (JsonNode tool : tools) {
            String toolName = tool.get("name").asText();
            
            // This will FAIL because input schemas aren't properly defined
            assertTrue(tool.has("inputSchema"), 
                "Tool " + toolName + " should have inputSchema");
            
            JsonNode inputSchema = tool.get("inputSchema");
            assertTrue(inputSchema.has("type"), 
                "Tool " + toolName + " inputSchema should have type");
            assertEquals("object", inputSchema.get("type").asText(), 
                "Tool " + toolName + " inputSchema should be object type");
            
            assertTrue(inputSchema.has("properties"), 
                "Tool " + toolName + " inputSchema should have properties");
            
            JsonNode properties = inputSchema.get("properties");
            assertTrue(properties.isObject(), 
                "Tool " + toolName + " inputSchema properties should be object");
            
            // Validate required field for operations that need IDs
            if (toolName.contains("_get") || toolName.contains("_update") || toolName.contains("_delete")) {
                assertTrue(inputSchema.has("required"), 
                    "Tool " + toolName + " should have required fields");
                
                JsonNode required = inputSchema.get("required");
                assertTrue(required.isArray() && required.size() > 0, 
                    "Tool " + toolName + " should have at least one required field");
            }
        }
    }

    private void validateToolSchema(JsonNode tool) {
        String toolName = tool.get("name").asText();
        
        // Basic structure validation
        assertTrue(tool.has("name"), "Tool should have name");
        assertTrue(tool.has("description"), "Tool should have description");
        assertTrue(tool.has("inputSchema"), "Tool should have inputSchema");
        
        // Name validation
        String name = tool.get("name").asText();
        assertFalse(name.isEmpty(), "Tool name should not be empty");
        assertTrue(name.matches("^[a-z_]+$"), 
            "Tool name should contain only lowercase letters and underscores: " + name);
        
        // Description validation
        String description = tool.get("description").asText();
        assertFalse(description.isEmpty(), "Tool description should not be empty");
        
        // Input schema validation
        JsonNode inputSchema = tool.get("inputSchema");
        assertTrue(inputSchema.isObject(), "Input schema should be object");
        assertTrue(inputSchema.has("type"), "Input schema should have type");
        assertTrue(inputSchema.has("properties"), "Input schema should have properties");
        
        assertEquals("object", inputSchema.get("type").asText(), 
            "Input schema type should be object");
        
        JsonNode properties = inputSchema.get("properties");
        assertTrue(properties.isObject(), "Properties should be object");
        
        // Validate each property has proper structure
        properties.fieldNames().forEachRemaining(fieldName -> {
            JsonNode property = properties.get(fieldName);
            assertTrue(property.has("type"), 
                "Property " + fieldName + " in tool " + toolName + " should have type");
        });
    }
}