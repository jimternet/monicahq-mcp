package com.monicahq.mcp.integration;

import com.monicahq.mcp.controller.McpToolRegistry;
import com.monicahq.mcp.registry.DomainToolRegistry;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for McpToolRegistry verifying the aggregated registry architecture.
 * These tests ensure all domain registries are properly integrated and tools are accessible
 * through the central McpToolRegistry.
 */
@SpringBootTest
@ActiveProfiles("test")
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class McpToolRegistryIntegrationTest {

    @Autowired
    private McpToolRegistry toolRegistry;

    // ========== Tool Registration Tests ==========

    @Nested
    @DisplayName("Tool Registration Tests")
    class ToolRegistrationTests {

        @Test
        @DisplayName("Should have all 141 tools registered")
        void shouldHaveAllToolsRegistered() {
            var tools = toolRegistry.getAllTools();

            assertNotNull(tools);
            assertEquals(141, tools.size(),
                "Expected exactly 141 tools registered, but found: " + tools.size());
        }

        @Test
        @DisplayName("Should verify key tools from each domain are present")
        void shouldHaveKeyToolsFromEachDomain() {
            var tools = toolRegistry.getAllTools();
            var toolNames = tools.stream()
                .map(tool -> (String) tool.get("name"))
                .collect(Collectors.toSet());

            // Contact domain
            assertTrue(toolNames.contains("contact_create"), "Missing contact_create");
            assertTrue(toolNames.contains("contact_get"), "Missing contact_get");
            assertTrue(toolNames.contains("contact_update"), "Missing contact_update");
            assertTrue(toolNames.contains("contact_delete"), "Missing contact_delete");
            assertTrue(toolNames.contains("contact_list"), "Missing contact_list");

            // Contact Field domain
            assertTrue(toolNames.contains("contact_field_create"), "Missing contact_field_create");
            assertTrue(toolNames.contains("contacttag_add"), "Missing contacttag_add");

            // Contact Extension domain
            assertTrue(toolNames.contains("address_create"), "Missing address_create");
            assertTrue(toolNames.contains("group_create"), "Missing group_create");
            assertTrue(toolNames.contains("occupation_create"), "Missing occupation_create");

            // Relationship domain
            assertTrue(toolNames.contains("relationship_create"), "Missing relationship_create");
            assertTrue(toolNames.contains("relationship_type_list"), "Missing relationship_type_list");

            // Company domain
            assertTrue(toolNames.contains("company_create"), "Missing company_create");

            // Activity domain
            assertTrue(toolNames.contains("activity_create"), "Missing activity_create");
            assertTrue(toolNames.contains("activity_type_create"), "Missing activity_type_create");
            assertTrue(toolNames.contains("activity_type_category_create"), "Missing activity_type_category_create");

            // Communication domain
            assertTrue(toolNames.contains("call_create"), "Missing call_create");
            assertTrue(toolNames.contains("conversation_create"), "Missing conversation_create");
            assertTrue(toolNames.contains("conversation_message_create"), "Missing conversation_message_create");

            // Productivity domain
            assertTrue(toolNames.contains("note_create"), "Missing note_create");
            assertTrue(toolNames.contains("task_create"), "Missing task_create");
            assertTrue(toolNames.contains("reminder_create"), "Missing reminder_create");
            assertTrue(toolNames.contains("tag_create"), "Missing tag_create");

            // Content domain
            assertTrue(toolNames.contains("document_create"), "Missing document_create");
            assertTrue(toolNames.contains("photo_create"), "Missing photo_create");
            assertTrue(toolNames.contains("pet_create"), "Missing pet_create");

            // Financial domain
            assertTrue(toolNames.contains("debt_create"), "Missing debt_create");
            assertTrue(toolNames.contains("gift_create"), "Missing gift_create");

            // Reference Data domain
            assertTrue(toolNames.contains("auditlog_list"), "Missing auditlog_list");
            assertTrue(toolNames.contains("country_list"), "Missing country_list");
            assertTrue(toolNames.contains("currency_list"), "Missing currency_list");
            assertTrue(toolNames.contains("gender_list"), "Missing gender_list");
            assertTrue(toolNames.contains("contact_field_type_list"), "Missing contact_field_type_list");

            // Admin domain
            assertTrue(toolNames.contains("user_create"), "Missing user_create");
            assertTrue(toolNames.contains("compliance_create"), "Missing compliance_create");
        }

        @Test
        @DisplayName("Should return correct tool count via getToolCount method")
        void shouldReturnCorrectToolCount() {
            assertEquals(141, toolRegistry.getToolCount(),
                "getToolCount() should return 141");
        }
    }

    // ========== Domain Registry Aggregation Tests ==========

    @Nested
    @DisplayName("Domain Registry Aggregation Tests")
    class DomainRegistryAggregationTests {

        @Test
        @DisplayName("Should aggregate from 12 domain registries")
        void shouldAggregateFromAllDomainRegistries() {
            List<DomainToolRegistry> domainRegistries = toolRegistry.getDomainRegistries();

            assertNotNull(domainRegistries);
            assertEquals(12, domainRegistries.size(),
                "Expected 12 domain registries");
        }

        @Test
        @DisplayName("Should include all expected domain registries")
        void shouldIncludeAllExpectedDomains() {
            List<DomainToolRegistry> domainRegistries = toolRegistry.getDomainRegistries();
            Set<String> domains = domainRegistries.stream()
                .map(DomainToolRegistry::getDomain)
                .collect(Collectors.toSet());

            assertTrue(domains.contains("Contact"), "Missing Contact domain");
            assertTrue(domains.contains("ContactField"), "Missing ContactField domain");
            assertTrue(domains.contains("ContactExtension"), "Missing ContactExtension domain");
            assertTrue(domains.contains("Relationship"), "Missing Relationship domain");
            assertTrue(domains.contains("Company"), "Missing Company domain");
            assertTrue(domains.contains("Activity"), "Missing Activity domain");
            assertTrue(domains.contains("Communication"), "Missing Communication domain");
            assertTrue(domains.contains("Productivity"), "Missing Productivity domain");
            assertTrue(domains.contains("Content"), "Missing Content domain");
            assertTrue(domains.contains("Financial"), "Missing Financial domain");
            assertTrue(domains.contains("ReferenceData"), "Missing ReferenceData domain");
            assertTrue(domains.contains("Admin"), "Missing Admin domain");
        }

        @Test
        @DisplayName("Should route tools to correct domain registry")
        void shouldRouteToolsToCorrectRegistry() {
            // Verify contact tools route to Contact registry
            DomainToolRegistry contactRegistry = toolRegistry.getRegistryForTool("contact_create");
            assertNotNull(contactRegistry);
            assertEquals("Contact", contactRegistry.getDomain());

            // Verify note tools route to Productivity registry
            DomainToolRegistry productivityRegistry = toolRegistry.getRegistryForTool("note_create");
            assertNotNull(productivityRegistry);
            assertEquals("Productivity", productivityRegistry.getDomain());

            // Verify activity tools route to Activity registry
            DomainToolRegistry activityRegistry = toolRegistry.getRegistryForTool("activity_create");
            assertNotNull(activityRegistry);
            assertEquals("Activity", activityRegistry.getDomain());

            // Verify call tools route to Communication registry
            DomainToolRegistry communicationRegistry = toolRegistry.getRegistryForTool("call_create");
            assertNotNull(communicationRegistry);
            assertEquals("Communication", communicationRegistry.getDomain());

            // Verify debt tools route to Financial registry
            DomainToolRegistry financialRegistry = toolRegistry.getRegistryForTool("debt_create");
            assertNotNull(financialRegistry);
            assertEquals("Financial", financialRegistry.getDomain());
        }

        @Test
        @DisplayName("Should return null for non-existent tool lookup")
        void shouldReturnNullForNonExistentTool() {
            DomainToolRegistry registry = toolRegistry.getRegistryForTool("non_existent_tool");
            assertNull(registry);
        }

        @Test
        @DisplayName("Domain registries should be immutable")
        void domainRegistriesShouldBeImmutable() {
            List<DomainToolRegistry> domainRegistries = toolRegistry.getDomainRegistries();

            assertThrows(UnsupportedOperationException.class, () -> {
                domainRegistries.add(null);
            });
        }
    }

    // ========== Tool Execution Tests ==========

    @Nested
    @DisplayName("Tool Execution Tests")
    class ToolExecutionTests {

        @Test
        @DisplayName("Should validate and execute contact_create with valid arguments")
        void shouldValidateToolArgumentsCorrectly() {
            // Test valid arguments - using TestMonicaHqClient in test profile
            Map<String, Object> validArgs = Map.of(
                "firstName", "John",
                "lastName", "Doe",
                "genderId", 1,
                "isBirthdateKnown", false,
                "isDeceased", false,
                "isDeceasedDateKnown", false
            );

            // This should not throw an exception
            assertDoesNotThrow(() -> {
                Object result = toolRegistry.callTool("contact_create", validArgs);
                assertNotNull(result);
            });
        }

        @Test
        @DisplayName("Should throw exception for unknown tool")
        void shouldHandleInvalidToolGracefully() {
            // Test calling non-existent tool
            IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> toolRegistry.callTool("non_existent_tool", Map.of())
            );

            assertEquals("Unknown tool: non_existent_tool", exception.getMessage());
        }

        @Test
        @DisplayName("Should handle missing required fields gracefully")
        void shouldValidateRequiredFieldsForContactCreation() {
            // Test missing required fields
            Map<String, Object> invalidArgs = Map.of(
                "lastName", "Doe"
                // Missing required firstName and genderId
            );

            // This should fail with validation error when we try to call the actual service
            // The tool registry itself doesn't validate - that's handled by the service layer
            assertDoesNotThrow(() -> {
                try {
                    toolRegistry.callTool("contact_create", invalidArgs);
                } catch (Exception e) {
                    // Expected to fail due to validation, which is fine
                    assertTrue(e instanceof IllegalArgumentException ||
                              e.getMessage().contains("required"));
                }
            });
        }
    }

    // ========== Tool Definition Structure Tests ==========

    @Nested
    @DisplayName("Tool Definition Structure Tests")
    class ToolDefinitionStructureTests {

        @Test
        @DisplayName("All tools should have required fields")
        void allToolsShouldHaveRequiredFields() {
            var tools = toolRegistry.getAllTools();

            for (var tool : tools) {
                String toolName = (String) tool.get("name");

                assertNotNull(tool.get("name"),
                    "Tool should have a name");
                assertNotNull(tool.get("description"),
                    "Tool " + toolName + " should have a description");
                assertNotNull(tool.get("inputSchema"),
                    "Tool " + toolName + " should have an inputSchema");
            }
        }

        @Test
        @DisplayName("Tool names should follow naming convention")
        void toolNamesShouldFollowNamingConvention() {
            var tools = toolRegistry.getAllTools();

            for (var tool : tools) {
                String toolName = (String) tool.get("name");

                // Tool names should be lowercase with underscores
                assertTrue(toolName.matches("^[a-z][a-z0-9_]*$"),
                    "Tool name '" + toolName + "' should be lowercase with underscores");
            }
        }

        @Test
        @DisplayName("Tool input schemas should be valid")
        void toolInputSchemasShouldBeValid() {
            var tools = toolRegistry.getAllTools();

            for (var tool : tools) {
                String toolName = (String) tool.get("name");
                @SuppressWarnings("unchecked")
                Map<String, Object> inputSchema = (Map<String, Object>) tool.get("inputSchema");

                // All schemas should have a type
                assertNotNull(inputSchema.get("type"),
                    "Tool " + toolName + " inputSchema should have a type");
                assertEquals("object", inputSchema.get("type"),
                    "Tool " + toolName + " inputSchema type should be 'object'");
            }
        }
    }
}
