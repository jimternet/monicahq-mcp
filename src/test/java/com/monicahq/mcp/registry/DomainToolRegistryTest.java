package com.monicahq.mcp.registry;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for domain-specific tool registries.
 * Verifies that each domain registry properly registers its tools
 * and provides accurate tool counts and supported tool names.
 */
@SpringBootTest
@ActiveProfiles("test")
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class DomainToolRegistryTest {

    @Autowired
    private List<DomainToolRegistry> domainRegistries;

    @Autowired
    private ContactToolRegistry contactToolRegistry;

    @Autowired
    private ContactFieldToolRegistry contactFieldToolRegistry;

    @Autowired
    private ContactExtensionToolRegistry contactExtensionToolRegistry;

    @Autowired
    private RelationshipToolRegistry relationshipToolRegistry;

    @Autowired
    private CompanyToolRegistry companyToolRegistry;

    @Autowired
    private ActivityToolRegistry activityToolRegistry;

    @Autowired
    private CommunicationToolRegistry communicationToolRegistry;

    @Autowired
    private ProductivityToolRegistry productivityToolRegistry;

    @Autowired
    private ContentToolRegistry contentToolRegistry;

    @Autowired
    private FinancialToolRegistry financialToolRegistry;

    @Autowired
    private ReferenceDataToolRegistry referenceDataToolRegistry;

    @Autowired
    private AdminToolRegistry adminToolRegistry;

    // ========== Aggregate Registry Tests ==========

    @Test
    @DisplayName("All domain registries should be loaded")
    void shouldLoadAllDomainRegistries() {
        assertNotNull(domainRegistries);
        assertEquals(12, domainRegistries.size(),
            "Expected 12 domain registries to be loaded");
    }

    @Test
    @DisplayName("Total tools across all registries should equal 141")
    void shouldHaveCorrectTotalToolCount() {
        int totalTools = domainRegistries.stream()
            .mapToInt(registry -> registry.getTools().size())
            .sum();

        assertEquals(141, totalTools,
            "Expected 141 total tools across all domain registries, but found: " + totalTools);
    }

    @Test
    @DisplayName("No duplicate tool names should exist across registries")
    void shouldNotHaveDuplicateToolNames() {
        Set<String> allToolNames = new java.util.HashSet<>();

        for (DomainToolRegistry registry : domainRegistries) {
            for (String toolName : registry.getSupportedToolNames()) {
                boolean added = allToolNames.add(toolName);
                assertTrue(added,
                    "Duplicate tool name found: " + toolName + " in registry: " + registry.getDomain());
            }
        }
    }

    // ========== Contact Domain Registry Tests ==========

    @Nested
    @DisplayName("ContactToolRegistry Tests")
    class ContactToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(contactToolRegistry);
            assertEquals("Contact", contactToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 9 contact tools")
        void shouldRegisterCorrectToolCount() {
            assertEquals(9, contactToolRegistry.getToolCount(),
                "ContactToolRegistry should have 9 tools");
        }

        @Test
        @DisplayName("Should support expected tool names")
        void shouldSupportExpectedToolNames() {
            Set<String> supportedTools = contactToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("contact_create"));
            assertTrue(supportedTools.contains("contact_get"));
            assertTrue(supportedTools.contains("contact_update"));
            assertTrue(supportedTools.contains("contact_delete"));
            assertTrue(supportedTools.contains("contact_list"));
            assertTrue(supportedTools.contains("contact_search"));
            assertTrue(supportedTools.contains("contact_career_update"));
            assertTrue(supportedTools.contains("contact_audit_logs"));
            assertTrue(supportedTools.contains("contacts_by_tag"));
        }

        @Test
        @DisplayName("Should correctly identify supported tools")
        void shouldIdentifySupportedTools() {
            assertTrue(contactToolRegistry.supportsToolName("contact_create"));
            assertFalse(contactToolRegistry.supportsToolName("note_create"));
        }
    }

    // ========== Contact Field Registry Tests ==========

    @Nested
    @DisplayName("ContactFieldToolRegistry Tests")
    class ContactFieldToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(contactFieldToolRegistry);
            assertEquals("ContactField", contactFieldToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 7 contact field/tag tools")
        void shouldRegisterCorrectToolCount() {
            assertEquals(7, contactFieldToolRegistry.getToolCount(),
                "ContactFieldToolRegistry should have 7 tools");
        }

        @Test
        @DisplayName("Should support expected tool names")
        void shouldSupportExpectedToolNames() {
            Set<String> supportedTools = contactFieldToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("contact_field_create"));
            assertTrue(supportedTools.contains("contact_field_get"));
            assertTrue(supportedTools.contains("contact_field_update"));
            assertTrue(supportedTools.contains("contact_field_delete"));
            assertTrue(supportedTools.contains("contact_field_list"));
            assertTrue(supportedTools.contains("contacttag_add"));
            assertTrue(supportedTools.contains("contacttag_remove"));
        }
    }

    // ========== Contact Extension Registry Tests ==========

    @Nested
    @DisplayName("ContactExtensionToolRegistry Tests")
    class ContactExtensionToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(contactExtensionToolRegistry);
            assertEquals("ContactExtension", contactExtensionToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 15 extension tools (5 address + 5 group + 5 occupation)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(15, contactExtensionToolRegistry.getToolCount(),
                "ContactExtensionToolRegistry should have 15 tools");
        }

        @Test
        @DisplayName("Should support address tools")
        void shouldSupportAddressTools() {
            Set<String> supportedTools = contactExtensionToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("address_create"));
            assertTrue(supportedTools.contains("address_get"));
            assertTrue(supportedTools.contains("address_update"));
            assertTrue(supportedTools.contains("address_delete"));
            assertTrue(supportedTools.contains("address_list"));
        }

        @Test
        @DisplayName("Should support group tools")
        void shouldSupportGroupTools() {
            Set<String> supportedTools = contactExtensionToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("group_create"));
            assertTrue(supportedTools.contains("group_get"));
            assertTrue(supportedTools.contains("group_update"));
            assertTrue(supportedTools.contains("group_delete"));
            assertTrue(supportedTools.contains("group_list"));
        }

        @Test
        @DisplayName("Should support occupation tools")
        void shouldSupportOccupationTools() {
            Set<String> supportedTools = contactExtensionToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("occupation_create"));
            assertTrue(supportedTools.contains("occupation_get"));
            assertTrue(supportedTools.contains("occupation_update"));
            assertTrue(supportedTools.contains("occupation_delete"));
            assertTrue(supportedTools.contains("occupation_list"));
        }
    }

    // ========== Relationship Registry Tests ==========

    @Nested
    @DisplayName("RelationshipToolRegistry Tests")
    class RelationshipToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(relationshipToolRegistry);
            assertEquals("Relationship", relationshipToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 9 relationship tools")
        void shouldRegisterCorrectToolCount() {
            assertEquals(9, relationshipToolRegistry.getToolCount(),
                "RelationshipToolRegistry should have 9 tools");
        }

        @Test
        @DisplayName("Should support expected tool names")
        void shouldSupportExpectedToolNames() {
            Set<String> supportedTools = relationshipToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("relationship_create"));
            assertTrue(supportedTools.contains("relationship_get"));
            assertTrue(supportedTools.contains("relationship_update"));
            assertTrue(supportedTools.contains("relationship_delete"));
            assertTrue(supportedTools.contains("relationship_list"));
            assertTrue(supportedTools.contains("relationship_type_get"));
            assertTrue(supportedTools.contains("relationship_type_list"));
            assertTrue(supportedTools.contains("relationship_type_group_get"));
            assertTrue(supportedTools.contains("relationship_type_group_list"));
        }
    }

    // ========== Company Registry Tests ==========

    @Nested
    @DisplayName("CompanyToolRegistry Tests")
    class CompanyToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(companyToolRegistry);
            assertEquals("Company", companyToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 5 company tools")
        void shouldRegisterCorrectToolCount() {
            assertEquals(5, companyToolRegistry.getToolCount(),
                "CompanyToolRegistry should have 5 tools");
        }

        @Test
        @DisplayName("Should support expected tool names")
        void shouldSupportExpectedToolNames() {
            Set<String> supportedTools = companyToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("company_create"));
            assertTrue(supportedTools.contains("company_get"));
            assertTrue(supportedTools.contains("company_update"));
            assertTrue(supportedTools.contains("company_delete"));
            assertTrue(supportedTools.contains("company_list"));
        }
    }

    // ========== Activity Registry Tests ==========

    @Nested
    @DisplayName("ActivityToolRegistry Tests")
    class ActivityToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(activityToolRegistry);
            assertEquals("Activity", activityToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 15 activity tools (5 activity + 5 type + 5 category)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(15, activityToolRegistry.getToolCount(),
                "ActivityToolRegistry should have 15 tools");
        }

        @Test
        @DisplayName("Should support activity tools")
        void shouldSupportActivityTools() {
            Set<String> supportedTools = activityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("activity_create"));
            assertTrue(supportedTools.contains("activity_get"));
            assertTrue(supportedTools.contains("activity_update"));
            assertTrue(supportedTools.contains("activity_delete"));
            assertTrue(supportedTools.contains("activity_list"));
        }

        @Test
        @DisplayName("Should support activity type tools")
        void shouldSupportActivityTypeTools() {
            Set<String> supportedTools = activityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("activity_type_create"));
            assertTrue(supportedTools.contains("activity_type_get"));
            assertTrue(supportedTools.contains("activity_type_update"));
            assertTrue(supportedTools.contains("activity_type_delete"));
            assertTrue(supportedTools.contains("activity_type_list"));
        }

        @Test
        @DisplayName("Should support activity type category tools")
        void shouldSupportActivityTypeCategoryTools() {
            Set<String> supportedTools = activityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("activity_type_category_create"));
            assertTrue(supportedTools.contains("activity_type_category_get"));
            assertTrue(supportedTools.contains("activity_type_category_update"));
            assertTrue(supportedTools.contains("activity_type_category_delete"));
            assertTrue(supportedTools.contains("activity_type_category_list"));
        }
    }

    // ========== Communication Registry Tests ==========

    @Nested
    @DisplayName("CommunicationToolRegistry Tests")
    class CommunicationToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(communicationToolRegistry);
            assertEquals("Communication", communicationToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 15 communication tools (5 call + 5 conversation + 5 message)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(15, communicationToolRegistry.getToolCount(),
                "CommunicationToolRegistry should have 15 tools");
        }

        @Test
        @DisplayName("Should support call tools")
        void shouldSupportCallTools() {
            Set<String> supportedTools = communicationToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("call_create"));
            assertTrue(supportedTools.contains("call_get"));
            assertTrue(supportedTools.contains("call_update"));
            assertTrue(supportedTools.contains("call_delete"));
            assertTrue(supportedTools.contains("call_list"));
        }

        @Test
        @DisplayName("Should support conversation tools")
        void shouldSupportConversationTools() {
            Set<String> supportedTools = communicationToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("conversation_create"));
            assertTrue(supportedTools.contains("conversation_get"));
            assertTrue(supportedTools.contains("conversation_update"));
            assertTrue(supportedTools.contains("conversation_delete"));
            assertTrue(supportedTools.contains("conversation_list"));
        }

        @Test
        @DisplayName("Should support conversation message tools")
        void shouldSupportConversationMessageTools() {
            Set<String> supportedTools = communicationToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("conversation_message_create"));
            assertTrue(supportedTools.contains("conversation_message_get"));
            assertTrue(supportedTools.contains("conversation_message_update"));
            assertTrue(supportedTools.contains("conversation_message_delete"));
            assertTrue(supportedTools.contains("conversation_message_list"));
        }
    }

    // ========== Productivity Registry Tests ==========

    @Nested
    @DisplayName("ProductivityToolRegistry Tests")
    class ProductivityToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(productivityToolRegistry);
            assertEquals("Productivity", productivityToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 20 productivity tools (5 note + 5 task + 5 reminder + 5 tag)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(20, productivityToolRegistry.getToolCount(),
                "ProductivityToolRegistry should have 20 tools");
        }

        @Test
        @DisplayName("Should support note tools")
        void shouldSupportNoteTools() {
            Set<String> supportedTools = productivityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("note_create"));
            assertTrue(supportedTools.contains("note_get"));
            assertTrue(supportedTools.contains("note_update"));
            assertTrue(supportedTools.contains("note_delete"));
            assertTrue(supportedTools.contains("note_list"));
        }

        @Test
        @DisplayName("Should support task tools")
        void shouldSupportTaskTools() {
            Set<String> supportedTools = productivityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("task_create"));
            assertTrue(supportedTools.contains("task_get"));
            assertTrue(supportedTools.contains("task_update"));
            assertTrue(supportedTools.contains("task_delete"));
            assertTrue(supportedTools.contains("task_list"));
        }

        @Test
        @DisplayName("Should support reminder tools")
        void shouldSupportReminderTools() {
            Set<String> supportedTools = productivityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("reminder_create"));
            assertTrue(supportedTools.contains("reminder_get"));
            assertTrue(supportedTools.contains("reminder_update"));
            assertTrue(supportedTools.contains("reminder_delete"));
            assertTrue(supportedTools.contains("reminder_list"));
        }

        @Test
        @DisplayName("Should support tag tools")
        void shouldSupportTagTools() {
            Set<String> supportedTools = productivityToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("tag_create"));
            assertTrue(supportedTools.contains("tag_get"));
            assertTrue(supportedTools.contains("tag_update"));
            assertTrue(supportedTools.contains("tag_delete"));
            assertTrue(supportedTools.contains("tag_list"));
        }
    }

    // ========== Content Registry Tests ==========

    @Nested
    @DisplayName("ContentToolRegistry Tests")
    class ContentToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(contentToolRegistry);
            assertEquals("Content", contentToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 15 content tools (5 document + 5 photo + 5 pet)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(15, contentToolRegistry.getToolCount(),
                "ContentToolRegistry should have 15 tools");
        }

        @Test
        @DisplayName("Should support document tools")
        void shouldSupportDocumentTools() {
            Set<String> supportedTools = contentToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("document_create"));
            assertTrue(supportedTools.contains("document_get"));
            assertTrue(supportedTools.contains("document_update"));
            assertTrue(supportedTools.contains("document_delete"));
            assertTrue(supportedTools.contains("document_list"));
        }

        @Test
        @DisplayName("Should support photo tools")
        void shouldSupportPhotoTools() {
            Set<String> supportedTools = contentToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("photo_create"));
            assertTrue(supportedTools.contains("photo_get"));
            assertTrue(supportedTools.contains("photo_update"));
            assertTrue(supportedTools.contains("photo_delete"));
            assertTrue(supportedTools.contains("photo_list"));
        }

        @Test
        @DisplayName("Should support pet tools")
        void shouldSupportPetTools() {
            Set<String> supportedTools = contentToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("pet_create"));
            assertTrue(supportedTools.contains("pet_get"));
            assertTrue(supportedTools.contains("pet_update"));
            assertTrue(supportedTools.contains("pet_delete"));
            assertTrue(supportedTools.contains("pet_list"));
        }
    }

    // ========== Financial Registry Tests ==========

    @Nested
    @DisplayName("FinancialToolRegistry Tests")
    class FinancialToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(financialToolRegistry);
            assertEquals("Financial", financialToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 10 financial tools (5 debt + 5 gift)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(10, financialToolRegistry.getToolCount(),
                "FinancialToolRegistry should have 10 tools");
        }

        @Test
        @DisplayName("Should support debt tools")
        void shouldSupportDebtTools() {
            Set<String> supportedTools = financialToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("debt_create"));
            assertTrue(supportedTools.contains("debt_get"));
            assertTrue(supportedTools.contains("debt_update"));
            assertTrue(supportedTools.contains("debt_delete"));
            assertTrue(supportedTools.contains("debt_list"));
        }

        @Test
        @DisplayName("Should support gift tools")
        void shouldSupportGiftTools() {
            Set<String> supportedTools = financialToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("gift_create"));
            assertTrue(supportedTools.contains("gift_get"));
            assertTrue(supportedTools.contains("gift_update"));
            assertTrue(supportedTools.contains("gift_delete"));
            assertTrue(supportedTools.contains("gift_list"));
        }
    }

    // ========== Reference Data Registry Tests ==========

    @Nested
    @DisplayName("ReferenceDataToolRegistry Tests")
    class ReferenceDataToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(referenceDataToolRegistry);
            assertEquals("ReferenceData", referenceDataToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 11 reference data tools")
        void shouldRegisterCorrectToolCount() {
            assertEquals(11, referenceDataToolRegistry.getToolCount(),
                "ReferenceDataToolRegistry should have 11 tools");
        }

        @Test
        @DisplayName("Should support audit log tools")
        void shouldSupportAuditLogTools() {
            Set<String> supportedTools = referenceDataToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("auditlog_get"));
            assertTrue(supportedTools.contains("auditlog_list"));
            assertTrue(supportedTools.contains("auditlog_search"));
        }

        @Test
        @DisplayName("Should support country tools")
        void shouldSupportCountryTools() {
            Set<String> supportedTools = referenceDataToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("country_get"));
            assertTrue(supportedTools.contains("country_list"));
            assertTrue(supportedTools.contains("country_search"));
        }

        @Test
        @DisplayName("Should support currency tools")
        void shouldSupportCurrencyTools() {
            Set<String> supportedTools = referenceDataToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("currency_get"));
            assertTrue(supportedTools.contains("currency_list"));
            assertTrue(supportedTools.contains("currency_search"));
        }

        @Test
        @DisplayName("Should support discovery tools")
        void shouldSupportDiscoveryTools() {
            Set<String> supportedTools = referenceDataToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("gender_list"));
            assertTrue(supportedTools.contains("contact_field_type_list"));
        }
    }

    // ========== Admin Registry Tests ==========

    @Nested
    @DisplayName("AdminToolRegistry Tests")
    class AdminToolRegistryTests {

        @Test
        @DisplayName("Should be initialized")
        void shouldBeInitialized() {
            assertNotNull(adminToolRegistry);
            assertEquals("Admin", adminToolRegistry.getDomain());
        }

        @Test
        @DisplayName("Should register 10 admin tools (5 user + 5 compliance)")
        void shouldRegisterCorrectToolCount() {
            assertEquals(10, adminToolRegistry.getToolCount(),
                "AdminToolRegistry should have 10 tools");
        }

        @Test
        @DisplayName("Should support user tools")
        void shouldSupportUserTools() {
            Set<String> supportedTools = adminToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("user_create"));
            assertTrue(supportedTools.contains("user_get"));
            assertTrue(supportedTools.contains("user_update"));
            assertTrue(supportedTools.contains("user_delete"));
            assertTrue(supportedTools.contains("user_list"));
        }

        @Test
        @DisplayName("Should support compliance tools")
        void shouldSupportComplianceTools() {
            Set<String> supportedTools = adminToolRegistry.getSupportedToolNames();

            assertTrue(supportedTools.contains("compliance_create"));
            assertTrue(supportedTools.contains("compliance_get"));
            assertTrue(supportedTools.contains("compliance_update"));
            assertTrue(supportedTools.contains("compliance_delete"));
            assertTrue(supportedTools.contains("compliance_list"));
        }
    }

    // ========== ToolDefinition Tests ==========

    @Nested
    @DisplayName("ToolDefinition Tests")
    class ToolDefinitionTests {

        @Test
        @DisplayName("Tool definitions should have valid structure")
        void toolDefinitionsShouldHaveValidStructure() {
            for (DomainToolRegistry registry : domainRegistries) {
                for (ToolDefinition tool : registry.getTools()) {
                    assertNotNull(tool.name(), "Tool name should not be null");
                    assertFalse(tool.name().isEmpty(), "Tool name should not be empty");
                    assertNotNull(tool.description(), "Tool description should not be null");
                    assertFalse(tool.description().isEmpty(), "Tool description should not be empty");
                    assertNotNull(tool.inputSchema(), "Tool inputSchema should not be null");
                }
            }
        }

        @Test
        @DisplayName("Tool definitions should convert to map correctly")
        void toolDefinitionsShouldConvertToMap() {
            ToolDefinition contactCreate = contactToolRegistry.getTools().stream()
                .filter(t -> "contact_create".equals(t.name()))
                .findFirst()
                .orElseThrow();

            var map = contactCreate.toMap();

            assertEquals("contact_create", map.get("name"));
            assertNotNull(map.get("description"));
            assertNotNull(map.get("inputSchema"));
        }
    }
}
