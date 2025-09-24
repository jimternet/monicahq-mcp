package com.monicahq.mcp.controller;

import com.monicahq.mcp.service.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
@Slf4j
public class McpToolRegistry {

    private final Map<String, McpTool> tools = new ConcurrentHashMap<>();
    
    // Service dependencies
    private final ContactService contactService;
    private final ActivityService activityService;
    private final CallService callService;
    private final NoteService noteService;
    private final TaskService taskService;
    private final TagService tagService;
    private final ReminderService reminderService;
    private final JournalEntryService journalEntryService;
    private final ConversationService conversationService;
    private final ConversationMessageService conversationMessageService;
    private final ContactFieldService contactFieldService;
    private final ContactTagService contactTagService;
    
    // Relationship and Company services - Critical Gap Implementation
    private final RelationshipService relationshipService;
    private final CompanyService companyService;
    private final RelationshipTypeService relationshipTypeService;
    private final RelationshipTypeGroupService relationshipTypeGroupService;
    
    // Phase 4.1: Core Missing Entities
    private final ActivityTypeService activityTypeService;
    private final ActivityTypeCategoryService activityTypeCategoryService;
    private final AddressService addressService;
    private final GroupService groupService;
    private final OccupationService occupationService;
    
    // Phase 4.2: Financial & Content Management
    private final DebtService debtService;
    private final DocumentService documentService;
    private final PhotoService photoService;
    private final GiftService giftService;
    
    // Phase 4.3: Reference Data Management
    private final AuditLogService auditLogService;
    private final CountryService countryService;
    private final CurrencyService currencyService;
    
    // Discovery services - Constitutional Principle VII
    private final GenderService genderService;
    private final ContactFieldTypeService contactFieldTypeService;
    
    // API Gap Fix services - Missing Operations Implementation
    private final UserService userService;
    private final ComplianceService complianceService;

    @PostConstruct
    public void initializeTools() {
        log.info("Initializing MCP tool registry with 136 operations"); // 122 + 14 new API gap fix operations
        
        // === CONTACT MANAGEMENT (12 operations) ===
        // Core contact operations (5)
        registerTool("contact_create", "[Contact] Create a new contact in MonicaHQ", createContactSchema(), "Contact Management");
        registerTool("contact_get", "[Contact] Get a contact by ID", createIdSchema("Contact ID"), "Contact Management");
        registerTool("contact_update", "[Contact] Update an existing contact", createContactUpdateSchema(), "Contact Management");
        registerTool("contact_delete", "[Contact] Delete a contact", createIdSchema("Contact ID"), "Contact Management");
        registerTool("contact_list", "[Contact] List contacts with pagination", createListSchema(), "Contact Management");
        
        // Contact field operations (5)
        registerTool("contact_field_create", "[Contact Field] Create a new contact field", createContactFieldSchema(), "Contact Management");
        registerTool("contact_field_get", "[Contact Field] Get a contact field by ID", createIdSchema("Contact Field ID"), "Contact Management");
        registerTool("contact_field_update", "[Contact Field] Update an existing contact field", createContactFieldUpdateSchema(), "Contact Management");
        registerTool("contact_field_delete", "[Contact Field] Delete a contact field", createIdSchema("Contact Field ID"), "Contact Management");
        registerTool("contact_field_list", "[Contact Field] List contact fields for a contact", createContactFieldListSchema(), "Contact Management");
        
        // Contact tag operations (2)
        registerTool("contacttag_add", "[Contact Tag] Add a tag to a contact", createContactTagSchema(), "Contact Management");
        registerTool("contacttag_remove", "[Contact Tag] Remove a tag from a contact", createContactTagSchema(), "Contact Management");
        
        // Contact gap fix operations (4) - API Gap Implementation
        registerTool("contact_search", "[Contact] Search contacts by query", createContactSearchSchema(), "Contact Management");
        registerTool("contact_career_update", "[Contact] Update contact career/work information", createContactCareerSchema(), "Contact Management");
        registerTool("contact_audit_logs", "[Contact] Get contact audit logs/history", createContactAuditLogSchema(), "Contact Management");
        registerTool("contacts_by_tag", "[Contact] List contacts filtered by tag", createContactsByTagSchema(), "Contact Management");
        
        // === RELATIONSHIP MANAGEMENT (9 operations) ===
        // Relationship operations (5)
        registerTool("relationship_create", "[Relationship] Create a new relationship between contacts", createRelationshipSchema(), "Relationship Management");
        registerTool("relationship_get", "[Relationship] Get a relationship by ID", createIdSchema("Relationship ID"), "Relationship Management");
        registerTool("relationship_update", "[Relationship] Update an existing relationship", createRelationshipUpdateSchema(), "Relationship Management");
        registerTool("relationship_delete", "[Relationship] Delete a relationship", createIdSchema("Relationship ID"), "Relationship Management");
        registerTool("relationship_list", "[Relationship] List relationships with pagination", createListSchema(), "Relationship Management");
        
        // Relationship discovery operations (4)
        registerTool("relationship_type_get", "[Discovery] Get a relationship type by ID", createIdSchema("Relationship Type ID"), "Relationship Management");
        registerTool("relationship_type_list", "[Discovery] List all available relationship types", createListSchema(), "Relationship Management");
        registerTool("relationship_type_group_get", "[Discovery] Get a relationship type group by ID", createIdSchema("Relationship Type Group ID"), "Relationship Management");
        registerTool("relationship_type_group_list", "[Discovery] List all relationship type groups", createListSchema(), "Relationship Management");
        
        // === COMPANY MANAGEMENT (5 operations) ===
        registerTool("company_create", "[Company] Create a new company", createCompanySchema(), "Company Management");
        registerTool("company_get", "[Company] Get a company by ID", createIdSchema("Company ID"), "Company Management");
        registerTool("company_update", "[Company] Update an existing company", createCompanyUpdateSchema(), "Company Management");
        registerTool("company_delete", "[Company] Delete a company", createIdSchema("Company ID"), "Company Management");
        registerTool("company_list", "[Company] List companies with pagination", createListSchema(), "Company Management");
        
        // === ACTIVITY TYPE MANAGEMENT (8 operations) ===
        // Activity type operations (4)
        registerTool("activity_type_create", "[Activity Type] Create a new activity type", createActivityTypeSchema(), "Activity Management");
        registerTool("activity_type_get", "[Activity Type] Get activity type by ID", createIdSchema("Activity Type ID"), "Activity Management");
        registerTool("activity_type_update", "[Activity Type] Update existing activity type", createActivityTypeUpdateSchema(), "Activity Management");
        registerTool("activity_type_delete", "[Activity Type] Delete activity type", createIdSchema("Activity Type ID"), "Activity Management");
        registerTool("activity_type_list", "[Activity Type] List activity types with pagination", createListSchema(), "Activity Management");
        
        // Activity type category operations (4)
        registerTool("activity_type_category_create", "[Activity Category] Create a new activity type category", createActivityTypeCategorySchema(), "Activity Management");
        registerTool("activity_type_category_get", "[Activity Category] Get activity type category by ID", createIdSchema("Activity Type Category ID"), "Activity Management");
        registerTool("activity_type_category_update", "[Activity Category] Update existing activity type category", createActivityTypeCategoryUpdateSchema(), "Activity Management");
        registerTool("activity_type_category_delete", "[Activity Category] Delete activity type category", createIdSchema("Activity Type Category ID"), "Activity Management");
        registerTool("activity_type_category_list", "[Activity Category] List activity type categories with pagination", createListSchema(), "Activity Management");
        
        // === ADDRESS MANAGEMENT (5 operations) ===
        registerTool("address_create", "[Address] Create a new address for a contact", createAddressSchema(), "Contact Management");
        registerTool("address_get", "[Address] Get an address by ID", createIdSchema("Address ID"), "Contact Management");
        registerTool("address_update", "[Address] Update an existing address", createAddressUpdateSchema(), "Contact Management");
        registerTool("address_delete", "[Address] Delete an address", createIdSchema("Address ID"), "Contact Management");
        registerTool("address_list", "[Address] List addresses with pagination", createListSchema(), "Contact Management");
        
        // === GROUP MANAGEMENT (5 operations) ===
        registerTool("group_create", "[Group] Create a new contact group", createGroupSchema(), "Contact Management");
        registerTool("group_get", "[Group] Get a group by ID", createIdSchema("Group ID"), "Contact Management");
        registerTool("group_update", "[Group] Update an existing group", createGroupUpdateSchema(), "Contact Management");
        registerTool("group_delete", "[Group] Delete a group", createIdSchema("Group ID"), "Contact Management");
        registerTool("group_list", "[Group] List groups with pagination", createListSchema(), "Contact Management");
        
        // === OCCUPATION MANAGEMENT (5 operations) ===
        registerTool("occupation_create", "[Occupation] Create a new occupation/job for a contact", createOccupationSchema(), "Contact Management");
        registerTool("occupation_get", "[Occupation] Get an occupation by ID", createIdSchema("Occupation ID"), "Contact Management");
        registerTool("occupation_update", "[Occupation] Update an existing occupation", createOccupationUpdateSchema(), "Contact Management");
        registerTool("occupation_delete", "[Occupation] Delete an occupation", createIdSchema("Occupation ID"), "Contact Management");
        registerTool("occupation_list", "[Occupation] List occupations with pagination", createListSchema(), "Contact Management");
        
        // === DEBT MANAGEMENT (5 operations) ===
        registerTool("debt_create", "[Debt] Create a new debt record", createDebtSchema(), "Financial Management");
        registerTool("debt_get", "[Debt] Get a debt by ID", createIdSchema("Debt ID"), "Financial Management");
        registerTool("debt_update", "[Debt] Update an existing debt", createDebtUpdateSchema(), "Financial Management");
        registerTool("debt_delete", "[Debt] Delete a debt", createIdSchema("Debt ID"), "Financial Management");
        registerTool("debt_list", "[Debt] List debts with pagination", createListSchema(), "Financial Management");
        
        // === DOCUMENT MANAGEMENT (5 operations) ===
        registerTool("document_create", "[Document] Create a new document", createDocumentSchema(), "Content Management");
        registerTool("document_get", "[Document] Get a document by ID", createIdSchema("Document ID"), "Content Management");
        registerTool("document_update", "[Document] Update an existing document", createDocumentUpdateSchema(), "Content Management");
        registerTool("document_delete", "[Document] Delete a document", createIdSchema("Document ID"), "Content Management");
        registerTool("document_list", "[Document] List documents with pagination", createListSchema(), "Content Management");
        
        // === PHOTO MANAGEMENT (5 operations) ===
        registerTool("photo_create", "[Photo] Create a new photo", createPhotoSchema(), "Content Management");
        registerTool("photo_get", "[Photo] Get a photo by ID", createIdSchema("Photo ID"), "Content Management");
        registerTool("photo_update", "[Photo] Update an existing photo", createPhotoUpdateSchema(), "Content Management");
        registerTool("photo_delete", "[Photo] Delete a photo", createIdSchema("Photo ID"), "Content Management");
        registerTool("photo_list", "[Photo] List photos with pagination", createListSchema(), "Content Management");
        
        // === GIFT MANAGEMENT (5 operations) ===
        registerTool("gift_create", "[Gift] Create a new gift record", createGiftSchema(), "Gift Management");
        registerTool("gift_get", "[Gift] Get a gift by ID", createIdSchema("Gift ID"), "Gift Management");
        registerTool("gift_update", "[Gift] Update an existing gift", createGiftUpdateSchema(), "Gift Management");
        registerTool("gift_delete", "[Gift] Delete a gift", createIdSchema("Gift ID"), "Gift Management");
        registerTool("gift_list", "[Gift] List gifts with pagination", createListSchema(), "Gift Management");
        
        // === AUDIT LOG MANAGEMENT (3 operations) ===
        registerTool("auditlog_get", "[Audit Log] Get an audit log by ID", createIdSchema("Audit Log ID"), "System & Reference");
        registerTool("auditlog_list", "[Audit Log] List audit logs with pagination", createListSchema(), "System & Reference");
        registerTool("auditlog_search", "[Audit Log] Search audit logs by criteria", createAuditLogSearchSchema(), "System & Reference");
        
        // === COUNTRY REFERENCE DATA (3 operations) ===
        registerTool("country_get", "[Country] Get a country by ID", createIdSchema("Country ID"), "System & Reference");
        registerTool("country_list", "[Country] List all countries", createListSchema(), "System & Reference");
        registerTool("country_search", "[Country] Search countries by name", createCountrySearchSchema(), "System & Reference");
        
        // === CURRENCY REFERENCE DATA (3 operations) ===
        registerTool("currency_get", "[Currency] Get a currency by ID", createIdSchema("Currency ID"), "System & Reference");
        registerTool("currency_list", "[Currency] List all currencies", createListSchema(), "System & Reference");
        registerTool("currency_search", "[Currency] Search currencies by code or name", createCurrencySearchSchema(), "System & Reference");
        
        // === DISCOVERY TOOLS (Constitutional Principle VII) ===
        registerTool("gender_list", "[Discovery] List all available genders", createListOnlySchema(), "Discovery & Reference");
        registerTool("contact_field_type_list", "[Discovery] List all available contact field types", createListOnlySchema(), "Discovery & Reference");
        
        // === USER MANAGEMENT (5 operations) ===
        // Note: Users API may not be available in all Monica configurations
        registerTool("user_create", "[User] Create a new user account", createUserSchema(), "User Management");
        registerTool("user_get", "[User] Get a user by ID", createIdSchema("User ID"), "User Management");
        registerTool("user_update", "[User] Update an existing user", createUserUpdateSchema(), "User Management");
        registerTool("user_delete", "[User] Delete a user", createIdSchema("User ID"), "User Management");
        registerTool("user_list", "[User] List users with pagination", createListSchema(), "User Management");
        
        // === COMPLIANCE MANAGEMENT (5 operations) ===
        // Note: Compliance API endpoints may not be clearly defined in all Monica versions
        registerTool("compliance_create", "[Compliance] Create a new compliance record", createComplianceSchema(), "Compliance Management");
        registerTool("compliance_get", "[Compliance] Get a compliance record by ID", createIdSchema("Compliance ID"), "Compliance Management");
        registerTool("compliance_update", "[Compliance] Update an existing compliance record", createComplianceUpdateSchema(), "Compliance Management");
        registerTool("compliance_delete", "[Compliance] Delete a compliance record", createIdSchema("Compliance ID"), "Compliance Management");
        registerTool("compliance_list", "[Compliance] List compliance records with pagination", createListSchema(), "Compliance Management");
        
        // === PRODUCTIVITY & ORGANIZATION (20 operations) ===
        // Note operations (5) - frequently used with contacts
        registerTool("note_create", "[Note] Create a new note", createNoteSchema(), "Productivity & Organization");
        registerTool("note_get", "[Note] Get a note by ID", createIdSchema("Note ID"), "Productivity & Organization");
        registerTool("note_update", "[Note] Update an existing note", createNoteUpdateSchema(), "Productivity & Organization");
        registerTool("note_delete", "[Note] Delete a note", createIdSchema("Note ID"), "Productivity & Organization");
        registerTool("note_list", "[Note] List notes with pagination", createListSchema(), "Productivity & Organization");
        
        // Task operations (5)
        registerTool("task_create", "[Task] Create a new task", createTaskSchema(), "Productivity & Organization");
        registerTool("task_get", "[Task] Get a task by ID", createIdSchema("Task ID"), "Productivity & Organization");
        registerTool("task_update", "[Task] Update an existing task", createTaskUpdateSchema(), "Productivity & Organization");
        registerTool("task_delete", "[Task] Delete a task", createIdSchema("Task ID"), "Productivity & Organization");
        registerTool("task_list", "[Task] List tasks with pagination", createListSchema(), "Productivity & Organization");
        
        // Reminder operations (5)
        registerTool("reminder_create", "[Reminder] Create a new reminder", createReminderSchema(), "Productivity & Organization");
        registerTool("reminder_get", "[Reminder] Get a reminder by ID", createIdSchema("Reminder ID"), "Productivity & Organization");
        registerTool("reminder_update", "[Reminder] Update an existing reminder", createReminderUpdateSchema(), "Productivity & Organization");
        registerTool("reminder_delete", "[Reminder] Delete a reminder", createIdSchema("Reminder ID"), "Productivity & Organization");
        registerTool("reminder_list", "[Reminder] List reminders with pagination", createListSchema(), "Productivity & Organization");
        
        // Tag operations (5)
        registerTool("tag_create", "[Tag] Create a new tag", createTagSchema(), "Productivity & Organization");
        registerTool("tag_get", "[Tag] Get a tag by ID", createIdSchema("Tag ID"), "Productivity & Organization");
        registerTool("tag_update", "[Tag] Update an existing tag", createTagUpdateSchema(), "Productivity & Organization");
        registerTool("tag_delete", "[Tag] Delete a tag", createIdSchema("Tag ID"), "Productivity & Organization");
        registerTool("tag_list", "[Tag] List tags with pagination", createListSchema(), "Productivity & Organization");
        
        // Journal Entry operations (5) - DISABLED: MonicaHQ doesn't support journal entries
        // registerTool("journal_entry_create", "Create a new journal entry", createJournalSchema());
        // registerTool("journal_entry_get", "Get a journal entry by ID", createIdSchema("Journal Entry ID"));
        // registerTool("journal_entry_update", "Update an existing journal entry", createJournalUpdateSchema());
        // registerTool("journal_entry_delete", "Delete a journal entry", createIdSchema("Journal Entry ID"));
        // registerTool("journal_entry_list", "List journal entries with pagination", createListSchema());
        
        // === ACTIVITY & COMMUNICATION (18 operations) ===
        // Activity operations (5)
        registerTool("activity_create", "[Activity] Create a new activity", createActivitySchema(), "Activity & Communication");
        registerTool("activity_get", "[Activity] Get an activity by ID", createIdSchema("Activity ID"), "Activity & Communication");
        registerTool("activity_update", "[Activity] Update an existing activity", createActivityUpdateSchema(), "Activity & Communication");
        registerTool("activity_delete", "[Activity] Delete an activity", createIdSchema("Activity ID"), "Activity & Communication");
        registerTool("activity_list", "[Activity] List activities with pagination", createListSchema(), "Activity & Communication");
        
        // Call operations (5)
        registerTool("call_create", "[Call] Create a new call record", createCallSchema(), "Activity & Communication");
        registerTool("call_get", "[Call] Get a call by ID", createIdSchema("Call ID"), "Activity & Communication");
        registerTool("call_update", "[Call] Update an existing call", createCallUpdateSchema(), "Activity & Communication");
        registerTool("call_delete", "[Call] Delete a call", createIdSchema("Call ID"), "Activity & Communication");
        registerTool("call_list", "[Call] List calls with pagination", createListSchema(), "Activity & Communication");
        
        // Conversation operations (5)
        registerTool("conversation_create", "[Conversation] Create a new conversation", createConversationSchema(), "Activity & Communication");
        registerTool("conversation_get", "[Conversation] Get a conversation by ID", createIdSchema("Conversation ID"), "Activity & Communication");
        registerTool("conversation_update", "[Conversation] Update an existing conversation", createConversationUpdateSchema(), "Activity & Communication");
        registerTool("conversation_delete", "[Conversation] Delete a conversation", createIdSchema("Conversation ID"), "Activity & Communication");
        registerTool("conversation_list", "[Conversation] List conversations with pagination", createListSchema(), "Activity & Communication");
        
        // Conversation message operations (5)
        registerTool("conversation_message_create", "[Message] Create a new conversation message", createMessageSchema(), "Activity & Communication");
        registerTool("conversation_message_get", "[Message] Get a conversation message by ID", createMessageGetSchema(), "Activity & Communication");
        registerTool("conversation_message_update", "[Message] Update an existing conversation message", createMessageUpdateSchema(), "Activity & Communication");
        registerTool("conversation_message_delete", "[Message] Delete a conversation message", createMessageGetSchema(), "Activity & Communication");
        registerTool("conversation_message_list", "[Message] List conversation messages", createMessageListSchema(), "Activity & Communication");
        
        log.info("Initialized {} MCP tools", tools.size());
    }

    private void registerTool(String name, String description, Map<String, Object> inputSchema) {
        registerTool(name, description, inputSchema, null);
    }
    
    private void registerTool(String name, String description, Map<String, Object> inputSchema, String category) {
        McpTool tool = new McpTool(name, description, inputSchema, category);
        tools.put(name, tool);
        log.debug("Registered MCP tool: {} (category: {})", name, category);
    }

    public List<Map<String, Object>> getAllTools() {
        return tools.values().stream()
            .map(McpTool::toMap)
            .toList();
    }

    public Object callTool(String toolName, Map<String, Object> arguments) {
        McpTool tool = tools.get(toolName);
        if (tool == null) {
            throw new IllegalArgumentException("Unknown tool: " + toolName);
        }
        
        log.info("Executing MCP tool: {} with arguments: {}", toolName, arguments);
        
        try {
            Mono<Map<String, Object>> result = executeToolOperation(toolName, arguments);
            return result.block(); // Blocking call for synchronous MCP response
            
        } catch (Exception e) {
            log.error("Error executing tool {}: {}", toolName, e.getMessage(), e);
            throw e;
        }
    }
    
    private Mono<Map<String, Object>> executeToolOperation(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Contact operations
            case "contact_create" -> contactService.createContact(arguments);
            case "contact_get" -> contactService.getContact(arguments);
            case "contact_update" -> contactService.updateContact(arguments);
            case "contact_delete" -> contactService.deleteContact(arguments);
            case "contact_list" -> contactService.listContacts(arguments);
            
            // Contact gap fix operations - API Gap Implementation
            case "contact_search" -> contactService.searchContacts(arguments);
            case "contact_career_update" -> contactService.updateContactCareer(arguments);
            case "contact_audit_logs" -> contactService.getContactAuditLogs(arguments);
            case "contacts_by_tag" -> tagService.listContactsByTag(arguments);
            
            // Activity operations
            case "activity_create" -> activityService.createActivity(arguments);
            case "activity_get" -> activityService.getActivity(arguments);
            case "activity_update" -> activityService.updateActivity(arguments);
            case "activity_delete" -> activityService.deleteActivity(arguments);
            case "activity_list" -> activityService.listActivities(arguments);
            
            // Call operations
            case "call_create" -> callService.createCall(arguments);
            case "call_get" -> callService.getCall(arguments);
            case "call_update" -> callService.updateCall(arguments);
            case "call_delete" -> callService.deleteCall(arguments);
            case "call_list" -> callService.listCalls(arguments);
            
            // Note operations
            case "note_create" -> noteService.createNote(arguments);
            case "note_get" -> noteService.getNote(arguments);
            case "note_update" -> noteService.updateNote(arguments);
            case "note_delete" -> noteService.deleteNote(arguments);
            case "note_list" -> noteService.listNotes(arguments);
            
            // Task operations
            case "task_create" -> taskService.createTask(arguments);
            case "task_get" -> taskService.getTask(arguments);
            case "task_update" -> taskService.updateTask(arguments);
            case "task_delete" -> taskService.deleteTask(arguments);
            case "task_list" -> taskService.listTasks(arguments);
            
            // Tag operations
            case "tag_create" -> tagService.createTag(arguments);
            case "tag_get" -> tagService.getTag(arguments);
            case "tag_update" -> tagService.updateTag(arguments);
            case "tag_delete" -> tagService.deleteTag(arguments);
            case "tag_list" -> tagService.listTags(arguments);
            
            // Reminder operations
            case "reminder_create" -> reminderService.createReminder(arguments);
            case "reminder_get" -> reminderService.getReminder(arguments);
            case "reminder_update" -> reminderService.updateReminder(arguments);
            case "reminder_delete" -> reminderService.deleteReminder(arguments);
            case "reminder_list" -> reminderService.listReminders(arguments);
            
            // Journal Entry operations - DISABLED: MonicaHQ doesn't support journal entries
            // case "journal_entry_create" -> journalEntryService.createJournalEntry(arguments);
            // case "journal_entry_get" -> journalEntryService.getJournalEntry(arguments);
            // case "journal_entry_update" -> journalEntryService.updateJournalEntry(arguments);
            // case "journal_entry_delete" -> journalEntryService.deleteJournalEntry(arguments);
            // case "journal_entry_list" -> journalEntryService.listJournalEntries(arguments);
            
            // Conversation operations
            case "conversation_create" -> conversationService.createConversation(arguments);
            case "conversation_get" -> conversationService.getConversation(arguments);
            case "conversation_update" -> conversationService.updateConversation(arguments);
            case "conversation_delete" -> conversationService.deleteConversation(arguments);
            case "conversation_list" -> conversationService.listConversations(arguments);
            
            // Conversation Message operations
            case "conversation_message_create" -> conversationMessageService.createConversationMessage(arguments);
            case "conversation_message_get" -> conversationMessageService.getConversationMessage(arguments);
            case "conversation_message_update" -> conversationMessageService.updateConversationMessage(arguments);
            case "conversation_message_delete" -> conversationMessageService.deleteConversationMessage(arguments);
            case "conversation_message_list" -> conversationMessageService.listConversationMessages(arguments);
            
            // Contact Field operations
            case "contact_field_create" -> contactFieldService.createContactField(arguments);
            case "contact_field_get" -> contactFieldService.getContactField(arguments);
            case "contact_field_update" -> contactFieldService.updateContactField(arguments);
            case "contact_field_delete" -> contactFieldService.deleteContactField(arguments);
            case "contact_field_list" -> contactFieldService.listContactFields(arguments);
            
            // Contact Tag operations
            case "contacttag_add" -> contactTagService.attachTag(arguments);
            case "contacttag_remove" -> contactTagService.detachTag(arguments);
            
            // Relationship operations
            case "relationship_create" -> relationshipService.createRelationship(arguments);
            case "relationship_get" -> relationshipService.getRelationship(arguments);
            case "relationship_update" -> relationshipService.updateRelationship(arguments);
            case "relationship_delete" -> relationshipService.deleteRelationship(arguments);
            case "relationship_list" -> relationshipService.listRelationships(arguments);
            
            // Relationship type operations
            case "relationship_type_get" -> relationshipTypeService.getRelationshipType(arguments);
            case "relationship_type_list" -> relationshipTypeService.listRelationshipTypes(arguments);
            case "relationship_type_group_get" -> relationshipTypeGroupService.getRelationshipTypeGroup(arguments);
            case "relationship_type_group_list" -> relationshipTypeGroupService.listRelationshipTypeGroups(arguments);
            
            // Company operations
            case "company_create" -> companyService.createCompany(arguments);
            case "company_get" -> companyService.getCompany(arguments);
            case "company_update" -> companyService.updateCompany(arguments);
            case "company_delete" -> companyService.deleteCompany(arguments);
            case "company_list" -> companyService.listCompanies(arguments);
            
            // Activity type operations
            case "activity_type_create" -> activityTypeService.createActivityType(arguments);
            case "activity_type_get" -> activityTypeService.getActivityType(arguments);
            case "activity_type_update" -> activityTypeService.updateActivityType(arguments);
            case "activity_type_delete" -> activityTypeService.deleteActivityType(arguments);
            case "activity_type_list" -> activityTypeService.listActivityTypes(arguments);
            
            // Activity type category operations
            case "activity_type_category_create" -> activityTypeCategoryService.createActivityTypeCategory(arguments);
            case "activity_type_category_get" -> activityTypeCategoryService.getActivityTypeCategory(arguments);
            case "activity_type_category_update" -> activityTypeCategoryService.updateActivityTypeCategory(arguments);
            case "activity_type_category_delete" -> activityTypeCategoryService.deleteActivityTypeCategory(arguments);
            case "activity_type_category_list" -> activityTypeCategoryService.listActivityTypeCategories(arguments);
            
            // Address operations
            case "address_create" -> addressService.createAddress(arguments);
            case "address_get" -> addressService.getAddress(arguments);
            case "address_update" -> addressService.updateAddress(arguments);
            case "address_delete" -> addressService.deleteAddress(arguments);
            case "address_list" -> addressService.listAddresses(arguments);
            
            // Group operations
            case "group_create" -> groupService.createGroup(arguments);
            case "group_get" -> groupService.getGroup(arguments);
            case "group_update" -> groupService.updateGroup(arguments);
            case "group_delete" -> groupService.deleteGroup(arguments);
            case "group_list" -> groupService.listGroups(arguments);
            
            // Occupation operations
            case "occupation_create" -> occupationService.createOccupation(arguments);
            case "occupation_get" -> occupationService.getOccupation(arguments);
            case "occupation_update" -> occupationService.updateOccupation(arguments);
            case "occupation_delete" -> occupationService.deleteOccupation(arguments);
            case "occupation_list" -> occupationService.listOccupations(arguments);
            
            // Debt operations
            case "debt_create" -> debtService.createDebt(arguments);
            case "debt_get" -> debtService.getDebt(arguments);
            case "debt_update" -> debtService.updateDebt(arguments);
            case "debt_delete" -> debtService.deleteDebt(arguments);
            case "debt_list" -> debtService.listDebts(arguments);
            
            // Document operations
            case "document_create" -> documentService.createDocument(arguments);
            case "document_get" -> documentService.getDocument(arguments);
            case "document_update" -> documentService.updateDocument(arguments);
            case "document_delete" -> documentService.deleteDocument(arguments);
            case "document_list" -> documentService.listDocuments(arguments);
            
            // Photo operations
            case "photo_create" -> photoService.createPhoto(arguments);
            case "photo_get" -> photoService.getPhoto(arguments);
            case "photo_update" -> photoService.updatePhoto(arguments);
            case "photo_delete" -> photoService.deletePhoto(arguments);
            case "photo_list" -> photoService.listPhotos(arguments);
            
            // Gift operations
            case "gift_create" -> giftService.createGift(arguments);
            case "gift_get" -> giftService.getGift(arguments);
            case "gift_update" -> giftService.updateGift(arguments);
            case "gift_delete" -> giftService.deleteGift(arguments);
            case "gift_list" -> giftService.listGifts(arguments);
            
            // Audit Log operations
            case "auditlog_get" -> auditLogService.getAuditLog(arguments);
            case "auditlog_list" -> auditLogService.listAuditLogs(arguments);
            case "auditlog_search" -> auditLogService.searchAuditLogs(arguments);
            
            // Country operations
            case "country_get" -> countryService.getCountry(arguments);
            case "country_list" -> countryService.listCountries(arguments);
            case "country_search" -> countryService.searchCountries(arguments);
            
            // Currency operations
            case "currency_get" -> currencyService.getCurrency(arguments);
            case "currency_list" -> currencyService.listCurrencies(arguments);
            case "currency_search" -> currencyService.searchCurrencies(arguments);
            
            // Discovery operations (Constitutional Principle VII)
            case "gender_list" -> genderService.listGenders(arguments);
            case "contact_field_type_list" -> contactFieldTypeService.listContactFieldTypes(arguments);
            
            // User Management operations - API Gap Implementation
            case "user_create" -> userService.createUser(arguments);
            case "user_get" -> userService.getUser(arguments);
            case "user_update" -> userService.updateUser(arguments);
            case "user_delete" -> userService.deleteUser(arguments);
            case "user_list" -> userService.listUsers(arguments);
            
            // Compliance Management operations - API Gap Implementation
            case "compliance_create" -> complianceService.createCompliance(arguments);
            case "compliance_get" -> complianceService.getCompliance(arguments);
            case "compliance_update" -> complianceService.updateCompliance(arguments);
            case "compliance_delete" -> complianceService.deleteCompliance(arguments);
            case "compliance_list" -> complianceService.listCompliance(arguments);
            
            default -> Mono.error(new UnsupportedOperationException("Tool not implemented: " + toolName));
        };
    }

    // Schema creation methods
    
    /**
     * Creates a schema for discovery tools that take no parameters.
     * These tools simply list available options from Monica API.
     */
    private Map<String, Object> createListOnlySchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(),
            "additionalProperties", false,
            "description", "No parameters required - this discovery tool lists available options"
        );
    }
    
    private Map<String, Object> createIdSchema(String description) {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", description
                )
            ),
            "required", List.of("id")
        );
    }

    private Map<String, Object> createListSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 10,
                    "maximum", 100
                ),
                "search", Map.of(
                    "type", "string",
                    "description", "Search query"
                )
            )
        );
    }

    private Map<String, Object> createContactSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("firstName", Map.of(
            "type", "string",
            "description", "Contact's first name (required)",
            "maxLength", 255
        ));
        properties.put("lastName", Map.of(
            "type", "string",
            "description", "Contact's last name (optional)",
            "maxLength", 255
        ));
        properties.put("genderId", Map.of(
            "type", "string", 
            "description", "Gender ID (required) - Use gender_list tool to see available options. Note: Monica supports custom genders per account."
        ));
        properties.put("nickname", Map.of(
            "type", "string",
            "description", "Contact's nickname (optional)",
            "maxLength", 255
        ));
        properties.put("isBirthdateKnown", Map.of(
            "type", "boolean",
            "description", "Set to true if you know the birthdate (required, default: false)",
            "default", false
        ));
        properties.put("birthdate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Birthdate in YYYY-MM-DD format (optional, only if isBirthdateKnown=true)"
        ));
        properties.put("isDeceased", Map.of(
            "type", "boolean",
            "description", "Set to true if person is deceased (required, default: false)",
            "default", false
        ));
        properties.put("isDeceasedDateKnown", Map.of(
            "type", "boolean",
            "description", "Set to true if you know the death date (required, default: false)",
            "default", false
        ));
        properties.put("deceasedDate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Death date in YYYY-MM-DD format (optional, only if isDeceasedDateKnown=true)"
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Notes or description about the contact (optional)"
        ));
        
        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("firstName", "genderId", "isBirthdateKnown", "isDeceased", "isDeceasedDateKnown")
        );
    }

    private Map<String, Object> createContactUpdateSchema() {
        Map<String, Object> properties = new HashMap<>();
        
        // Required ID field
        properties.put("id", Map.of(
            "type", "integer",
            "description", "Contact ID (required)"
        ));
        
        // Fields that can be updated
        properties.put("firstName", Map.of(
            "type", "string",
            "description", "Contact's first name (optional - if not provided, existing value will be kept)",
            "maxLength", 255
        ));
        properties.put("lastName", Map.of(
            "type", "string",
            "description", "Contact's last name (optional)",
            "maxLength", 255
        ));
        properties.put("genderId", Map.of(
            "type", "string",
            "description", "Gender ID (optional) - Use gender_list tool to see available options"
        ));
        properties.put("nickname", Map.of(
            "type", "string",
            "description", "Contact's nickname (optional)",
            "maxLength", 255
        ));
        properties.put("email", Map.of(
            "type", "string",
            "format", "email",
            "description", "Primary email address (optional)"
        ));
        properties.put("phone", Map.of(
            "type", "string",
            "description", "Primary phone number (optional)"
        ));
        properties.put("company", Map.of(
            "type", "string",
            "description", "Company or organization (optional)",
            "maxLength", 255
        ));
        properties.put("jobTitle", Map.of(
            "type", "string",
            "description", "Job title or position (optional)",
            "maxLength", 255
        ));
        properties.put("birthdate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Birthdate in YYYY-MM-DD format (optional)"
        ));
        properties.put("isBirthdateKnown", Map.of(
            "type", "boolean",
            "description", "Whether the birthdate is known (optional - defaults to existing value)"
        ));
        properties.put("isDeceased", Map.of(
            "type", "boolean",
            "description", "Whether the contact is deceased (optional - defaults to existing value)"
        ));
        properties.put("isDeceasedDateKnown", Map.of(
            "type", "boolean",
            "description", "Whether the deceased date is known (optional - defaults to existing value)"
        ));
        
        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("id"),
            "additionalProperties", false,
            "description", "Update a contact. NOTE: MonicaHQ API will automatically fetch and preserve existing values for all fields not provided in the update."
        );
    }

    // Additional schema methods for other entities (simplified for brevity)
    private Map<String, Object> createActivitySchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "ID of the contact associated with this activity (required)"
                ),
                "activityTypeId", Map.of(
                    "type", "integer",
                    "description", "Activity type ID from Monica (required). Common types: 1=Simple Activity"
                ),
                "summary", Map.of(
                    "type", "string",
                    "description", "Brief summary of the activity (required)",
                    "maxLength", 255
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Detailed description of the activity (optional)"
                ),
                "happenedAt", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Date when activity happened in YYYY-MM-DD format (required)"
                )
            ),
            "required", List.of("contactId", "activityTypeId", "summary", "happenedAt")
        );
    }

    private Map<String, Object> createActivityUpdateSchema() {
        return createUpdateSchema(createActivitySchema());
    }

    private Map<String, Object> createCallSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "ID of the contact you called or who called you (required)"
                ),
                "calledAt", Map.of(
                    "type", "string",
                    "format", "date-time",
                    "description", "Date and time of the call in ISO 8601 format (required)"
                ),
                "content", Map.of(
                    "type", "string",
                    "description", "Notes about what was discussed during the call (optional)"
                ),
                "contactCalledYou", Map.of(
                    "type", "boolean",
                    "description", "Set to true if contact called you, false if you called them (optional, default: false)",
                    "default", false
                )
            ),
            "required", List.of("contactId", "calledAt")
        );
    }

    private Map<String, Object> createCallUpdateSchema() {
        return createUpdateSchema(createCallSchema());
    }

    private Map<String, Object> createNoteSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "ID of the contact this note is about (required)"
                ),
                "body", Map.of(
                    "type", "string",
                    "description", "Content of the note (required). Can be plain text or markdown."
                ),
                "isFavorited", Map.of(
                    "type", "boolean",
                    "description", "Mark this note as favorited/important (optional, default: false)",
                    "default", false
                )
            ),
            "required", List.of("contactId", "body")
        );
    }

    private Map<String, Object> createNoteUpdateSchema() {
        return createUpdateSchema(createNoteSchema());
    }

    private Map<String, Object> createTaskSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "ID of the contact this task is related to (required)"
                ),
                "title", Map.of(
                    "type", "string",
                    "description", "Task title/summary (required)",
                    "maxLength", 255
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Detailed task description (optional)"
                ),
                "completed", Map.of(
                    "type", "boolean",
                    "description", "Whether the task is completed (optional, default: false)",
                    "default", false
                ),
                "completedAt", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Date when task was completed in YYYY-MM-DD format (optional)"
                )
            ),
            "required", List.of("contactId", "title")
        );
    }

    private Map<String, Object> createTaskUpdateSchema() {
        return createUpdateSchema(createTaskSchema());
    }

    private Map<String, Object> createTagSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of("type", "string", "description", "Tag name"),
                "nameSlug", Map.of("type", "string", "description", "Tag slug")
            ),
            "required", List.of("name")
        );
    }

    private Map<String, Object> createTagUpdateSchema() {
        return createUpdateSchema(createTagSchema());
    }

    private Map<String, Object> createReminderSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "ID of the contact this reminder is about (required)"
                ),
                "title", Map.of(
                    "type", "string",
                    "description", "Reminder title/subject (required)",
                    "maxLength", 255
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Additional details about the reminder (optional)"
                ),
                "initialDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Date for the reminder in YYYY-MM-DD format (required)"
                ),
                "frequency", Map.of(
                    "type", "string",
                    "description", "Recurrence frequency: 'one_time', 'weekly', 'monthly', 'yearly' (optional, default: 'one_time')",
                    "enum", List.of("one_time", "weekly", "monthly", "yearly"),
                    "default", "one_time"
                )
            ),
            "required", List.of("contactId", "title", "initialDate")
        );
    }

    private Map<String, Object> createReminderUpdateSchema() {
        return createUpdateSchema(createReminderSchema());
    }

    private Map<String, Object> createJournalSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "title", Map.of("type", "string", "description", "Journal entry title"),
                "post", Map.of("type", "string", "description", "Journal entry content"),
                "date", Map.of("type", "string", "format", "date", "description", "Entry date")
            ),
            "required", List.of("title", "post", "date")
        );
    }

    private Map<String, Object> createJournalUpdateSchema() {
        return createUpdateSchema(createJournalSchema());
    }

    private Map<String, Object> createConversationSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "happenedAt", Map.of("type", "string", "format", "date-time", "description", "Conversation timestamp")
            ),
            "required", List.of("contactId", "happenedAt")
        );
    }

    private Map<String, Object> createConversationUpdateSchema() {
        return createUpdateSchema(createConversationSchema());
    }

    private Map<String, Object> createMessageSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "conversationId", Map.of("type", "integer", "description", "Associated conversation ID"),
                "content", Map.of("type", "string", "description", "Message content"),
                "writtenAt", Map.of("type", "string", "format", "date-time", "description", "Message timestamp"),
                "writtenByMe", Map.of("type", "boolean", "description", "Whether message was written by user")
            ),
            "required", List.of("conversationId", "content", "writtenAt", "writtenByMe")
        );
    }

    private Map<String, Object> createMessageListSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "conversationId", Map.of("type", "integer", "description", "Conversation ID"),
                "page", Map.of("type", "integer", "description", "Page number", "default", 1),
                "limit", Map.of("type", "integer", "description", "Items per page", "default", 10)
            ),
            "required", List.of("conversationId")
        );
    }

    private Map<String, Object> createMessageGetSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "conversationId", Map.of("type", "integer", "description", "Conversation ID"),
                "id", Map.of("type", "integer", "description", "Message ID")
            ),
            "required", List.of("conversationId", "id")
        );
    }

    private Map<String, Object> createMessageUpdateSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "conversationId", Map.of("type", "integer", "description", "Conversation ID"),
                "id", Map.of("type", "integer", "description", "Message ID"),
                "content", Map.of("type", "string", "description", "Message content"),
                "writtenAt", Map.of("type", "string", "format", "date-time", "description", "Message timestamp"),
                "writtenByMe", Map.of("type", "boolean", "description", "Whether message was written by user")
            ),
            "required", List.of("conversationId", "id")
        );
    }

    private Map<String, Object> createMessageDeleteSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "conversationId", Map.of("type", "integer", "description", "Conversation ID"),
                "id", Map.of("type", "integer", "description", "Message ID")
            ),
            "required", List.of("conversationId", "id")
        );
    }

    private Map<String, Object> createContactFieldSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "contactFieldTypeId", Map.of("type", "integer", "description", "Field type ID - Use contact_field_type_list tool to see available types (email, phone, etc.)"),
                "data", Map.of("type", "string", "description", "Field data")
            ),
            "required", List.of("contactId", "contactFieldTypeId", "data")
        );
    }

    private Map<String, Object> createContactFieldUpdateSchema() {
        return createUpdateSchema(createContactFieldSchema());
    }

    private Map<String, Object> createContactFieldListSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of("type", "integer", "description", "Contact ID"),
                "page", Map.of("type", "integer", "description", "Page number", "default", 1),
                "limit", Map.of("type", "integer", "description", "Items per page", "default", 10)
            ),
            "required", List.of("contactId")
        );
    }

    private Map<String, Object> createContactTagSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of("type", "integer", "description", "Contact ID"),
                "tagId", Map.of("type", "integer", "description", "Tag ID")
            ),
            "required", List.of("contactId", "tagId")
        );
    }

    private Map<String, Object> createUpdateSchema(Map<String, Object> baseSchema) {
        Map<String, Object> schema = new HashMap<>(baseSchema);
        @SuppressWarnings("unchecked")
        Map<String, Object> properties = new HashMap<>((Map<String, Object>) schema.get("properties"));
        properties.put("id", Map.of(
            "type", "integer",
            "description", "Entity ID"
        ));
        schema.put("properties", properties);
        schema.put("required", List.of("id"));
        return schema;
    }

    private Map<String, Object> createRelationshipSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactIs", Map.of(
                    "type", "integer",
                    "description", "ID of the first contact in the relationship (required)"
                ),
                "ofContact", Map.of(
                    "type", "integer",
                    "description", "ID of the second contact in the relationship (required)"
                ),
                "relationshipTypeId", Map.of(
                    "type", "integer",
                    "description", "ID of the relationship type (required) - Use relationship_type_list to see available types"
                ),
                "notes", Map.of(
                    "type", "string",
                    "description", "Optional notes about this relationship"
                )
            ),
            "required", List.of("contactIs", "ofContact", "relationshipTypeId")
        );
    }
    
    private Map<String, Object> createRelationshipUpdateSchema() {
        return createUpdateSchema(createRelationshipSchema());
    }
    
    private Map<String, Object> createCompanySchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Company name (required)",
                    "maxLength", 255
                ),
                "website", Map.of(
                    "type", "string",
                    "description", "Company website URL (optional)"
                ),
                "numberOfEmployees", Map.of(
                    "type", "integer",
                    "description", "Number of employees (optional)"
                )
            ),
            "required", List.of("name")
        );
    }
    
    private Map<String, Object> createCompanyUpdateSchema() {
        return createUpdateSchema(createCompanySchema());
    }
    
    private Map<String, Object> createActivityTypeSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Activity type name (required)",
                    "maxLength", 255
                ),
                "categoryId", Map.of(
                    "type", "integer",
                    "description", "Activity type category ID (optional)"
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Activity type description (optional)",
                    "maxLength", 1000
                ),
                "icon", Map.of(
                    "type", "string",
                    "description", "Activity type icon (optional)"
                )
            ),
            "required", List.of("name")
        );
    }
    
    private Map<String, Object> createActivityTypeUpdateSchema() {
        return createUpdateSchema(createActivityTypeSchema());
    }
    
    private Map<String, Object> createActivityTypeCategorySchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Activity type category name (required)",
                    "maxLength", 255
                ),
                "parentId", Map.of(
                    "type", "integer",
                    "description", "Parent category ID for hierarchical structure (optional)"
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Category description (optional)",
                    "maxLength", 1000
                ),
                "sortOrder", Map.of(
                    "type", "integer",
                    "description", "Sort order for display (optional)"
                )
            ),
            "required", List.of("name")
        );
    }
    
    private Map<String, Object> createActivityTypeCategoryUpdateSchema() {
        return createUpdateSchema(createActivityTypeCategorySchema());
    }
    
    private Map<String, Object> createAddressSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID this address belongs to (required)"
                ),
                "name", Map.of(
                    "type", "string",
                    "description", "Address name/label (optional)",
                    "maxLength", 255
                ),
                "street", Map.of(
                    "type", "string",
                    "description", "Street address (optional)",
                    "maxLength", 255
                ),
                "city", Map.of(
                    "type", "string",
                    "description", "City (optional)",
                    "maxLength", 255
                ),
                "province", Map.of(
                    "type", "string",
                    "description", "Province/State (optional)",
                    "maxLength", 255
                ),
                "postalCode", Map.of(
                    "type", "string",
                    "description", "Postal/ZIP code (optional)",
                    "maxLength", 255
                ),
                "country", Map.of(
                    "type", "string",
                    "description", "Country code (optional, max 3 characters)",
                    "maxLength", 3
                ),
                "latitude", Map.of(
                    "type", "number",
                    "description", "Latitude coordinate (optional)"
                ),
                "longitude", Map.of(
                    "type", "number",
                    "description", "Longitude coordinate (optional)"
                )
            ),
            "required", List.of("contactId")
        );
    }
    
    private Map<String, Object> createAddressUpdateSchema() {
        return createUpdateSchema(createAddressSchema());
    }
    
    private Map<String, Object> createGroupSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Group name (required)",
                    "maxLength", 255
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Group description (optional)",
                    "maxLength", 1000
                )
            ),
            "required", List.of("name")
        );
    }
    
    private Map<String, Object> createGroupUpdateSchema() {
        return createUpdateSchema(createGroupSchema());
    }
    
    private Map<String, Object> createOccupationSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID this occupation belongs to (required)"
                ),
                "companyId", Map.of(
                    "type", "integer",
                    "description", "Company ID where the person works (optional)"
                ),
                "title", Map.of(
                    "type", "string",
                    "description", "Job title (required)",
                    "maxLength", 255
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Job description (optional)",
                    "maxLength", 1000
                ),
                "salary", Map.of(
                    "type", "string",
                    "description", "Salary amount (optional)"
                ),
                "salaryUnit", Map.of(
                    "type", "string",
                    "description", "Salary unit (e.g., 'per year', 'per month', 'per hour') (optional)"
                ),
                "currentlyWorksHere", Map.of(
                    "type", "boolean",
                    "description", "Whether the person currently works at this job (optional, default: true)",
                    "default", true
                ),
                "startDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Start date in YYYY-MM-DD format (optional)"
                ),
                "endDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "End date in YYYY-MM-DD format (optional, only if currentlyWorksHere=false)"
                )
            ),
            "required", List.of("contactId", "title")
        );
    }
    
    private Map<String, Object> createOccupationUpdateSchema() {
        return createUpdateSchema(createOccupationSchema());
    }
    
    private Map<String, Object> createDebtSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID this debt belongs to (required)"
                ),
                "amount", Map.of(
                    "type", "number",
                    "description", "Debt amount (required)"
                ),
                "currency", Map.of(
                    "type", "string",
                    "description", "Currency code (optional, max 3 characters)",
                    "maxLength", 3
                ),
                "inDebt", Map.of(
                    "type", "string",
                    "description", "Who is in debt - 'contact' or 'user' (optional)"
                ),
                "status", Map.of(
                    "type", "string",
                    "description", "Debt status (optional)",
                    "maxLength", 255
                ),
                "reason", Map.of(
                    "type", "string",
                    "description", "Reason for the debt (optional)",
                    "maxLength", 1000
                )
            ),
            "required", List.of("contactId", "amount")
        );
    }
    
    private Map<String, Object> createDebtUpdateSchema() {
        return createUpdateSchema(createDebtSchema());
    }
    
    private Map<String, Object> createDocumentSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID this document belongs to (required)"
                ),
                "filename", Map.of(
                    "type", "string",
                    "description", "Document filename (required)",
                    "maxLength", 255
                ),
                "originalFilename", Map.of(
                    "type", "string",
                    "description", "Original filename (optional)",
                    "maxLength", 255
                ),
                "mimeType", Map.of(
                    "type", "string",
                    "description", "MIME type of the document (optional)",
                    "maxLength", 100
                ),
                "size", Map.of(
                    "type", "integer",
                    "description", "File size in bytes (optional)"
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Document description (optional)",
                    "maxLength", 1000
                )
            ),
            "required", List.of("contactId", "filename")
        );
    }
    
    private Map<String, Object> createDocumentUpdateSchema() {
        return createUpdateSchema(createDocumentSchema());
    }
    
    private Map<String, Object> createPhotoSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID this photo belongs to (required)"
                ),
                "filename", Map.of(
                    "type", "string",
                    "description", "Photo filename (required)",
                    "maxLength", 255
                ),
                "originalFilename", Map.of(
                    "type", "string",
                    "description", "Original filename (optional)",
                    "maxLength", 255
                ),
                "width", Map.of(
                    "type", "integer",
                    "description", "Image width in pixels (optional)"
                ),
                "height", Map.of(
                    "type", "integer",
                    "description", "Image height in pixels (optional)"
                ),
                "filesize", Map.of(
                    "type", "integer",
                    "description", "File size in bytes (optional)"
                ),
                "mimeType", Map.of(
                    "type", "string",
                    "description", "MIME type of the image (optional)",
                    "maxLength", 100
                )
            ),
            "required", List.of("contactId", "filename")
        );
    }
    
    private Map<String, Object> createPhotoUpdateSchema() {
        return createUpdateSchema(createPhotoSchema());
    }
    
    private Map<String, Object> createGiftSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID this gift is for (required)"
                ),
                "name", Map.of(
                    "type", "string",
                    "description", "Gift name (required)",
                    "maxLength", 255
                ),
                "comment", Map.of(
                    "type", "string",
                    "description", "Gift comment or notes (optional)",
                    "maxLength", 1000
                ),
                "url", Map.of(
                    "type", "string",
                    "description", "Gift URL or link (optional)",
                    "maxLength", 255
                ),
                "value", Map.of(
                    "type", "number",
                    "description", "Gift value/price (optional)"
                ),
                "status", Map.of(
                    "type", "string",
                    "description", "Gift status - 'idea', 'purchased', 'given' (optional)",
                    "maxLength", 255
                ),
                "date", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Gift date in YYYY-MM-DD format (optional)"
                ),
                "isFor", Map.of(
                    "type", "string",
                    "description", "What occasion the gift is for (optional)"
                )
            ),
            "required", List.of("contactId", "name")
        );
    }
    
    private Map<String, Object> createGiftUpdateSchema() {
        return createUpdateSchema(createGiftSchema());
    }
    
    private Map<String, Object> createAuditLogSearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "action", Map.of(
                    "type", "string",
                    "description", "Filter by action type (e.g., 'create', 'update', 'delete')"
                ),
                "auditableType", Map.of(
                    "type", "string",
                    "description", "Filter by entity type (e.g., 'Contact', 'Activity')"
                ),
                "userId", Map.of(
                    "type", "integer",
                    "description", "Filter by user ID who performed the action"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 25,
                    "maximum", 100
                )
            ),
            "additionalProperties", false
        );
    }
    
    private Map<String, Object> createCountrySearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "search", Map.of(
                    "type", "string",
                    "description", "Search query for country name"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 50,
                    "maximum", 200
                )
            ),
            "additionalProperties", false
        );
    }
    
    private Map<String, Object> createCurrencySearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "search", Map.of(
                    "type", "string",
                    "description", "Search query for currency code or name"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 50,
                    "maximum", 200
                )
            ),
            "additionalProperties", false
        );
    }
    
    // API Gap Fix Schema Methods - Contact Operations
    private Map<String, Object> createContactSearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "query", Map.of(
                    "type", "string",
                    "description", "Search query for contacts (name, email, etc.)"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "minimum", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of results per page (default: 10, max: 100)",
                    "minimum", 1,
                    "maximum", 100
                )
            ),
            "required", List.of()
        );
    }
    
    private Map<String, Object> createContactCareerSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", "Contact ID (required)"
                ),
                "jobTitle", Map.of(
                    "type", "string",
                    "description", "Job title"
                ),
                "company", Map.of(
                    "type", "string",
                    "description", "Company name"
                ),
                "startDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Start date in YYYY-MM-DD format"
                ),
                "endDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "End date in YYYY-MM-DD format (optional)"
                ),
                "salary", Map.of(
                    "type", "string",
                    "description", "Salary information"
                )
            ),
            "required", List.of("id")
        );
    }
    
    private Map<String, Object> createContactAuditLogSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", "Contact ID (required)"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "minimum", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of results per page (default: 20, max: 100)",
                    "minimum", 1,
                    "maximum", 100
                )
            ),
            "required", List.of("id")
        );
    }
    
    private Map<String, Object> createContactsByTagSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "tagId", Map.of(
                    "type", "integer",
                    "description", "Tag ID (required)"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "minimum", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of results per page (default: 10, max: 100)",
                    "minimum", 1,
                    "maximum", 100
                )
            ),
            "required", List.of("tagId")
        );
    }
    
    // User Management Schema Methods
    private Map<String, Object> createUserSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "firstName", Map.of(
                    "type", "string",
                    "description", "First name (required)",
                    "maxLength", 255
                ),
                "lastName", Map.of(
                    "type", "string",
                    "description", "Last name",
                    "maxLength", 255
                ),
                "email", Map.of(
                    "type", "string",
                    "format", "email",
                    "description", "Email address (required)",
                    "maxLength", 255
                ),
                "timezone", Map.of(
                    "type", "string",
                    "description", "User timezone"
                ),
                "locale", Map.of(
                    "type", "string",
                    "description", "User locale"
                ),
                "currency", Map.of(
                    "type", "string",
                    "description", "User currency"
                ),
                "isAdministrator", Map.of(
                    "type", "boolean",
                    "description", "Administrator flag"
                )
            ),
            "required", List.of("firstName", "email")
        );
    }
    
    private Map<String, Object> createUserUpdateSchema() {
        return createUpdateSchema(createUserSchema());
    }
    
    // Compliance Management Schema Methods
    private Map<String, Object> createComplianceSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Associated contact ID"
                ),
                "type", Map.of(
                    "type", "string",
                    "description", "Compliance type (required)",
                    "maxLength", 100
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Compliance description",
                    "maxLength", 500
                ),
                "isActive", Map.of(
                    "type", "boolean",
                    "description", "Active status"
                ),
                "dataRetentionDays", Map.of(
                    "type", "integer",
                    "description", "Data retention period in days"
                ),
                "privacyLevel", Map.of(
                    "type", "string",
                    "description", "Privacy level"
                ),
                "consentGiven", Map.of(
                    "type", "boolean",
                    "description", "Consent status"
                ),
                "consentDate", Map.of(
                    "type", "string",
                    "format", "date-time",
                    "description", "Consent date"
                ),
                "auditRequired", Map.of(
                    "type", "boolean",
                    "description", "Audit requirement flag"
                )
            ),
            "required", List.of("type")
        );
    }
    
    private Map<String, Object> createComplianceUpdateSchema() {
        return createUpdateSchema(createComplianceSchema());
    }

    // Inner class for MCP tools
    private static class McpTool {
        private final String name;
        private final String description;
        private final Map<String, Object> inputSchema;
        private final String category;

        public McpTool(String name, String description, Map<String, Object> inputSchema, String category) {
            this.name = name;
            this.description = description;
            this.inputSchema = inputSchema;
            this.category = category;
        }

        public Map<String, Object> toMap() {
            Map<String, Object> result = new HashMap<>();
            result.put("name", name);
            result.put("description", description);
            result.put("inputSchema", inputSchema);
            
            // Add category metadata for future client-side grouping
            if (category != null) {
                // Add category as custom metadata
                Map<String, Object> enhancedSchema = new HashMap<>(inputSchema);
                enhancedSchema.put("x-category", category);
                result.put("inputSchema", enhancedSchema);
            }
            
            return result;
        }
    }
}
