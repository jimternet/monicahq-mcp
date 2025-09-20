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

    @PostConstruct
    public void initializeTools() {
        log.info("Initializing MCP tool registry with 52 operations");
        
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
        
        // Conversation operations (4)
        registerTool("conversation_create", "[Conversation] Create a new conversation", createConversationSchema(), "Activity & Communication");
        registerTool("conversation_get", "[Conversation] Get a conversation by ID", createIdSchema("Conversation ID"), "Activity & Communication");
        registerTool("conversation_update", "[Conversation] Update an existing conversation", createConversationUpdateSchema(), "Activity & Communication");
        registerTool("conversation_list", "[Conversation] List conversations with pagination", createListSchema(), "Activity & Communication");
        
        // Conversation message operations (4)
        registerTool("conversation_message_create", "[Message] Create a new conversation message", createMessageSchema(), "Activity & Communication");
        registerTool("conversation_message_get", "[Message] Get a conversation message by ID", createMessageGetSchema(), "Activity & Communication");
        registerTool("conversation_message_update", "[Message] Update an existing conversation message", createMessageUpdateSchema(), "Activity & Communication");
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
            case "conversation_list" -> conversationService.listConversations(arguments);
            
            // Conversation Message operations
            case "conversation_message_create" -> conversationMessageService.createConversationMessage(arguments);
            case "conversation_message_get" -> conversationMessageService.getConversationMessage(arguments);
            case "conversation_message_update" -> conversationMessageService.updateConversationMessage(arguments);
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
            
            default -> Mono.error(new UnsupportedOperationException("Tool not implemented: " + toolName));
        };
    }

    // Schema creation methods
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
            "description", "Gender ID: 1=Male, 2=Female, 3=Other (required)",
            "enum", List.of("1", "2", "3")
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
        Map<String, Object> schema = new HashMap<>(createContactSchema());
        @SuppressWarnings("unchecked")
        Map<String, Object> properties = new HashMap<>((Map<String, Object>) schema.get("properties"));
        properties.put("id", Map.of(
            "type", "integer",
            "description", "Contact ID"
        ));
        schema.put("properties", properties);
        schema.put("required", List.of("id"));
        return schema;
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
                "contactFieldTypeId", Map.of("type", "integer", "description", "Field type ID"),
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
