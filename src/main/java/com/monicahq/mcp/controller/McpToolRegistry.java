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
        log.info("Initializing MCP tool registry with 54 operations");
        
        // Contact operations (5)
        registerTool("contact_create", "Create a new contact in MonicaHQ", createContactSchema());
        registerTool("contact_get", "Get a contact by ID", createIdSchema("Contact ID"));
        registerTool("contact_update", "Update an existing contact", createContactUpdateSchema());
        registerTool("contact_delete", "Delete a contact", createIdSchema("Contact ID"));
        registerTool("contact_list", "List contacts with pagination", createListSchema());
        
        // Activity operations (5)
        registerTool("activity_create", "Create a new activity", createActivitySchema());
        registerTool("activity_get", "Get an activity by ID", createIdSchema("Activity ID"));
        registerTool("activity_update", "Update an existing activity", createActivityUpdateSchema());
        registerTool("activity_delete", "Delete an activity", createIdSchema("Activity ID"));
        registerTool("activity_list", "List activities with pagination", createListSchema());
        
        // Call operations (5)
        registerTool("call_create", "Create a new call record", createCallSchema());
        registerTool("call_get", "Get a call by ID", createIdSchema("Call ID"));
        registerTool("call_update", "Update an existing call", createCallUpdateSchema());
        registerTool("call_delete", "Delete a call", createIdSchema("Call ID"));
        registerTool("call_list", "List calls with pagination", createListSchema());
        
        // Note operations (5)
        registerTool("note_create", "Create a new note", createNoteSchema());
        registerTool("note_get", "Get a note by ID", createIdSchema("Note ID"));
        registerTool("note_update", "Update an existing note", createNoteUpdateSchema());
        registerTool("note_delete", "Delete a note", createIdSchema("Note ID"));
        registerTool("note_list", "List notes with pagination", createListSchema());
        
        // Task operations (5)
        registerTool("task_create", "Create a new task", createTaskSchema());
        registerTool("task_get", "Get a task by ID", createIdSchema("Task ID"));
        registerTool("task_update", "Update an existing task", createTaskUpdateSchema());
        registerTool("task_delete", "Delete a task", createIdSchema("Task ID"));
        registerTool("task_list", "List tasks with pagination", createListSchema());
        
        // Tag operations (5)
        registerTool("tag_create", "Create a new tag", createTagSchema());
        registerTool("tag_get", "Get a tag by ID", createIdSchema("Tag ID"));
        registerTool("tag_update", "Update an existing tag", createTagUpdateSchema());
        registerTool("tag_delete", "Delete a tag", createIdSchema("Tag ID"));
        registerTool("tag_list", "List tags with pagination", createListSchema());
        
        // Reminder operations (5)
        registerTool("reminder_create", "Create a new reminder", createReminderSchema());
        registerTool("reminder_get", "Get a reminder by ID", createIdSchema("Reminder ID"));
        registerTool("reminder_update", "Update an existing reminder", createReminderUpdateSchema());
        registerTool("reminder_delete", "Delete a reminder", createIdSchema("Reminder ID"));
        registerTool("reminder_list", "List reminders with pagination", createListSchema());
        
        // Journal Entry operations (5)
        registerTool("journal_entry_create", "Create a new journal entry", createJournalSchema());
        registerTool("journal_entry_get", "Get a journal entry by ID", createIdSchema("Journal Entry ID"));
        registerTool("journal_entry_update", "Update an existing journal entry", createJournalUpdateSchema());
        registerTool("journal_entry_delete", "Delete a journal entry", createIdSchema("Journal Entry ID"));
        registerTool("journal_entry_list", "List journal entries with pagination", createListSchema());
        
        // Conversation operations (4)
        registerTool("conversation_create", "Create a new conversation", createConversationSchema());
        registerTool("conversation_get", "Get a conversation by ID", createIdSchema("Conversation ID"));
        registerTool("conversation_update", "Update an existing conversation", createConversationUpdateSchema());
        registerTool("conversation_list", "List conversations with pagination", createListSchema());
        
        // Conversation Message operations (4)
        registerTool("conversation_message_create", "Create a new conversation message", createMessageSchema());
        registerTool("conversation_message_get", "Get a conversation message by ID", createMessageGetSchema());
        registerTool("conversation_message_update", "Update an existing conversation message", createMessageUpdateSchema());
        registerTool("conversation_message_list", "List conversation messages", createMessageListSchema());
        
        // Contact Field operations (4)
        registerTool("contact_field_create", "Create a new contact field", createContactFieldSchema());
        registerTool("contact_field_get", "Get a contact field by ID", createIdSchema("Contact Field ID"));
        registerTool("contact_field_update", "Update an existing contact field", createContactFieldUpdateSchema());
        registerTool("contact_field_list", "List contact fields with pagination", createListSchema());
        
        // ContactTag operations (2)
        registerTool("contacttag_add", "Add a tag to a contact", createContactTagSchema());
        registerTool("contacttag_remove", "Remove a tag from a contact", createContactTagSchema());
        
        log.info("Initialized {} MCP tools", tools.size());
    }

    private void registerTool(String name, String description, Map<String, Object> inputSchema) {
        McpTool tool = new McpTool(name, description, inputSchema);
        tools.put(name, tool);
        log.debug("Registered MCP tool: {}", name);
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
            
            // Journal Entry operations
            case "journal_entry_create" -> journalEntryService.createJournalEntry(arguments);
            case "journal_entry_get" -> journalEntryService.getJournalEntry(arguments);
            case "journal_entry_update" -> journalEntryService.updateJournalEntry(arguments);
            case "journal_entry_delete" -> journalEntryService.deleteJournalEntry(arguments);
            case "journal_entry_list" -> journalEntryService.listJournalEntries(arguments);
            
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
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "firstName", Map.of(
                    "type", "string",
                    "description", "Contact's first name",
                    "maxLength", 255
                ),
                "lastName", Map.of(
                    "type", "string",
                    "description", "Contact's last name",
                    "maxLength", 255
                ),
                "genderId", Map.of(
                    "type", "integer",
                    "description", "Gender identifier (required by MonicaHQ)"
                ),
                "isBirthdateKnown", Map.of(
                    "type", "boolean",
                    "description", "Whether birthdate is known",
                    "default", false
                ),
                "isDeceased", Map.of(
                    "type", "boolean",
                    "description", "Whether person is deceased",
                    "default", false
                ),
                "isDeceasedDateKnown", Map.of(
                    "type", "boolean",
                    "description", "Whether death date is known",
                    "default", false
                )
            ),
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
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "type", Map.of("type", "string", "description", "Activity type"),
                "summary", Map.of("type", "string", "description", "Activity summary"),
                "date", Map.of("type", "string", "format", "date-time", "description", "Activity date")
            ),
            "required", List.of("contactId", "type", "summary", "date")
        );
    }

    private Map<String, Object> createActivityUpdateSchema() {
        return createUpdateSchema(createActivitySchema());
    }

    private Map<String, Object> createCallSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "calledAt", Map.of("type", "string", "format", "date-time", "description", "Call timestamp"),
                "duration", Map.of("type", "integer", "description", "Call duration in minutes"),
                "type", Map.of("type", "string", "description", "Call type (incoming/outgoing)")
            ),
            "required", List.of("contactId", "calledAt", "type")
        );
    }

    private Map<String, Object> createCallUpdateSchema() {
        return createUpdateSchema(createCallSchema());
    }

    private Map<String, Object> createNoteSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "body", Map.of("type", "string", "description", "Note content"),
                "isFavorited", Map.of("type", "boolean", "description", "Whether note is favorited")
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
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "title", Map.of("type", "string", "description", "Task title"),
                "description", Map.of("type", "string", "description", "Task description"),
                "completed", Map.of("type", "boolean", "description", "Task completion status")
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
                "contactId", Map.of("type", "integer", "description", "Associated contact ID"),
                "title", Map.of("type", "string", "description", "Reminder title"),
                "description", Map.of("type", "string", "description", "Reminder description"),
                "initialDate", Map.of("type", "string", "format", "date", "description", "Reminder date")
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

        public McpTool(String name, String description, Map<String, Object> inputSchema) {
            this.name = name;
            this.description = description;
            this.inputSchema = inputSchema;
        }

        public Map<String, Object> toMap() {
            return Map.of(
                "name", name,
                "description", description,
                "inputSchema", inputSchema
            );
        }
    }
}
