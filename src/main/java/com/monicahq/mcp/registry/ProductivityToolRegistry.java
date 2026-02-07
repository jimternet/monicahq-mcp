package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.NoteService;
import com.monicahq.mcp.service.ReminderService;
import com.monicahq.mcp.service.TagService;
import com.monicahq.mcp.service.TaskService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Productivity operations.
 *
 * This registry handles productivity-related MCP tools:
 * - Note CRUD operations (create, get, update, delete, list)
 * - Task CRUD operations (create, get, update, delete, list)
 * - Reminder CRUD operations (create, get, update, delete, list)
 * - Tag CRUD operations (create, get, update, delete, list)
 *
 * Notes are text entries associated with contacts for storing information.
 * Tasks are to-do items related to contacts that can be marked complete.
 * Reminders are scheduled notifications for important dates/events.
 * Tags are labels for categorizing and organizing contacts.
 *
 * The registry delegates execution to:
 * - NoteService for note operations
 * - TaskService for task operations
 * - ReminderService for reminder operations
 * - TagService for tag operations
 */
@Component
@Slf4j
public class ProductivityToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Productivity";
    private static final String CATEGORY = "Productivity & Organization";

    private final NoteService noteService;
    private final TaskService taskService;
    private final ReminderService reminderService;
    private final TagService tagService;

    public ProductivityToolRegistry(
            NoteService noteService,
            TaskService taskService,
            ReminderService reminderService,
            TagService tagService) {
        this.noteService = noteService;
        this.taskService = taskService;
        this.reminderService = reminderService;
        this.tagService = tagService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Note CRUD operations (5)
        registerTool(
            "note_create",
            "[Note] Create a new note",
            createNoteSchema(),
            CATEGORY
        );

        registerTool(
            "note_get",
            "[Note] Get a note by ID",
            createIdSchema("Note ID"),
            CATEGORY
        );

        registerTool(
            "note_update",
            "[Note] Update an existing note",
            createNoteUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "note_delete",
            "[Note] Delete a note",
            createIdSchema("Note ID"),
            CATEGORY
        );

        registerTool(
            "note_list",
            "[Note] List notes with pagination",
            createListSchema(),
            CATEGORY
        );

        registerTool(
            "note_list_by_contact",
            "[Note] List all notes for a specific contact",
            createContactScopedListSchema("Contact ID to retrieve notes for"),
            CATEGORY
        );

        // Task CRUD operations (5)
        registerTool(
            "task_create",
            "[Task] Create a new task",
            createTaskSchema(),
            CATEGORY
        );

        registerTool(
            "task_get",
            "[Task] Get a task by ID",
            createIdSchema("Task ID"),
            CATEGORY
        );

        registerTool(
            "task_update",
            "[Task] Update an existing task",
            createTaskUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "task_delete",
            "[Task] Delete a task",
            createIdSchema("Task ID"),
            CATEGORY
        );

        registerTool(
            "task_list",
            "[Task] List tasks with pagination",
            createListSchema(),
            CATEGORY
        );

        registerTool(
            "task_list_by_contact",
            "[Task] List all tasks for a specific contact",
            createContactScopedListSchema("Contact ID to retrieve tasks for"),
            CATEGORY
        );

        // Reminder CRUD operations (5)
        registerTool(
            "reminder_create",
            "[Reminder] Create a new reminder",
            createReminderSchema(),
            CATEGORY
        );

        registerTool(
            "reminder_get",
            "[Reminder] Get a reminder by ID",
            createIdSchema("Reminder ID"),
            CATEGORY
        );

        registerTool(
            "reminder_update",
            "[Reminder] Update an existing reminder",
            createReminderUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "reminder_delete",
            "[Reminder] Delete a reminder",
            createIdSchema("Reminder ID"),
            CATEGORY
        );

        registerTool(
            "reminder_list",
            "[Reminder] List reminders with pagination",
            createListSchema(),
            CATEGORY
        );

        registerTool(
            "reminder_list_by_contact",
            "[Reminder] List all reminders for a specific contact",
            createContactScopedListSchema("Contact ID to retrieve reminders for"),
            CATEGORY
        );

        // Tag CRUD operations (5)
        registerTool(
            "tag_create",
            "[Tag] Create a new tag",
            createTagSchema(),
            CATEGORY
        );

        registerTool(
            "tag_get",
            "[Tag] Get a tag by ID",
            createIdSchema("Tag ID"),
            CATEGORY
        );

        registerTool(
            "tag_update",
            "[Tag] Update an existing tag",
            createTagUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "tag_delete",
            "[Tag] Delete a tag",
            createIdSchema("Tag ID"),
            CATEGORY
        );

        registerTool(
            "tag_list",
            "[Tag] List tags with pagination",
            createListSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Note operations
            case "note_create" -> noteService.createNote(arguments);
            case "note_get" -> noteService.getNote(arguments);
            case "note_update" -> noteService.updateNote(arguments);
            case "note_delete" -> noteService.deleteNote(arguments);
            case "note_list" -> noteService.listNotes(arguments);
            case "note_list_by_contact" -> noteService.listNotesByContact(arguments);

            // Task operations
            case "task_create" -> taskService.createTask(arguments);
            case "task_get" -> taskService.getTask(arguments);
            case "task_update" -> taskService.updateTask(arguments);
            case "task_delete" -> taskService.deleteTask(arguments);
            case "task_list" -> taskService.listTasks(arguments);
            case "task_list_by_contact" -> taskService.listTasksByContact(arguments);

            // Reminder operations
            case "reminder_create" -> reminderService.createReminder(arguments);
            case "reminder_get" -> reminderService.getReminder(arguments);
            case "reminder_update" -> reminderService.updateReminder(arguments);
            case "reminder_delete" -> reminderService.deleteReminder(arguments);
            case "reminder_list" -> reminderService.listReminders(arguments);
            case "reminder_list_by_contact" -> reminderService.listRemindersByContact(arguments);

            // Tag operations
            case "tag_create" -> tagService.createTag(arguments);
            case "tag_get" -> tagService.getTag(arguments);
            case "tag_update" -> tagService.updateTag(arguments);
            case "tag_delete" -> tagService.deleteTag(arguments);
            case "tag_list" -> tagService.listTags(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Note Schema Methods ==========

    /**
     * Creates the schema for note creation.
     * Notes are text entries associated with contacts for storing information.
     */
    private Map<String, Object> createNoteSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "ID of the contact this note is about (required)"
        ));
        properties.put("body", Map.of(
            "type", "string",
            "description", "Content of the note (required). Can be plain text or markdown."
        ));
        properties.put("isFavorited", Map.of(
            "type", "boolean",
            "description", "Mark this note as favorited/important (optional, default: false)",
            "default", false
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "body"));

        return schema;
    }

    /**
     * Creates the schema for note updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createNoteUpdateSchema() {
        return createUpdateSchema(createNoteSchema());
    }

    // ========== Task Schema Methods ==========

    /**
     * Creates the schema for task creation.
     * Tasks are to-do items related to contacts that can be marked complete.
     */
    private Map<String, Object> createTaskSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "ID of the contact this task is related to (required)"
        ));
        properties.put("title", Map.of(
            "type", "string",
            "description", "Task title/summary (required)",
            "maxLength", 255
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Detailed task description (optional)"
        ));
        properties.put("completed", Map.of(
            "type", "boolean",
            "description", "Whether the task is completed (optional, default: false)",
            "default", false
        ));
        properties.put("completedAt", Map.of(
            "type", "string",
            "format", "date",
            "description", "Date when task was completed in YYYY-MM-DD format (optional)"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "title"));

        return schema;
    }

    /**
     * Creates the schema for task updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createTaskUpdateSchema() {
        return createUpdateSchema(createTaskSchema());
    }

    // ========== Reminder Schema Methods ==========

    /**
     * Creates the schema for reminder creation.
     * Reminders are scheduled notifications for important dates/events.
     */
    private Map<String, Object> createReminderSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "ID of the contact this reminder is about (required)"
        ));
        properties.put("title", Map.of(
            "type", "string",
            "description", "Reminder title/subject (required)",
            "maxLength", 255
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Additional details about the reminder (optional)"
        ));
        properties.put("initialDate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Date for the reminder in YYYY-MM-DD format (required)"
        ));
        properties.put("frequency", Map.of(
            "type", "string",
            "description", "Recurrence frequency: 'one_time', 'weekly', 'monthly', 'yearly' (optional, default: 'one_time')",
            "enum", List.of("one_time", "weekly", "monthly", "yearly"),
            "default", "one_time"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "title", "initialDate"));

        return schema;
    }

    /**
     * Creates the schema for reminder updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createReminderUpdateSchema() {
        return createUpdateSchema(createReminderSchema());
    }

    // ========== Tag Schema Methods ==========

    /**
     * Creates the schema for tag creation.
     * Tags are labels for categorizing and organizing contacts.
     */
    private Map<String, Object> createTagSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("name", Map.of(
            "type", "string",
            "description", "Tag name (required)"
        ));
        properties.put("nameSlug", Map.of(
            "type", "string",
            "description", "Tag slug (optional, auto-generated from name if not provided)"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("name"));

        return schema;
    }

    /**
     * Creates the schema for tag updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createTagUpdateSchema() {
        return createUpdateSchema(createTagSchema());
    }

    // ========== Contact-Scoped List Schema ==========

    /**
     * Creates the schema for contact-scoped list operations.
     * Includes contactId as required parameter with optional pagination.
     *
     * @param contactIdDescription description for the contactId parameter
     * @return schema Map with contactId and pagination properties
     */
    private Map<String, Object> createContactScopedListSchema(String contactIdDescription) {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", contactIdDescription
        ));
        properties.put("page", Map.of(
            "type", "integer",
            "description", "Page number (starting from 1)",
            "default", 1
        ));
        properties.put("limit", Map.of(
            "type", "integer",
            "description", "Number of items per page",
            "default", 10,
            "maximum", 100
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId"));

        return schema;
    }
}
