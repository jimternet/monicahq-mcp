package com.monicahq.mcp.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.dto.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Utility class for formatting MCP response content fields to ensure Claude Desktop visibility.
 * 
 * According to Constitutional Principle VI: MCP Response Content Visibility & Complete Monica API Data Access,
 * ALL MCP tool responses MUST include COMPLETE Monica API data as escaped JSON within the content field
 * for Claude Desktop accessibility. Since Claude Desktop only processes the content field of responses, 
 * ALL raw Monica API response data MUST be provided as escaped JSON strings within the content field 
 * to enable direct data parsing and manipulation by Claude.
 * 
 * This formatter provides both human-readable and escaped JSON formatting methods.
 * The escaped JSON methods comply with Constitutional Principle VI requirements.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class ContentFormatter {
    
    private final ObjectMapper objectMapper;
    
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    
    // ========================================================================================
    // CONSTITUTIONAL PRINCIPLE VI: ESCAPED JSON FORMATTING METHODS
    // ========================================================================================
    
    /**
     * Formats raw Monica API data as escaped JSON for Claude Desktop accessibility.
     * This method complies with Constitutional Principle VI by providing complete Monica API 
     * data as escaped JSON strings that Claude Desktop can parse and manipulate directly.
     * 
     * @param rawApiData The complete raw response data from Monica API
     * @return Escaped JSON string containing all Monica API data
     */
    public String formatAsEscapedJson(Map<String, Object> rawApiData) {
        if (rawApiData == null) {
            return "{}";
        }
        
        try {
            return objectMapper.writeValueAsString(rawApiData);
        } catch (JsonProcessingException e) {
            log.error("Failed to serialize Monica API data to JSON: {}", e.getMessage(), e);
            // Fallback to ensure Claude Desktop still gets some data
            return "{\"error\":\"Failed to serialize Monica API data\",\"original_data_keys\":" + 
                   rawApiData.keySet() + "}";
        }
    }
    
    /**
     * Formats a list response as escaped JSON for Claude Desktop accessibility.
     * This includes both the data array and any metadata like pagination.
     * 
     * @param listResponse The complete list response from Monica API including data and meta
     * @return Escaped JSON string containing complete list response
     */
    public String formatListAsEscapedJson(Map<String, Object> listResponse) {
        return formatAsEscapedJson(listResponse);
    }
    
    // ========================================================================================
    // LEGACY HUMAN-READABLE FORMATTING METHODS (kept for backward compatibility)
    // ========================================================================================
    
    // Contact Formatting
    
    /**
     * Formats a Contact entity for Claude Desktop visibility.
     * Includes ALL contact fields from Monica API in human-readable format.
     */
    public String formatContact(Contact contact) {
        if (contact == null) {
            return "Contact: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Contact Details:\n");
        content.append("===============\n");
        content.append("ID: ").append(contact.getId()).append("\n");
        content.append("Name: ").append(formatContactName(contact)).append("\n");
        
        if (contact.getNickname() != null && !contact.getNickname().trim().isEmpty()) {
            content.append("Nickname: ").append(contact.getNickname()).append("\n");
        }
        
        // Email and phone information
        if (contact.getEmail() != null && !contact.getEmail().trim().isEmpty()) {
            content.append("Email: ").append(contact.getEmail()).append("\n");
        }
        if (contact.getPhone() != null && !contact.getPhone().trim().isEmpty()) {
            content.append("Phone: ").append(contact.getPhone()).append("\n");
        }
        
        // Birthdate information
        content.append("Birthdate Known: ").append(contact.getIsBirthdateKnown() != null ? (contact.getIsBirthdateKnown() ? "Yes" : "No") : "Unknown").append("\n");
        if (contact.getBirthdate() != null) {
            content.append("Birthdate: ").append(contact.getBirthdate().format(DATE_FORMATTER)).append("\n");
        }
        
        // Gender and demographic information
        content.append("Gender ID: ").append(contact.getGenderId()).append("\n");
        content.append("Deceased: ").append(contact.getIsDeceased() != null ? (contact.getIsDeceased() ? "Yes" : "No") : "Unknown").append("\n");
        content.append("Deceased Date Known: ").append(contact.getIsDeceasedDateKnown() != null ? (contact.getIsDeceasedDateKnown() ? "Yes" : "No") : "Unknown").append("\n");
        
        // Professional information
        if (contact.getCompany() != null && !contact.getCompany().trim().isEmpty()) {
            content.append("Company: ").append(contact.getCompany()).append("\n");
        }
        if (contact.getJobTitle() != null && !contact.getJobTitle().trim().isEmpty()) {
            content.append("Job Title: ").append(contact.getJobTitle()).append("\n");
        }
        
        // Tags information
        if (contact.getTags() != null && !contact.getTags().isEmpty()) {
            content.append("Tags: ");
            for (int i = 0; i < contact.getTags().size(); i++) {
                Tag tag = contact.getTags().get(i);
                content.append(tag.getName() != null ? tag.getName() : "Unnamed Tag");
                if (i < contact.getTags().size() - 1) {
                    content.append(", ");
                }
            }
            content.append("\n");
        }
        
        // Timestamps
        if (contact.getCreatedAt() != null) {
            content.append("Created: ").append(contact.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (contact.getUpdatedAt() != null) {
            content.append("Updated: ").append(contact.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    /**
     * Formats a Contact from raw API data for complete field coverage.
     * Includes ALL fields returned by Monica API, including nested structures.
     */
    public String formatContactFromRawData(Map<String, Object> contactData) {
        if (contactData == null) {
            return "Contact: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Contact Details:\n");
        content.append("===============\n");
        
        // Basic information
        content.append("ID: ").append(contactData.get("id")).append("\n");
        
        // Name fields
        String firstName = (String) contactData.get("firstName");
        String lastName = (String) contactData.get("lastName");
        String completeName = (String) contactData.get("complete_name");
        
        if (completeName != null && !completeName.trim().isEmpty()) {
            content.append("Complete Name: ").append(completeName).append("\n");
        } else {
            StringBuilder name = new StringBuilder();
            if (firstName != null) name.append(firstName);
            if (lastName != null) {
                if (name.length() > 0) name.append(" ");
                name.append(lastName);
            }
            content.append("Name: ").append(name.length() > 0 ? name.toString() : "Unnamed Contact").append("\n");
        }
        
        // Additional basic fields
        Object nickname = contactData.get("nickname");
        if (nickname != null && !nickname.toString().trim().isEmpty()) {
            content.append("Nickname: ").append(nickname).append("\n");
        }
        
        // Contact information
        Object email = contactData.get("email");
        if (email != null && !email.toString().trim().isEmpty()) {
            content.append("Email: ").append(email).append("\n");
        }
        Object phone = contactData.get("phone");
        if (phone != null && !phone.toString().trim().isEmpty()) {
            content.append("Phone: ").append(phone).append("\n");
        }
        
        // Demographics
        Object genderId = contactData.get("genderId");
        if (genderId != null) {
            content.append("Gender ID: ").append(genderId).append("\n");
        }
        
        Object birthdate = contactData.get("birthdate");
        if (birthdate != null) {
            content.append("Birthdate: ").append(birthdate).append("\n");
        }
        
        Object isBirthdateKnown = contactData.get("isBirthdateKnown");
        if (isBirthdateKnown != null) {
            content.append("Birthdate Known: ").append(Boolean.TRUE.equals(isBirthdateKnown) ? "Yes" : "No").append("\n");
        }
        
        Object isDeceased = contactData.get("isDeceased");
        if (isDeceased != null) {
            content.append("Deceased: ").append(Boolean.TRUE.equals(isDeceased) ? "Yes" : "No").append("\n");
        }
        
        Object isDeceasedDateKnown = contactData.get("isDeceasedDateKnown");
        if (isDeceasedDateKnown != null) {
            content.append("Deceased Date Known: ").append(Boolean.TRUE.equals(isDeceasedDateKnown) ? "Yes" : "No").append("\n");
        }
        
        // Professional information
        Object company = contactData.get("company");
        if (company != null && !company.toString().trim().isEmpty()) {
            content.append("Company: ").append(company).append("\n");
        }
        Object jobTitle = contactData.get("jobTitle");
        if (jobTitle != null && !jobTitle.toString().trim().isEmpty()) {
            content.append("Job Title: ").append(jobTitle).append("\n");
        }
        
        // Handle nested information structure
        Object information = contactData.get("information");
        if (information instanceof Map<?, ?> infoMap) {
            content.append("\nAdditional Information:\n");
            content.append("----------------------\n");
            
            // Career information
            Object career = infoMap.get("career");
            if (career instanceof Map<?, ?> careerMap) {
                Object job = careerMap.get("job");
                Object careerCompany = careerMap.get("company");
                if (job != null && !job.toString().isEmpty()) {
                    content.append("Career Job: ").append(job).append("\n");
                }
                if (careerCompany != null && !careerCompany.toString().isEmpty()) {
                    content.append("Career Company: ").append(careerCompany).append("\n");
                }
            }
        }
        
        // Tags
        Object tags = contactData.get("tags");
        if (tags instanceof List<?> tagList && !tagList.isEmpty()) {
            content.append("\nTags:\n");
            content.append("-----\n");
            for (int i = 0; i < tagList.size(); i++) {
                if (tagList.get(i) instanceof Map<?, ?> tag) {
                    Object tagName = tag.get("name");
                    Object tagId = tag.get("id");
                    content.append("• ").append(tagName != null ? tagName : "Unnamed Tag");
                    if (tagId != null) {
                        content.append(" (ID: ").append(tagId).append(")");
                    }
                    content.append("\n");
                }
            }
        }
        
        // Statistics
        Object statistics = contactData.get("statistics");
        if (statistics instanceof Map<?, ?> statsMap) {
            content.append("\nActivity Statistics:\n");
            content.append("------------------\n");
            Object notes = statsMap.get("number_of_notes");
            Object activities = statsMap.get("number_of_activities");
            Object reminders = statsMap.get("number_of_reminders");
            
            if (notes != null) {
                content.append("Notes: ").append(notes).append("\n");
            }
            if (activities != null) {
                content.append("Activities: ").append(activities).append("\n");
            }
            if (reminders != null) {
                content.append("Reminders: ").append(reminders).append("\n");
            }
        }
        
        // Timestamps
        Object createdAt = contactData.get("createdAt");
        if (createdAt != null) {
            content.append("\nCreated: ").append(createdAt).append("\n");
        }
        Object updatedAt = contactData.get("updatedAt");
        if (updatedAt != null) {
            content.append("Updated: ").append(updatedAt).append("\n");
        }
        
        return content.toString();
    }
    
    private String formatContactName(Contact contact) {
        StringBuilder name = new StringBuilder();
        if (contact.getFirstName() != null) {
            name.append(contact.getFirstName());
        }
        if (contact.getLastName() != null) {
            if (name.length() > 0) name.append(" ");
            name.append(contact.getLastName());
        }
        return name.length() > 0 ? name.toString() : "Unnamed Contact";
    }
    
    // Activity Formatting
    
    /**
     * Formats an Activity entity for Claude Desktop visibility.
     */
    public String formatActivity(Activity activity) {
        if (activity == null) {
            return "Activity: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Activity Details:\n");
        content.append("================\n");
        content.append("ID: ").append(activity.getId()).append("\n");
        content.append("Summary: ").append(Optional.ofNullable(activity.getSummary()).orElse("No summary")).append("\n");
        content.append("Description: ").append(Optional.ofNullable(activity.getDescription()).orElse("No description")).append("\n");
        
        if (activity.getDate() != null) {
            content.append("Date: ").append(activity.getDate().format(DATETIME_FORMATTER)).append("\n");
        }
        
        content.append("Type: ").append(Optional.ofNullable(activity.getType()).orElse("Unknown")).append("\n");
        content.append("Contact ID: ").append(activity.getContactId()).append("\n");
        
        if (activity.getDuration() != null) {
            content.append("Duration: ").append(activity.getDuration()).append(" minutes\n");
        }
        
        if (activity.getCreatedAt() != null) {
            content.append("Created: ").append(activity.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (activity.getUpdatedAt() != null) {
            content.append("Updated: ").append(activity.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // Task Formatting
    
    /**
     * Formats a Task entity for Claude Desktop visibility.
     */
    public String formatTask(Task task) {
        if (task == null) {
            return "Task: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Task Details:\n");
        content.append("============\n");
        content.append("ID: ").append(task.getId()).append("\n");
        content.append("Title: ").append(Optional.ofNullable(task.getTitle()).orElse("Untitled")).append("\n");
        content.append("Description: ").append(Optional.ofNullable(task.getDescription()).orElse("No description")).append("\n");
        content.append("Completed: ").append(task.getCompleted() != null ? (task.getCompleted() ? "Yes" : "No") : "Unknown").append("\n");
        
        if (task.getCompletedAt() != null) {
            content.append("Completed At: ").append(task.getCompletedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        content.append("Contact ID: ").append(task.getContactId()).append("\n");
        
        if (task.getCreatedAt() != null) {
            content.append("Created: ").append(task.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (task.getUpdatedAt() != null) {
            content.append("Updated: ").append(task.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // Note Formatting
    
    /**
     * Formats a Note entity for Claude Desktop visibility.
     */
    public String formatNote(Note note) {
        if (note == null) {
            return "Note: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Note Details:\n");
        content.append("============\n");
        content.append("ID: ").append(note.getId()).append("\n");
        content.append("Body: ").append(Optional.ofNullable(note.getBody()).orElse("Empty note")).append("\n");
        content.append("Contact ID: ").append(note.getContactId()).append("\n");
        
        if (note.getCreatedAt() != null) {
            content.append("Created: ").append(note.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (note.getUpdatedAt() != null) {
            content.append("Updated: ").append(note.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // Call Formatting
    
    /**
     * Formats a Call entity for Claude Desktop visibility.
     */
    public String formatCall(Call call) {
        if (call == null) {
            return "Call: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Call Details:\n");
        content.append("============\n");
        content.append("ID: ").append(call.getId()).append("\n");
        content.append("Contact ID: ").append(call.getContactId()).append("\n");
        
        if (call.getCalledAt() != null) {
            content.append("Called At: ").append(call.getCalledAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        if (call.getDuration() != null) {
            content.append("Duration: ").append(call.getDuration()).append(" minutes\n");
        }
        
        if (call.getType() != null) {
            content.append("Type: ").append(call.getType()).append("\n");
        }
        
        if (call.getContent() != null && !call.getContent().trim().isEmpty()) {
            content.append("Content: ").append(call.getContent()).append("\n");
        }
        
        if (call.getCreatedAt() != null) {
            content.append("Created: ").append(call.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (call.getUpdatedAt() != null) {
            content.append("Updated: ").append(call.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // Tag Formatting
    
    /**
     * Formats a Tag entity for Claude Desktop visibility.
     */
    public String formatTag(Tag tag) {
        if (tag == null) {
            return "Tag: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Tag Details:\n");
        content.append("===========\n");
        content.append("ID: ").append(tag.getId()).append("\n");
        content.append("Name: ").append(Optional.ofNullable(tag.getName()).orElse("Unnamed")).append("\n");
        
        if (tag.getNameSlug() != null && !tag.getNameSlug().trim().isEmpty()) {
            content.append("Name Slug: ").append(tag.getNameSlug()).append("\n");
        }
        
        if (tag.getColor() != null && !tag.getColor().trim().isEmpty()) {
            content.append("Color: ").append(tag.getColor()).append("\n");
        }
        
        if (tag.getContactCount() != null) {
            content.append("Contact Count: ").append(tag.getContactCount()).append("\n");
        }
        
        if (tag.getCreatedAt() != null) {
            content.append("Created: ").append(tag.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (tag.getUpdatedAt() != null) {
            content.append("Updated: ").append(tag.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // Reminder Formatting
    
    /**
     * Formats a Reminder entity for Claude Desktop visibility.
     */
    public String formatReminder(Reminder reminder) {
        if (reminder == null) {
            return "Reminder: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Reminder Details:\n");
        content.append("================\n");
        content.append("ID: ").append(reminder.getId()).append("\n");
        content.append("Title: ").append(Optional.ofNullable(reminder.getTitle()).orElse("Untitled")).append("\n");
        
        if (reminder.getDescription() != null && !reminder.getDescription().trim().isEmpty()) {
            content.append("Description: ").append(reminder.getDescription()).append("\n");
        }
        
        content.append("Contact ID: ").append(reminder.getContactId()).append("\n");
        
        if (reminder.getInitialDate() != null) {
            content.append("Initial Date: ").append(reminder.getInitialDate().format(DATE_FORMATTER)).append("\n");
        }
        
        if (reminder.getFrequency() != null && !reminder.getFrequency().trim().isEmpty()) {
            content.append("Frequency: ").append(reminder.getFrequency()).append("\n");
        }
        
        if (reminder.getCreatedAt() != null) {
            content.append("Created: ").append(reminder.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (reminder.getUpdatedAt() != null) {
            content.append("Updated: ").append(reminder.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // Journal Entry Formatting
    
    /**
     * Formats a JournalEntry entity for Claude Desktop visibility.
     */
    public String formatJournalEntry(JournalEntry journalEntry) {
        if (journalEntry == null) {
            return "Journal Entry: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Journal Entry Details:\n");
        content.append("=====================\n");
        content.append("ID: ").append(journalEntry.getId()).append("\n");
        content.append("Title: ").append(Optional.ofNullable(journalEntry.getTitle()).orElse("Untitled")).append("\n");
        
        if (journalEntry.getContent() != null && !journalEntry.getContent().trim().isEmpty()) {
            content.append("Content: ").append(journalEntry.getContent()).append("\n");
        }
        
        if (journalEntry.getDate() != null) {
            content.append("Date: ").append(journalEntry.getDate().format(DATE_FORMATTER)).append("\n");
        }
        
        if (journalEntry.getContactIds() != null && !journalEntry.getContactIds().isEmpty()) {
            content.append("Contact IDs: ").append(journalEntry.getContactIds()).append("\n");
        }
        
        if (journalEntry.getCreatedAt() != null) {
            content.append("Created: ").append(journalEntry.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (journalEntry.getUpdatedAt() != null) {
            content.append("Updated: ").append(journalEntry.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    // List Formatting with Pagination
    
    /**
     * Formats a list of entities with pagination information for Claude Desktop visibility.
     */
    public <T> String formatList(List<T> items, String entityType, int page, int totalPages, long totalItems) {
        StringBuilder content = new StringBuilder();
        content.append(entityType).append(" List:\n");
        content.append("=".repeat(entityType.length() + 6)).append("\n");
        
        if (items == null || items.isEmpty()) {
            content.append("No ").append(entityType.toLowerCase()).append("s found.\n");
        } else {
            content.append("Showing ").append(items.size()).append(" items (Page ").append(page)
                   .append(" of ").append(totalPages).append(", Total: ").append(totalItems).append(")\n\n");
            
            for (int i = 0; i < items.size(); i++) {
                T item = items.get(i);
                content.append((i + 1)).append(". ");
                
                // Format individual items based on type
                if (item instanceof Contact) {
                    content.append(formatContactSummary((Contact) item));
                } else if (item instanceof Activity) {
                    content.append(formatActivitySummary((Activity) item));
                } else if (item instanceof Task) {
                    content.append(formatTaskSummary((Task) item));
                } else if (item instanceof Note) {
                    content.append(formatNoteSummary((Note) item));
                } else if (item instanceof Call) {
                    content.append(formatCallSummary((Call) item));
                } else if (item instanceof Tag) {
                    content.append(formatTagSummary((Tag) item));
                } else if (item instanceof Reminder) {
                    content.append(formatReminderSummary((Reminder) item));
                } else if (item instanceof JournalEntry) {
                    content.append(formatJournalEntrySummary((JournalEntry) item));
                } else if (item instanceof Conversation) {
                    content.append(formatConversationSummary((Conversation) item));
                } else if (item instanceof ConversationMessage) {
                    content.append(formatConversationMessageSummary((ConversationMessage) item));
                } else if (item instanceof ContactField) {
                    content.append(formatContactFieldSummary((ContactField) item));
                } else if (item instanceof ContactTag) {
                    content.append(formatContactTagSummary((ContactTag) item));
                } else {
                    content.append(item.toString());
                }
                content.append("\n");
            }
        }
        
        return content.toString();
    }
    
    // Summary formatters for lists
    
    private String formatContactSummary(Contact contact) {
        return String.format("Contact #%d: %s", contact.getId(), formatContactName(contact));
    }
    
    private String formatActivitySummary(Activity activity) {
        return String.format("Activity #%d: %s", activity.getId(), 
               Optional.ofNullable(activity.getSummary()).orElse("No summary"));
    }
    
    private String formatTaskSummary(Task task) {
        String status = task.getCompleted() != null ? (task.getCompleted() ? "✓" : "○") : "?";
        return String.format("Task #%d: %s %s", task.getId(), status,
               Optional.ofNullable(task.getTitle()).orElse("Untitled"));
    }
    
    private String formatNoteSummary(Note note) {
        String preview = Optional.ofNullable(note.getBody()).orElse("Empty note");
        if (preview.length() > 50) {
            preview = preview.substring(0, 47) + "...";
        }
        return String.format("Note #%d: %s", note.getId(), preview);
    }
    
    private String formatCallSummary(Call call) {
        String type = Optional.ofNullable(call.getType()).orElse("unknown");
        String duration = call.getDuration() != null ? call.getDuration() + " min" : "no duration";
        return String.format("Call #%d: %s call, %s", call.getId(), type, duration);
    }
    
    private String formatTagSummary(Tag tag) {
        String name = Optional.ofNullable(tag.getName()).orElse("Unnamed");
        String color = tag.getColor() != null ? " (" + tag.getColor() + ")" : "";
        String contactCount = tag.getContactCount() != null ? ", " + tag.getContactCount() + " contacts" : "";
        return String.format("Tag #%d: %s%s%s", tag.getId(), name, color, contactCount);
    }
    
    private String formatReminderSummary(Reminder reminder) {
        String title = Optional.ofNullable(reminder.getTitle()).orElse("Untitled");
        String date = reminder.getInitialDate() != null ? 
            " (due " + reminder.getInitialDate().format(DATE_FORMATTER) + ")" : "";
        return String.format("Reminder #%d: %s%s", reminder.getId(), title, date);
    }
    
    private String formatJournalEntrySummary(JournalEntry journalEntry) {
        String title = Optional.ofNullable(journalEntry.getTitle()).orElse("Untitled");
        String date = journalEntry.getDate() != null ? 
            " (" + journalEntry.getDate().format(DATE_FORMATTER) + ")" : "";
        return String.format("Journal Entry #%d: %s%s", journalEntry.getId(), title, date);
    }
    
    private String formatConversationSummary(Conversation conversation) {
        String subject = Optional.ofNullable(conversation.getSubject()).orElse("No subject");
        String date = conversation.getHappenedAt() != null ? 
            " (" + conversation.getHappenedAt().format(DATETIME_FORMATTER) + ")" : "";
        return String.format("Conversation #%d: %s%s", conversation.getId(), subject, date);
    }
    
    private String formatConversationMessageSummary(ConversationMessage message) {
        String content = Optional.ofNullable(message.getContent()).orElse("No content");
        if (content.length() > 50) {
            content = content.substring(0, 47) + "...";
        }
        String author = message.getWrittenByMe() != null ? (message.getWrittenByMe() ? "Me" : "Contact") : "Unknown";
        return String.format("Message #%d: [%s] %s", message.getId(), author, content);
    }
    
    private String formatContactFieldSummary(ContactField contactField) {
        String data = Optional.ofNullable(contactField.getData()).orElse("No data");
        if (data.length() > 30) {
            data = data.substring(0, 27) + "...";
        }
        return String.format("Field #%d: Type %d - %s", contactField.getId(), contactField.getContactFieldTypeId(), data);
    }
    
    private String formatContactTagSummary(ContactTag contactTag) {
        return String.format("Contact #%d -> Tag #%d", contactTag.getContactId(), contactTag.getTagId());
    }
    
    // Conversation Formatting
    
    /**
     * Formats a Conversation entity for Claude Desktop visibility.
     */
    public String formatConversation(Conversation conversation) {
        if (conversation == null) {
            return "Conversation: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Conversation Details:\n");
        content.append("===================\n");
        content.append("ID: ").append(conversation.getId()).append("\n");
        content.append("Contact ID: ").append(conversation.getContactId()).append("\n");
        
        if (conversation.getSubject() != null && !conversation.getSubject().trim().isEmpty()) {
            content.append("Subject: ").append(conversation.getSubject()).append("\n");
        }
        
        if (conversation.getHappenedAt() != null) {
            content.append("Happened At: ").append(conversation.getHappenedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        if (conversation.getMessages() != null && !conversation.getMessages().isEmpty()) {
            content.append("Message Count: ").append(conversation.getMessages().size()).append("\n");
        }
        
        if (conversation.getCreatedAt() != null) {
            content.append("Created: ").append(conversation.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (conversation.getUpdatedAt() != null) {
            content.append("Updated: ").append(conversation.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    /**
     * Formats a ConversationMessage entity for Claude Desktop visibility.
     */
    public String formatConversationMessage(ConversationMessage message) {
        if (message == null) {
            return "Conversation Message: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Conversation Message Details:\n");
        content.append("============================\n");
        content.append("ID: ").append(message.getId()).append("\n");
        content.append("Conversation ID: ").append(message.getConversationId()).append("\n");
        content.append("Content: ").append(Optional.ofNullable(message.getContent()).orElse("No content")).append("\n");
        
        if (message.getWrittenAt() != null) {
            content.append("Written At: ").append(message.getWrittenAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        content.append("Written By Me: ").append(message.getWrittenByMe() != null ? (message.getWrittenByMe() ? "Yes" : "No") : "Unknown").append("\n");
        
        if (message.getCreatedAt() != null) {
            content.append("Created: ").append(message.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (message.getUpdatedAt() != null) {
            content.append("Updated: ").append(message.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    /**
     * Formats a ContactField entity for Claude Desktop visibility.
     */
    public String formatContactField(ContactField contactField) {
        if (contactField == null) {
            return "Contact Field: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Contact Field Details:\n");
        content.append("=====================\n");
        content.append("ID: ").append(contactField.getId()).append("\n");
        content.append("Contact ID: ").append(contactField.getContactId()).append("\n");
        content.append("Field Type ID: ").append(contactField.getContactFieldTypeId()).append("\n");
        content.append("Data: ").append(Optional.ofNullable(contactField.getData()).orElse("No data")).append("\n");
        
        if (contactField.getCreatedAt() != null) {
            content.append("Created: ").append(contactField.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        if (contactField.getUpdatedAt() != null) {
            content.append("Updated: ").append(contactField.getUpdatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    /**
     * Formats a ContactTag entity for Claude Desktop visibility.
     */
    public String formatContactTag(ContactTag contactTag) {
        if (contactTag == null) {
            return "Contact Tag: Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append("Contact Tag Details:\n");
        content.append("===================\n");
        content.append("Contact ID: ").append(contactTag.getContactId()).append("\n");
        content.append("Tag ID: ").append(contactTag.getTagId()).append("\n");
        
        if (contactTag.getCreatedAt() != null) {
            content.append("Created: ").append(contactTag.getCreatedAt().format(DATETIME_FORMATTER)).append("\n");
        }
        
        return content.toString();
    }
    
    /**
     * Formats a simple success/error message for operations like create, update, delete.
     */
    public String formatOperationResult(String operation, String entityType, Long entityId, boolean success, String message) {
        StringBuilder content = new StringBuilder();
        content.append(operation).append(" ").append(entityType).append(" Result:\n");
        content.append("=".repeat(operation.length() + entityType.length() + 9)).append("\n");
        
        if (success) {
            content.append("✅ Success: ").append(operation).append(" completed successfully");
            if (entityId != null) {
                content.append(" for ").append(entityType).append(" #").append(entityId);
            }
            content.append("\n");
        } else {
            content.append("❌ Error: ").append(operation).append(" failed");
            if (entityId != null) {
                content.append(" for ").append(entityType).append(" #").append(entityId);
            }
            content.append("\n");
        }
        
        if (message != null && !message.trim().isEmpty()) {
            content.append("Details: ").append(message).append("\n");
        }
        
        return content.toString();
    }
    
    /**
     * Generic formatter for any Monica API entity from raw data.
     * Ensures complete Constitutional Principle VI compliance by including ALL fields.
     */
    public String formatEntityFromRawData(Map<String, Object> entityData, String entityType) {
        if (entityData == null) {
            return entityType + ": Not found";
        }
        
        StringBuilder content = new StringBuilder();
        content.append(entityType).append(" Details:\n");
        content.append("=".repeat(Math.max(entityType.length() + 9, 15))).append("\n");
        
        // Format all fields recursively
        formatMapFields(content, entityData, "");
        
        return content.toString();
    }
    
    /**
     * Recursively formats all fields in a map structure.
     * Handles nested objects, arrays, and primitive values.
     */
    private void formatMapFields(StringBuilder content, Map<String, Object> data, String prefix) {
        for (Map.Entry<String, Object> entry : data.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            String displayKey = formatFieldName(key);
            
            if (value == null) {
                content.append(prefix).append(displayKey).append(": null\n");
            } else if (value instanceof Map<?, ?> nestedMap) {
                content.append(prefix).append(displayKey).append(":\n");
                @SuppressWarnings("unchecked")
                Map<String, Object> typedMap = (Map<String, Object>) nestedMap;
                formatMapFields(content, typedMap, prefix + "  ");
            } else if (value instanceof List<?> list) {
                content.append(prefix).append(displayKey).append(":\n");
                formatListFields(content, list, prefix + "  ");
            } else {
                // Handle primitive values and strings
                String formattedValue = formatFieldValue(value, key);
                content.append(prefix).append(displayKey).append(": ").append(formattedValue).append("\n");
            }
        }
    }
    
    /**
     * Formats list fields with proper structure.
     */
    private void formatListFields(StringBuilder content, List<?> list, String prefix) {
        if (list.isEmpty()) {
            content.append(prefix).append("(empty)\n");
            return;
        }
        
        for (int i = 0; i < list.size(); i++) {
            Object item = list.get(i);
            if (item instanceof Map<?, ?> mapItem) {
                content.append(prefix).append("• Item ").append(i + 1).append(":\n");
                @SuppressWarnings("unchecked")
                Map<String, Object> typedMap = (Map<String, Object>) mapItem;
                formatMapFields(content, typedMap, prefix + "    ");
            } else {
                content.append(prefix).append("• ").append(item != null ? item.toString() : "null").append("\n");
            }
        }
    }
    
    /**
     * Converts API field names to human-readable display names.
     */
    private String formatFieldName(String fieldName) {
        if (fieldName == null) return "Unknown Field";
        
        // Convert snake_case to Title Case
        String[] parts = fieldName.split("_");
        StringBuilder result = new StringBuilder();
        for (String part : parts) {
            if (result.length() > 0) result.append(" ");
            if (part.length() > 0) {
                result.append(Character.toUpperCase(part.charAt(0)))
                      .append(part.substring(1).toLowerCase());
            }
        }
        
        // Convert camelCase to Title Case
        if (parts.length == 1) {
            StringBuilder camelResult = new StringBuilder();
            for (int i = 0; i < fieldName.length(); i++) {
                char c = fieldName.charAt(i);
                if (i > 0 && Character.isUpperCase(c)) {
                    camelResult.append(" ");
                }
                camelResult.append(i == 0 ? Character.toUpperCase(c) : c);
            }
            return camelResult.toString();
        }
        
        return result.toString();
    }
    
    /**
     * Formats field values with context-appropriate formatting.
     */
    private String formatFieldValue(Object value, String fieldName) {
        if (value == null) return "null";
        
        String stringValue = value.toString();
        
        // Special formatting for certain field types
        if (fieldName != null) {
            String lowerFieldName = fieldName.toLowerCase();
            
            // Boolean fields
            if (lowerFieldName.startsWith("is_") || lowerFieldName.startsWith("has_") ||
                lowerFieldName.contains("boolean") || value instanceof Boolean) {
                return Boolean.TRUE.equals(value) ? "Yes" : "No";
            }
            
            // Date/time fields
            if (lowerFieldName.contains("date") || lowerFieldName.contains("time") || 
                lowerFieldName.contains("at") || lowerFieldName.contains("created") ||
                lowerFieldName.contains("updated")) {
                // Value is already formatted by Monica API, just return as-is
                return stringValue;
            }
            
            // URL fields
            if (lowerFieldName.contains("url") || lowerFieldName.contains("link")) {
                return stringValue;
            }
        }
        
        return stringValue;
    }
}