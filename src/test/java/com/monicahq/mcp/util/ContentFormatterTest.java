package com.monicahq.mcp.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.dto.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;

class ContentFormatterTest {

    private ContentFormatter contentFormatter;

    @BeforeEach
    void setUp() {
        contentFormatter = new ContentFormatter(new ObjectMapper());
    }

    @Test
    @DisplayName("Should format Contact entity with complete information")
    void testFormatContact() {
        // Given
        Contact contact = Contact.builder()
                .id(123L)
                .firstName("John")
                .lastName("Doe")
                .nickname("Johnny")
                .genderId(1L)
                .isDeceased(false)
                .birthdate(LocalDate.of(1990, 5, 15))
                .createdAt(LocalDateTime.of(2023, 1, 1, 10, 0, 0))
                .updatedAt(LocalDateTime.of(2023, 6, 1, 14, 30, 0))
                .build();

        // When
        String result = contentFormatter.formatContact(contact);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Contact Details:")
                .contains("ID: 123")
                .contains("Name: John Doe")
                .contains("Nickname: Johnny")
                .contains("Birthdate: 1990-05-15")
                .contains("Gender ID: 1")
                .contains("Deceased: No")
                .contains("Created: 2023-01-01 10:00:00")
                .contains("Updated: 2023-06-01 14:30:00");
    }

    @Test
    @DisplayName("Should handle null Contact gracefully")
    void testFormatContactNull() {
        // When
        String result = contentFormatter.formatContact(null);

        // Then
        assertEquals("Contact: Not found", result);
    }

    @Test
    @DisplayName("Should format Activity entity with complete information")
    void testFormatActivity() {
        // Given
        Activity activity = Activity.builder()
                .id(456L)
                .contactId(123L)
                .type("meeting")
                .summary("Project discussion")
                .description("Discussed Q3 goals and deliverables")
                .date(LocalDateTime.of(2023, 7, 15, 14, 0, 0))
                .duration(60)
                .createdAt(LocalDateTime.of(2023, 7, 15, 15, 0, 0))
                .updatedAt(LocalDateTime.of(2023, 7, 15, 15, 30, 0))
                .build();

        // When
        String result = contentFormatter.formatActivity(activity);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Activity Details:")
                .contains("ID: 456")
                .contains("Summary: Project discussion")
                .contains("Description: Discussed Q3 goals and deliverables")
                .contains("Date: 2023-07-15 14:00:00")
                .contains("Type: meeting")
                .contains("Contact ID: 123")
                .contains("Duration: 60 minutes")
                .contains("Created: 2023-07-15 15:00:00")
                .contains("Updated: 2023-07-15 15:30:00");
    }

    @Test
    @DisplayName("Should format Task entity with completion status")
    void testFormatTask() {
        // Given
        Task task = Task.builder()
                .id(789L)
                .contactId(123L)
                .title("Review quarterly report")
                .description("Analyze Q2 performance metrics")
                .completed(true)
                .completedAt(LocalDateTime.of(2023, 7, 20, 16, 0, 0))
                .createdAt(LocalDateTime.of(2023, 7, 10, 9, 0, 0))
                .updatedAt(LocalDateTime.of(2023, 7, 20, 16, 0, 0))
                .build();

        // When
        String result = contentFormatter.formatTask(task);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Task Details:")
                .contains("ID: 789")
                .contains("Title: Review quarterly report")
                .contains("Description: Analyze Q2 performance metrics")
                .contains("Completed: Yes")
                .contains("Completed At: 2023-07-20 16:00:00")
                .contains("Contact ID: 123")
                .contains("Created: 2023-07-10 09:00:00")
                .contains("Updated: 2023-07-20 16:00:00");
    }

    @Test
    @DisplayName("Should format Note entity with body content")
    void testFormatNote() {
        // Given
        Note note = Note.builder()
                .id(101L)
                .contactId(123L)
                .body("Remember to follow up on project proposal")
                .createdAt(LocalDateTime.of(2023, 7, 25, 11, 30, 0))
                .updatedAt(LocalDateTime.of(2023, 7, 25, 11, 30, 0))
                .build();

        // When
        String result = contentFormatter.formatNote(note);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Note Details:")
                .contains("ID: 101")
                .contains("Body: Remember to follow up on project proposal")
                .contains("Contact ID: 123")
                .contains("Created: 2023-07-25 11:30:00")
                .contains("Updated: 2023-07-25 11:30:00");
    }

    @Test
    @DisplayName("Should format list with pagination information")
    void testFormatList() {
        // Given
        Contact contact1 = Contact.builder().id(1L).firstName("John").lastName("Doe").build();
        Contact contact2 = Contact.builder().id(2L).firstName("Jane").lastName("Smith").build();
        List<Contact> contacts = Arrays.asList(contact1, contact2);

        // When
        String result = contentFormatter.formatList(contacts, "Contact", 1, 5, 50L);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Contact List:")
                .contains("Showing 2 items (Page 1 of 5, Total: 50)")
                .contains("1. Contact #1: John Doe")
                .contains("2. Contact #2: Jane Smith");
    }

    @Test
    @DisplayName("Should format empty list gracefully")
    void testFormatEmptyList() {
        // When
        String result = contentFormatter.formatList(Collections.emptyList(), "Contact", 1, 1, 0L);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Contact List:")
                .contains("No contacts found.");
    }

    @Test
    @DisplayName("Should format operation success result")
    void testFormatOperationResultSuccess() {
        // When
        String result = contentFormatter.formatOperationResult("Create", "Contact", 123L, true, "Contact created successfully");

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Create Contact Result:")
                .contains("✅ Success: Create completed successfully for Contact #123")
                .contains("Details: Contact created successfully");
    }

    @Test
    @DisplayName("Should format operation failure result")
    void testFormatOperationResultFailure() {
        // When
        String result = contentFormatter.formatOperationResult("Update", "Task", 456L, false, "Validation failed: Title is required");

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("Update Task Result:")
                .contains("❌ Error: Update failed for Task #456")
                .contains("Details: Validation failed: Title is required");
    }

    @Test
    @DisplayName("Should format Task summary for lists with completion indicator")
    void testFormatTaskSummary() {
        // Given
        Task completedTask = Task.builder().id(1L).title("Completed Task").completed(true).build();
        Task pendingTask = Task.builder().id(2L).title("Pending Task").completed(false).build();
        List<Task> tasks = Arrays.asList(completedTask, pendingTask);

        // When
        String result = contentFormatter.formatList(tasks, "Task", 1, 1, 2L);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("1. Task #1: ✓ Completed Task")
                .contains("2. Task #2: ○ Pending Task");
    }

    @Test
    @DisplayName("Should format Note summary with preview for lists")
    void testFormatNoteSummary() {
        // Given
        Note shortNote = Note.builder().id(1L).body("Short note").build();
        Note longNote = Note.builder().id(2L).body("This is a very long note that should be truncated to show only a preview of the content").build();
        List<Note> notes = Arrays.asList(shortNote, longNote);

        // When
        String result = contentFormatter.formatList(notes, "Note", 1, 1, 2L);

        // Then
        assertNotNull(result);
        assertThat(result)
                .contains("1. Note #1: Short note")
                .contains("2. Note #2: This is a very long note that should be truncat...");
    }

    @Test
    @DisplayName("Should format raw API data as escaped JSON")
    void testFormatAsEscapedJson() {
        // Given
        Map<String, Object> rawApiData = new HashMap<>();
        rawApiData.put("id", 123);
        rawApiData.put("first_name", "John");
        rawApiData.put("last_name", "Doe");
        rawApiData.put("email", "john.doe@example.com");
        rawApiData.put("is_deceased", false);
        rawApiData.put("created_at", "2023-01-01T10:00:00Z");
        
        // Add nested object
        Map<String, Object> information = new HashMap<>();
        information.put("career", Map.of("job", "Developer", "company", "TechCorp"));
        rawApiData.put("information", information);
        
        // Add array
        rawApiData.put("tags", Arrays.asList(
            Map.of("id", 1, "name", "Work"),
            Map.of("id", 2, "name", "Friend")
        ));

        // When
        String result = contentFormatter.formatAsEscapedJson(rawApiData);

        // Then
        assertNotNull(result);
        assertFalse(result.isEmpty());
        
        // Verify it's valid JSON by parsing it back
        try {
            ObjectMapper mapper = new ObjectMapper();
            @SuppressWarnings("unchecked")
            Map<String, Object> parsedBack = mapper.readValue(result, Map.class);
            
            // Verify key fields are preserved
            assertEquals(123, parsedBack.get("id"));
            assertEquals("John", parsedBack.get("first_name"));
            assertEquals("Doe", parsedBack.get("last_name"));
            assertEquals("john.doe@example.com", parsedBack.get("email"));
            assertEquals(false, parsedBack.get("is_deceased"));
            assertEquals("2023-01-01T10:00:00Z", parsedBack.get("created_at"));
            
            // Verify nested structures are preserved
            assertTrue(parsedBack.containsKey("information"));
            assertTrue(parsedBack.containsKey("tags"));
            
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> tags = (List<Map<String, Object>>) parsedBack.get("tags");
            assertEquals(2, tags.size());
            assertEquals("Work", tags.get(0).get("name"));
            assertEquals("Friend", tags.get(1).get("name"));
            
        } catch (Exception e) {
            fail("Result should be valid JSON, but failed to parse: " + e.getMessage());
        }
    }

    @Test
    @DisplayName("Should handle null raw API data gracefully")
    void testFormatAsEscapedJsonNull() {
        // When
        String result = contentFormatter.formatAsEscapedJson(null);

        // Then
        assertEquals("{}", result);
        
        // Verify it's valid JSON
        try {
            ObjectMapper mapper = new ObjectMapper();
            Map<String, Object> parsed = mapper.readValue(result, Map.class);
            assertTrue(parsed.isEmpty());
        } catch (Exception e) {
            fail("Result should be valid JSON: " + e.getMessage());
        }
    }

    @Test
    @DisplayName("Should format list response as escaped JSON")
    void testFormatListAsEscapedJson() {
        // Given
        Map<String, Object> listResponse = new HashMap<>();
        
        // Add data array
        List<Map<String, Object>> data = Arrays.asList(
            Map.of("id", 1, "first_name", "John", "last_name", "Doe"),
            Map.of("id", 2, "first_name", "Jane", "last_name", "Smith")
        );
        listResponse.put("data", data);
        
        // Add meta information
        Map<String, Object> meta = new HashMap<>();
        meta.put("current_page", 1);
        meta.put("total_pages", 3);
        meta.put("total", 25);
        meta.put("per_page", 10);
        listResponse.put("meta", meta);

        // When
        String result = contentFormatter.formatListAsEscapedJson(listResponse);

        // Then
        assertNotNull(result);
        assertFalse(result.isEmpty());
        
        // Verify it's valid JSON by parsing it back
        try {
            ObjectMapper mapper = new ObjectMapper();
            @SuppressWarnings("unchecked")
            Map<String, Object> parsedBack = mapper.readValue(result, Map.class);
            
            // Verify data array is preserved
            assertTrue(parsedBack.containsKey("data"));
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> parsedData = (List<Map<String, Object>>) parsedBack.get("data");
            assertEquals(2, parsedData.size());
            assertEquals("John", parsedData.get(0).get("first_name"));
            assertEquals("Jane", parsedData.get(1).get("first_name"));
            
            // Verify meta is preserved
            assertTrue(parsedBack.containsKey("meta"));
            @SuppressWarnings("unchecked")
            Map<String, Object> parsedMeta = (Map<String, Object>) parsedBack.get("meta");
            assertEquals(1, parsedMeta.get("current_page"));
            assertEquals(3, parsedMeta.get("total_pages"));
            assertEquals(25, parsedMeta.get("total"));
            assertEquals(10, parsedMeta.get("per_page"));
            
        } catch (Exception e) {
            fail("Result should be valid JSON, but failed to parse: " + e.getMessage());
        }
    }

    @Test
    @DisplayName("Should preserve all Monica API fields in escaped JSON")
    void testFormatAsEscapedJsonPreservesAllFields() {
        // Given - comprehensive Monica API response
        Map<String, Object> monicaResponse = new HashMap<>();
        monicaResponse.put("id", 456);
        monicaResponse.put("first_name", "Alice");
        monicaResponse.put("last_name", "Johnson");
        monicaResponse.put("nickname", "Ali");
        monicaResponse.put("email", "alice@example.com");
        monicaResponse.put("phone", "+1-555-0123");
        monicaResponse.put("gender_id", 2);
        monicaResponse.put("is_birthdate_known", true);
        monicaResponse.put("birthdate", "1985-03-15");
        monicaResponse.put("is_deceased", false);
        monicaResponse.put("is_deceased_date_known", false);
        monicaResponse.put("job_title", "Senior Developer");
        monicaResponse.put("company", "TechCorp Inc.");
        monicaResponse.put("created_at", "2023-01-01T10:00:00Z");
        monicaResponse.put("updated_at", "2023-06-15T14:30:00Z");
        
        // Add complex nested structures
        Map<String, Object> statistics = Map.of(
            "number_of_notes", 15,
            "number_of_activities", 8,
            "number_of_reminders", 3
        );
        monicaResponse.put("statistics", statistics);
        
        List<Map<String, Object>> tags = Arrays.asList(
            Map.of("id", 1, "name", "Family", "color", "#FF5733"),
            Map.of("id", 2, "name", "Work", "color", "#33FF57")
        );
        monicaResponse.put("tags", tags);

        // When
        String result = contentFormatter.formatAsEscapedJson(monicaResponse);

        // Then
        assertNotNull(result);
        
        // Parse back and verify ALL fields are preserved exactly
        try {
            ObjectMapper mapper = new ObjectMapper();
            @SuppressWarnings("unchecked")
            Map<String, Object> parsed = mapper.readValue(result, Map.class);
            
            // Verify every field is preserved
            assertEquals(456, parsed.get("id"));
            assertEquals("Alice", parsed.get("first_name"));
            assertEquals("Johnson", parsed.get("last_name"));
            assertEquals("Ali", parsed.get("nickname"));
            assertEquals("alice@example.com", parsed.get("email"));
            assertEquals("+1-555-0123", parsed.get("phone"));
            assertEquals(2, parsed.get("gender_id"));
            assertEquals(true, parsed.get("is_birthdate_known"));
            assertEquals("1985-03-15", parsed.get("birthdate"));
            assertEquals(false, parsed.get("is_deceased"));
            assertEquals(false, parsed.get("is_deceased_date_known"));
            assertEquals("Senior Developer", parsed.get("job_title"));
            assertEquals("TechCorp Inc.", parsed.get("company"));
            assertEquals("2023-01-01T10:00:00Z", parsed.get("created_at"));
            assertEquals("2023-06-15T14:30:00Z", parsed.get("updated_at"));
            
            // Verify nested structures are preserved exactly
            @SuppressWarnings("unchecked")
            Map<String, Object> parsedStats = (Map<String, Object>) parsed.get("statistics");
            assertEquals(15, parsedStats.get("number_of_notes"));
            assertEquals(8, parsedStats.get("number_of_activities"));
            assertEquals(3, parsedStats.get("number_of_reminders"));
            
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> parsedTags = (List<Map<String, Object>>) parsed.get("tags");
            assertEquals(2, parsedTags.size());
            assertEquals("Family", parsedTags.get(0).get("name"));
            assertEquals("#FF5733", parsedTags.get(0).get("color"));
            assertEquals("Work", parsedTags.get(1).get("name"));
            assertEquals("#33FF57", parsedTags.get(1).get("color"));
            
        } catch (Exception e) {
            fail("Result should be valid JSON with all fields preserved: " + e.getMessage());
        }
    }

    @Test
    @DisplayName("Should handle JSON serialization errors gracefully")
    void testFormatAsEscapedJsonErrorHandling() {
        // Given - create a map that might cause serialization issues
        Map<String, Object> problematicData = new HashMap<>();
        problematicData.put("id", 123);
        problematicData.put("valid_field", "valid_value");
        
        // When
        String result = contentFormatter.formatAsEscapedJson(problematicData);

        // Then - should still return valid JSON even if there are issues
        assertNotNull(result);
        assertFalse(result.isEmpty());
        
        // Should be parseable as JSON
        try {
            ObjectMapper mapper = new ObjectMapper();
            Map<String, Object> parsed = mapper.readValue(result, Map.class);
            // Should contain at least the valid data
            assertTrue(parsed.containsKey("id") || parsed.containsKey("error"));
        } catch (Exception e) {
            fail("Result should always be valid JSON: " + e.getMessage());
        }
    }

    // Helper method for cleaner assertions
    private ContentAssertion assertThat(String actual) {
        return new ContentAssertion(actual);
    }

    private static class ContentAssertion {
        private final String actual;

        ContentAssertion(String actual) {
            this.actual = actual;
        }

        ContentAssertion contains(String expected) {
            assertTrue(actual.contains(expected), 
                "Expected content to contain: '" + expected + "' but was: '" + actual + "'");
            return this;
        }
    }
}