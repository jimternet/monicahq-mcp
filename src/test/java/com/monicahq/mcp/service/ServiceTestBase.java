package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import org.mockito.ArgumentMatcher;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

/**
 * Base class for service unit tests providing reusable mocking patterns
 * for MonicaHqClient and ContentFormatter.
 *
 * <p>This class provides helper methods to:
 * <ul>
 *   <li>Build common mock API responses (single entities, lists, delete, errors)</li>
 *   <li>Set up MonicaHqClient mock behaviors for GET, POST, PUT, DELETE operations</li>
 *   <li>Set up ContentFormatter mock behaviors for JSON formatting</li>
 *   <li>Create test data for common entities (contacts, activities, tasks, notes, etc.)</li>
 * </ul>
 *
 * <p>Subclasses should use Mockito annotations:
 * <pre>{@code
 * @ExtendWith(MockitoExtension.class)
 * class MyServiceTest extends ServiceTestBase {
 *     @Mock
 *     private MonicaHqClient monicaClient;
 *
 *     @Mock
 *     private ContentFormatter contentFormatter;
 *
 *     @InjectMocks
 *     private MyService myService;
 * }
 * }</pre>
 */
public abstract class ServiceTestBase {

    protected static final DateTimeFormatter DATETIME_FORMATTER =
            DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'");
    protected static final DateTimeFormatter DATE_FORMATTER =
            DateTimeFormatter.ofPattern("yyyy-MM-dd");

    // ========================================================================================
    // MOCK RESPONSE BUILDERS
    // ========================================================================================

    /**
     * Creates a mock API response wrapping a single entity in a "data" field.
     *
     * @param entityData the entity data to wrap
     * @return a Map containing {"data": entityData}
     */
    protected Map<String, Object> createSingleEntityResponse(Map<String, Object> entityData) {
        Map<String, Object> response = new HashMap<>();
        response.put("data", entityData);
        return response;
    }

    /**
     * Creates a mock API response for a list of entities with pagination metadata.
     *
     * @param entities list of entity data maps
     * @param page current page number
     * @param perPage items per page
     * @param total total number of items
     * @return a Map containing {"data": entities, "meta": {...}}
     */
    protected Map<String, Object> createListResponse(
            List<Map<String, Object>> entities,
            int page,
            int perPage,
            int total) {
        Map<String, Object> response = new HashMap<>();
        response.put("data", entities);
        response.put("meta", createPaginationMeta(page, perPage, total));
        return response;
    }

    /**
     * Creates a mock API response for a list of entities with default pagination.
     * Defaults to page 1, 10 per page, with total equal to list size.
     *
     * @param entities list of entity data maps
     * @return a Map containing {"data": entities, "meta": {...}}
     */
    protected Map<String, Object> createListResponse(List<Map<String, Object>> entities) {
        return createListResponse(entities, 1, 10, entities.size());
    }

    /**
     * Creates pagination metadata for list responses.
     *
     * @param page current page number
     * @param perPage items per page
     * @param total total number of items
     * @return a Map containing pagination metadata
     */
    protected Map<String, Object> createPaginationMeta(int page, int perPage, int total) {
        Map<String, Object> meta = new HashMap<>();
        meta.put("current_page", page);
        meta.put("per_page", perPage);
        meta.put("total", total);
        meta.put("last_page", (int) Math.ceil((double) total / perPage));
        return meta;
    }

    /**
     * Creates a mock API response for a successful delete operation.
     *
     * @param entityId the ID of the deleted entity
     * @return a Map indicating successful deletion
     */
    protected Map<String, Object> createDeleteResponse(Long entityId) {
        Map<String, Object> response = new HashMap<>();
        response.put("deleted", true);
        response.put("id", entityId);
        return response;
    }

    /**
     * Creates a mock API error response.
     *
     * @param errorMessage the error message
     * @return a Map containing an error
     */
    protected Map<String, Object> createErrorResponse(String errorMessage) {
        Map<String, Object> response = new HashMap<>();
        response.put("error", errorMessage);
        return response;
    }

    /**
     * Creates a mock API validation error response.
     *
     * @param field the field that failed validation
     * @param message the validation error message
     * @return a Map containing validation errors
     */
    protected Map<String, Object> createValidationErrorResponse(String field, String message) {
        Map<String, Object> response = new HashMap<>();
        Map<String, List<String>> errors = new HashMap<>();
        errors.put(field, List.of(message));
        response.put("errors", errors);
        return response;
    }

    // ========================================================================================
    // MONICAHQCLIENT MOCK SETUP HELPERS
    // ========================================================================================

    /**
     * Sets up a mock for MonicaHqClient.get() to return a specific response.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param response the response to return
     */
    protected void mockGet(MonicaHqClient monicaClient, String endpoint, Map<String, Object> response) {
        when(monicaClient.get(eq(endpoint), any())).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.get() with query parameter matching.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param paramMatcher matcher for query parameters
     * @param response the response to return
     */
    protected void mockGetWithParams(
            MonicaHqClient monicaClient,
            String endpoint,
            ArgumentMatcher<Map<String, String>> paramMatcher,
            Map<String, Object> response) {
        when(monicaClient.get(eq(endpoint), argThat(paramMatcher))).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.get() with flexible parameter checking.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpointPattern a regex pattern to match the endpoint
     * @param response the response to return
     */
    protected void mockGetWithPattern(
            MonicaHqClient monicaClient,
            String endpointPattern,
            Map<String, Object> response) {
        when(monicaClient.get(matches(endpointPattern), any())).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.post() to return a specific response.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param response the response to return
     */
    protected void mockPost(MonicaHqClient monicaClient, String endpoint, Map<String, Object> response) {
        when(monicaClient.post(eq(endpoint), any())).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.post() with request body matching.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param bodyMatcher matcher for the request body
     * @param response the response to return
     */
    protected void mockPostWithBody(
            MonicaHqClient monicaClient,
            String endpoint,
            ArgumentMatcher<Map<String, Object>> bodyMatcher,
            Map<String, Object> response) {
        when(monicaClient.post(eq(endpoint), argThat(bodyMatcher))).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.put() to return a specific response.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param response the response to return
     */
    protected void mockPut(MonicaHqClient monicaClient, String endpoint, Map<String, Object> response) {
        when(monicaClient.put(eq(endpoint), any())).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.put() with request body matching.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param bodyMatcher matcher for the request body
     * @param response the response to return
     */
    protected void mockPutWithBody(
            MonicaHqClient monicaClient,
            String endpoint,
            ArgumentMatcher<Map<String, Object>> bodyMatcher,
            Map<String, Object> response) {
        when(monicaClient.put(eq(endpoint), argThat(bodyMatcher))).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.delete() to return a specific response.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint (exact match)
     * @param response the response to return
     */
    protected void mockDelete(MonicaHqClient monicaClient, String endpoint, Map<String, Object> response) {
        when(monicaClient.delete(eq(endpoint))).thenReturn(Mono.just(response));
    }

    /**
     * Sets up a mock for MonicaHqClient.get() to throw an error.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint
     * @param error the exception to throw
     */
    protected void mockGetError(MonicaHqClient monicaClient, String endpoint, Exception error) {
        when(monicaClient.get(eq(endpoint), any())).thenReturn(Mono.error(error));
    }

    /**
     * Sets up a mock for MonicaHqClient.post() to throw an error.
     *
     * @param monicaClient the mocked MonicaHqClient
     * @param endpoint the expected endpoint
     * @param error the exception to throw
     */
    protected void mockPostError(MonicaHqClient monicaClient, String endpoint, Exception error) {
        when(monicaClient.post(eq(endpoint), any())).thenReturn(Mono.error(error));
    }

    // ========================================================================================
    // CONTENTFORMATTER MOCK SETUP HELPERS
    // ========================================================================================

    /**
     * Sets up ContentFormatter to return a standard formatted JSON string.
     *
     * @param contentFormatter the mocked ContentFormatter
     */
    protected void mockContentFormatter(ContentFormatter contentFormatter) {
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted single JSON content");
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON content");
    }

    /**
     * Sets up ContentFormatter.formatAsEscapedJson() to return a specific string.
     *
     * @param contentFormatter the mocked ContentFormatter
     * @param output the formatted string to return
     */
    protected void mockFormatAsEscapedJson(ContentFormatter contentFormatter, String output) {
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn(output);
    }

    /**
     * Sets up ContentFormatter.formatListAsEscapedJson() to return a specific string.
     *
     * @param contentFormatter the mocked ContentFormatter
     * @param output the formatted string to return
     */
    protected void mockFormatListAsEscapedJson(ContentFormatter contentFormatter, String output) {
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn(output);
    }

    // ========================================================================================
    // ENTITY DATA BUILDERS
    // ========================================================================================

    /**
     * Builder for creating mock contact data.
     */
    protected ContactDataBuilder contactBuilder() {
        return new ContactDataBuilder();
    }

    /**
     * Builder for creating mock activity data.
     */
    protected ActivityDataBuilder activityBuilder() {
        return new ActivityDataBuilder();
    }

    /**
     * Builder for creating mock task data.
     */
    protected TaskDataBuilder taskBuilder() {
        return new TaskDataBuilder();
    }

    /**
     * Builder for creating mock note data.
     */
    protected NoteDataBuilder noteBuilder() {
        return new NoteDataBuilder();
    }

    /**
     * Builder for creating mock tag data.
     */
    protected TagDataBuilder tagBuilder() {
        return new TagDataBuilder();
    }

    /**
     * Builder for creating mock call data.
     */
    protected CallDataBuilder callBuilder() {
        return new CallDataBuilder();
    }

    /**
     * Builder for creating mock reminder data.
     */
    protected ReminderDataBuilder reminderBuilder() {
        return new ReminderDataBuilder();
    }

    /**
     * Builder for creating mock relationship data.
     */
    protected RelationshipDataBuilder relationshipBuilder() {
        return new RelationshipDataBuilder();
    }

    /**
     * Builder for creating mock company data.
     */
    protected CompanyDataBuilder companyBuilder() {
        return new CompanyDataBuilder();
    }

    /**
     * Builder for creating mock gift data.
     */
    protected GiftDataBuilder giftBuilder() {
        return new GiftDataBuilder();
    }

    /**
     * Builder for creating mock debt data.
     */
    protected DebtDataBuilder debtBuilder() {
        return new DebtDataBuilder();
    }

    /**
     * Builder for creating mock occupation data.
     */
    protected OccupationDataBuilder occupationBuilder() {
        return new OccupationDataBuilder();
    }

    /**
     * Builder for creating mock conversation data.
     */
    protected ConversationDataBuilder conversationBuilder() {
        return new ConversationDataBuilder();
    }

    /**
     * Builder for creating mock journal entry data.
     */
    protected JournalEntryDataBuilder journalEntryBuilder() {
        return new JournalEntryDataBuilder();
    }

    /**
     * Builder for creating mock document data.
     */
    protected DocumentDataBuilder documentBuilder() {
        return new DocumentDataBuilder();
    }

    // ========================================================================================
    // ARGUMENT MATCHER HELPERS
    // ========================================================================================

    /**
     * Creates an argument matcher that checks if a map contains a specific key-value pair.
     *
     * @param key the expected key
     * @param value the expected value
     * @return an ArgumentMatcher for maps
     */
    protected ArgumentMatcher<Map<String, Object>> hasEntry(String key, Object value) {
        return map -> map != null && value.equals(map.get(key));
    }

    /**
     * Creates an argument matcher that checks if a map contains a specific key.
     *
     * @param key the expected key
     * @return an ArgumentMatcher for maps
     */
    protected ArgumentMatcher<Map<String, Object>> hasKey(String key) {
        return map -> map != null && map.containsKey(key);
    }

    /**
     * Creates an argument matcher that checks if a map contains all specified keys.
     *
     * @param keys the expected keys
     * @return an ArgumentMatcher for maps
     */
    protected ArgumentMatcher<Map<String, Object>> hasKeys(String... keys) {
        return map -> {
            if (map == null) return false;
            for (String key : keys) {
                if (!map.containsKey(key)) return false;
            }
            return true;
        };
    }

    /**
     * Creates an argument matcher for string maps that checks if it contains a key-value pair.
     *
     * @param key the expected key
     * @param value the expected value
     * @return an ArgumentMatcher for string maps
     */
    protected ArgumentMatcher<Map<String, String>> hasStringEntry(String key, String value) {
        return map -> map != null && value.equals(map.get(key));
    }

    /**
     * Creates an argument matcher using a predicate.
     *
     * @param predicate the predicate to match
     * @return an ArgumentMatcher
     */
    protected <T> ArgumentMatcher<T> matching(Predicate<T> predicate) {
        return argument -> argument != null && predicate.test(argument);
    }

    // ========================================================================================
    // UTILITY METHODS
    // ========================================================================================

    /**
     * Formats a LocalDateTime to the standard API format.
     *
     * @param dateTime the date/time to format
     * @return formatted date/time string
     */
    protected String formatDateTime(LocalDateTime dateTime) {
        return dateTime.format(DATETIME_FORMATTER);
    }

    /**
     * Returns the current timestamp formatted for API responses.
     *
     * @return formatted current date/time string
     */
    protected String now() {
        return formatDateTime(LocalDateTime.now());
    }

    /**
     * Extracts a numeric ID from various formats (Long, Integer, String).
     *
     * @param value the value to extract from
     * @return the numeric ID, or null if extraction fails
     */
    protected Long extractId(Object value) {
        if (value == null) return null;
        if (value instanceof Long) return (Long) value;
        if (value instanceof Integer) return ((Integer) value).longValue();
        if (value instanceof String) {
            try {
                return Long.parseLong((String) value);
            } catch (NumberFormatException e) {
                return null;
            }
        }
        return null;
    }

    // ========================================================================================
    // INNER BUILDER CLASSES
    // ========================================================================================

    /**
     * Builder for creating mock contact data.
     */
    public static class ContactDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public ContactDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("first_name", "John");
            data.put("last_name", "Doe");
            data.put("gender_id", 1);
            data.put("is_birthdate_known", false);
            data.put("is_deceased", false);
            data.put("is_deceased_date_known", false);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public ContactDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public ContactDataBuilder firstName(String firstName) {
            data.put("first_name", firstName);
            return this;
        }

        public ContactDataBuilder lastName(String lastName) {
            data.put("last_name", lastName);
            return this;
        }

        public ContactDataBuilder email(String email) {
            data.put("email", email);
            return this;
        }

        public ContactDataBuilder phone(String phone) {
            data.put("phone", phone);
            return this;
        }

        public ContactDataBuilder genderId(Integer genderId) {
            data.put("gender_id", genderId);
            return this;
        }

        public ContactDataBuilder nickname(String nickname) {
            data.put("nickname", nickname);
            return this;
        }

        public ContactDataBuilder company(String company) {
            data.put("company", company);
            return this;
        }

        public ContactDataBuilder jobTitle(String jobTitle) {
            data.put("job_title", jobTitle);
            return this;
        }

        public ContactDataBuilder birthdate(String birthdate) {
            data.put("birthdate", birthdate);
            data.put("is_birthdate_known", true);
            return this;
        }

        public ContactDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock activity data.
     */
    public static class ActivityDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public ActivityDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("summary", "Test Activity");
            data.put("activity_type_id", 1);
            data.put("happened_at", LocalDateTime.now().format(DATE_FORMATTER));
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public ActivityDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public ActivityDataBuilder summary(String summary) {
            data.put("summary", summary);
            return this;
        }

        public ActivityDataBuilder description(String description) {
            data.put("description", description);
            return this;
        }

        public ActivityDataBuilder activityTypeId(Integer typeId) {
            data.put("activity_type_id", typeId);
            return this;
        }

        public ActivityDataBuilder happenedAt(String date) {
            data.put("happened_at", date);
            return this;
        }

        public ActivityDataBuilder attendees(List<Long> contactIds) {
            List<Map<String, Object>> attendees = new ArrayList<>();
            for (Long contactId : contactIds) {
                Map<String, Object> attendee = new HashMap<>();
                attendee.put("contact", Map.of("id", contactId));
                attendees.add(attendee);
            }
            data.put("attendees", attendees);
            return this;
        }

        public ActivityDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock task data.
     */
    public static class TaskDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public TaskDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("title", "Test Task");
            data.put("completed", false);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public TaskDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public TaskDataBuilder title(String title) {
            data.put("title", title);
            return this;
        }

        public TaskDataBuilder description(String description) {
            data.put("description", description);
            return this;
        }

        public TaskDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public TaskDataBuilder completed(boolean completed) {
            data.put("completed", completed);
            if (completed) {
                data.put("completed_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            }
            return this;
        }

        public TaskDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock note data.
     */
    public static class NoteDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public NoteDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("body", "Test note content");
            data.put("is_favorited", false);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public NoteDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public NoteDataBuilder body(String body) {
            data.put("body", body);
            return this;
        }

        public NoteDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public NoteDataBuilder favorited(boolean favorited) {
            data.put("is_favorited", favorited);
            return this;
        }

        public NoteDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock tag data.
     */
    public static class TagDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public TagDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "Test Tag");
            data.put("name_slug", "test-tag");
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public TagDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public TagDataBuilder name(String name) {
            data.put("name", name);
            data.put("name_slug", name.toLowerCase().replace(" ", "-"));
            return this;
        }

        public TagDataBuilder contactCount(Integer count) {
            data.put("contact_count", count);
            return this;
        }

        public TagDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock call data.
     */
    public static class CallDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public CallDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("content", "Test call notes");
            data.put("called_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public CallDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public CallDataBuilder content(String content) {
            data.put("content", content);
            return this;
        }

        public CallDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public CallDataBuilder calledAt(String calledAt) {
            data.put("called_at", calledAt);
            return this;
        }

        public CallDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock reminder data.
     */
    public static class ReminderDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public ReminderDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("title", "Test Reminder");
            data.put("initial_date", LocalDateTime.now().format(DATE_FORMATTER));
            data.put("frequency_type", "one_time");
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public ReminderDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public ReminderDataBuilder title(String title) {
            data.put("title", title);
            return this;
        }

        public ReminderDataBuilder description(String description) {
            data.put("description", description);
            return this;
        }

        public ReminderDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public ReminderDataBuilder initialDate(String date) {
            data.put("initial_date", date);
            return this;
        }

        public ReminderDataBuilder frequencyType(String type) {
            data.put("frequency_type", type);
            return this;
        }

        public ReminderDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock relationship data.
     */
    public static class RelationshipDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public RelationshipDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("contact_is", 1L);
            data.put("of_contact", 2L);
            data.put("relationship_type_id", 1);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public RelationshipDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public RelationshipDataBuilder contactIs(Long contactId) {
            data.put("contact_is", contactId);
            return this;
        }

        public RelationshipDataBuilder ofContact(Long contactId) {
            data.put("of_contact", contactId);
            return this;
        }

        public RelationshipDataBuilder relationshipTypeId(Integer typeId) {
            data.put("relationship_type_id", typeId);
            return this;
        }

        public RelationshipDataBuilder notes(String notes) {
            data.put("notes", notes);
            return this;
        }

        public RelationshipDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock company data.
     */
    public static class CompanyDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public CompanyDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "Test Company");
            data.put("website", "https://example.com");
            data.put("number_of_employees", 50);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public CompanyDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public CompanyDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public CompanyDataBuilder website(String website) {
            data.put("website", website);
            return this;
        }

        public CompanyDataBuilder numberOfEmployees(Integer count) {
            data.put("number_of_employees", count);
            return this;
        }

        public CompanyDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock gift data.
     */
    public static class GiftDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public GiftDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "Test Gift");
            data.put("status", "idea");
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public GiftDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public GiftDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public GiftDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public GiftDataBuilder comment(String comment) {
            data.put("comment", comment);
            return this;
        }

        public GiftDataBuilder url(String url) {
            data.put("url", url);
            return this;
        }

        public GiftDataBuilder value(Object value) {
            data.put("value", value);
            return this;
        }

        public GiftDataBuilder valueInBaseCurrency(Object value) {
            data.put("value_in_base_currency", value);
            return this;
        }

        public GiftDataBuilder status(String status) {
            data.put("status", status);
            return this;
        }

        public GiftDataBuilder date(String date) {
            data.put("date", date);
            return this;
        }

        public GiftDataBuilder isFor(String isFor) {
            data.put("is_for", isFor);
            return this;
        }

        public GiftDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock debt data.
     */
    public static class DebtDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public DebtDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("amount", "100.00");
            data.put("status", "inprogress");
            data.put("in_debt", "yes");
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public DebtDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public DebtDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public DebtDataBuilder amount(Object amount) {
            data.put("amount", amount);
            return this;
        }

        public DebtDataBuilder currency(String currency) {
            data.put("currency", currency);
            return this;
        }

        public DebtDataBuilder inDebt(String inDebt) {
            data.put("in_debt", inDebt);
            return this;
        }

        public DebtDataBuilder status(String status) {
            data.put("status", status);
            return this;
        }

        public DebtDataBuilder reason(String reason) {
            data.put("reason", reason);
            return this;
        }

        public DebtDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock occupation data.
     */
    public static class OccupationDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public OccupationDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("title", "Software Engineer");
            data.put("currently_works_here", true);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public OccupationDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public OccupationDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public OccupationDataBuilder companyId(Long companyId) {
            data.put("company_id", companyId);
            data.put("company", Map.of("id", companyId));
            return this;
        }

        public OccupationDataBuilder title(String title) {
            data.put("title", title);
            return this;
        }

        public OccupationDataBuilder description(String description) {
            data.put("description", description);
            return this;
        }

        public OccupationDataBuilder salary(Object salary) {
            data.put("salary", salary);
            return this;
        }

        public OccupationDataBuilder salaryUnit(String salaryUnit) {
            data.put("salary_unit", salaryUnit);
            return this;
        }

        public OccupationDataBuilder currentlyWorksHere(boolean currentlyWorksHere) {
            data.put("currently_works_here", currentlyWorksHere);
            return this;
        }

        public OccupationDataBuilder startDate(String startDate) {
            data.put("start_date", startDate);
            return this;
        }

        public OccupationDataBuilder endDate(String endDate) {
            data.put("end_date", endDate);
            return this;
        }

        public OccupationDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock conversation data.
     */
    public static class ConversationDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public ConversationDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("happened_at", LocalDateTime.now().format(DATE_FORMATTER));
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public ConversationDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public ConversationDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public ConversationDataBuilder happenedAt(String happenedAt) {
            data.put("happened_at", happenedAt);
            return this;
        }

        public ConversationDataBuilder messages(List<Map<String, Object>> messages) {
            data.put("messages", messages);
            return this;
        }

        public ConversationDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock journal entry data.
     */
    public static class JournalEntryDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public JournalEntryDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("title", "Test Journal Entry");
            data.put("post", "Test journal content");
            data.put("date", LocalDateTime.now().format(DATE_FORMATTER));
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public JournalEntryDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public JournalEntryDataBuilder title(String title) {
            data.put("title", title);
            return this;
        }

        public JournalEntryDataBuilder post(String post) {
            data.put("post", post);
            return this;
        }

        public JournalEntryDataBuilder date(String date) {
            data.put("date", date);
            return this;
        }

        public JournalEntryDataBuilder journalEntry(String journalEntry) {
            data.put("journal_entry", journalEntry);
            return this;
        }

        public JournalEntryDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }

    /**
     * Builder for creating mock document data.
     */
    public static class DocumentDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public DocumentDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("filename", "document.pdf");
            data.put("original_filename", "My Document.pdf");
            data.put("mime_type", "application/pdf");
            data.put("size", 1024);
            data.put("created_at", LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public DocumentDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public DocumentDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public DocumentDataBuilder filename(String filename) {
            data.put("filename", filename);
            return this;
        }

        public DocumentDataBuilder originalFilename(String originalFilename) {
            data.put("original_filename", originalFilename);
            return this;
        }

        public DocumentDataBuilder mimeType(String mimeType) {
            data.put("mime_type", mimeType);
            return this;
        }

        public DocumentDataBuilder size(Long size) {
            data.put("size", size);
            return this;
        }

        public DocumentDataBuilder downloadUrl(String downloadUrl) {
            data.put("download_url", downloadUrl);
            return this;
        }

        public DocumentDataBuilder description(String description) {
            data.put("description", description);
            return this;
        }

        public DocumentDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
