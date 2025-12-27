package com.monicahq.mcp.service.base;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Comprehensive unit tests for ValidationUtils utility class.
 * Tests all validation methods including edge cases and error handling.
 */
class ValidationUtilsTest {

    // ========================================================================================
    // CONSTANTS TESTS
    // ========================================================================================

    @Test
    @DisplayName("Default constants have expected values")
    void defaultConstants_HaveExpectedValues() {
        assertEquals(1, ValidationUtils.DEFAULT_MIN_LIMIT);
        assertEquals(100, ValidationUtils.DEFAULT_MAX_LIMIT);
        assertEquals(10, ValidationUtils.DEFAULT_LIMIT);
    }

    // ========================================================================================
    // validateRequired() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateRequired()")
    class ValidateRequiredTests {

        @Test
        @DisplayName("should pass when field exists and is not null")
        void validField_Passes() {
            Map<String, Object> args = Map.of("name", "John");
            assertDoesNotThrow(() -> ValidationUtils.validateRequired(args, "name"));
        }

        @Test
        @DisplayName("should throw when arguments is null")
        void nullArguments_ThrowsException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequired(null, "name"));
            assertEquals("name is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field is missing")
        void missingField_ThrowsException() {
            Map<String, Object> args = Map.of("other", "value");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequired(args, "name"));
            assertEquals("name is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field value is null")
        void nullFieldValue_ThrowsException() {
            Map<String, Object> args = new HashMap<>();
            args.put("name", null);
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequired(args, "name"));
            assertEquals("name is required", exception.getMessage());
        }

        @Test
        @DisplayName("should use custom error message when provided")
        void customErrorMessage_UsedInException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequired(null, "name", "Custom error message"));
            assertEquals("Custom error message", exception.getMessage());
        }

        @Test
        @DisplayName("should accept empty string as valid (only checks null)")
        void emptyString_IsAccepted() {
            Map<String, Object> args = Map.of("name", "");
            assertDoesNotThrow(() -> ValidationUtils.validateRequired(args, "name"));
        }
    }

    // ========================================================================================
    // validateRequiredFields() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateRequiredFields()")
    class ValidateRequiredFieldsTests {

        @Test
        @DisplayName("should pass when all required fields exist")
        void allFieldsPresent_Passes() {
            Map<String, Object> args = Map.of("contactId", 1L, "body", "test");
            Set<String> required = Set.of("contactId", "body");
            assertDoesNotThrow(() -> ValidationUtils.validateRequiredFields(args, required, "Note"));
        }

        @Test
        @DisplayName("should throw when arguments is null")
        void nullArguments_ThrowsException() {
            Set<String> required = Set.of("contactId");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredFields(null, required, "Note"));
            assertEquals("Note arguments cannot be empty", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when arguments is empty")
        void emptyArguments_ThrowsException() {
            Set<String> required = Set.of("contactId");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredFields(Map.of(), required, "Note"));
            assertEquals("Note arguments cannot be empty", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when required field is missing")
        void missingRequiredField_ThrowsException() {
            Map<String, Object> args = Map.of("body", "test");
            Set<String> required = Set.of("contactId", "body");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredFields(args, required, "Note"));
            assertEquals("contactId is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when required field value is null")
        void nullRequiredField_ThrowsException() {
            Map<String, Object> args = new HashMap<>();
            args.put("contactId", null);
            args.put("body", "test");
            Set<String> required = Set.of("contactId", "body");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredFields(args, required, "Note"));
            assertEquals("contactId is required", exception.getMessage());
        }

        @Test
        @DisplayName("should pass with empty required fields set")
        void emptyRequiredSet_Passes() {
            Map<String, Object> args = Map.of("any", "value");
            assertDoesNotThrow(() -> ValidationUtils.validateRequiredFields(args, Set.of(), "Note"));
        }
    }

    // ========================================================================================
    // validateNotEmpty() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateNotEmpty()")
    class ValidateNotEmptyTests {

        @Test
        @DisplayName("should pass when arguments is not empty")
        void nonEmptyArguments_Passes() {
            Map<String, Object> args = Map.of("key", "value");
            assertDoesNotThrow(() -> ValidationUtils.validateNotEmpty(args, "Note"));
        }

        @Test
        @DisplayName("should throw when arguments is null")
        void nullArguments_ThrowsException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateNotEmpty(null, "Note"));
            assertEquals("Note arguments cannot be empty", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when arguments is empty")
        void emptyArguments_ThrowsException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateNotEmpty(Map.of(), "Task"));
            assertEquals("Task arguments cannot be empty", exception.getMessage());
        }
    }

    // ========================================================================================
    // validateRequiredString() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateRequiredString()")
    class ValidateRequiredStringTests {

        @Test
        @DisplayName("should pass when field is a non-empty string")
        void nonEmptyString_Passes() {
            Map<String, Object> args = Map.of("body", "Hello world");
            assertDoesNotThrow(() -> ValidationUtils.validateRequiredString(args, "body"));
        }

        @Test
        @DisplayName("should throw when arguments is null")
        void nullArguments_ThrowsException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredString(null, "body"));
            assertEquals("body is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field is missing")
        void missingField_ThrowsException() {
            Map<String, Object> args = Map.of("other", "value");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredString(args, "body"));
            assertEquals("body is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field value is null")
        void nullFieldValue_ThrowsException() {
            Map<String, Object> args = new HashMap<>();
            args.put("body", null);
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredString(args, "body"));
            assertEquals("body is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field value is empty string")
        void emptyString_ThrowsException() {
            Map<String, Object> args = Map.of("body", "");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredString(args, "body"));
            assertEquals("body is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field value is whitespace only")
        void whitespaceOnly_ThrowsException() {
            Map<String, Object> args = Map.of("body", "   ");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredString(args, "body"));
            assertEquals("body is required", exception.getMessage());
        }

        @Test
        @DisplayName("should pass when field has whitespace but also content")
        void whitespaceWithContent_Passes() {
            Map<String, Object> args = Map.of("body", "  Hello  ");
            assertDoesNotThrow(() -> ValidationUtils.validateRequiredString(args, "body"));
        }

        @Test
        @DisplayName("should use custom error message when provided")
        void customErrorMessage_UsedInException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateRequiredString(null, "body", "Body content is required"));
            assertEquals("Body content is required", exception.getMessage());
        }
    }

    // ========================================================================================
    // extractId() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("extractId()")
    class ExtractIdTests {

        @Test
        @DisplayName("should extract Long value directly")
        void longValue_ExtractedDirectly() {
            Map<String, Object> args = Map.of("id", 42L);
            Long result = ValidationUtils.extractId(args, "Note");
            assertEquals(42L, result);
        }

        @Test
        @DisplayName("should convert Integer to Long")
        void integerValue_ConvertedToLong() {
            Map<String, Object> args = Map.of("id", 42);
            Long result = ValidationUtils.extractId(args, "Note");
            assertEquals(42L, result);
        }

        @Test
        @DisplayName("should parse String to Long")
        void stringValue_ParsedToLong() {
            Map<String, Object> args = Map.of("id", "42");
            Long result = ValidationUtils.extractId(args, "Note");
            assertEquals(42L, result);
        }

        @Test
        @DisplayName("should trim String before parsing")
        void stringWithWhitespace_TrimmedAndParsed() {
            Map<String, Object> args = Map.of("id", "  42  ");
            Long result = ValidationUtils.extractId(args, "Note");
            assertEquals(42L, result);
        }

        @Test
        @DisplayName("should throw when arguments is null")
        void nullArguments_ThrowsException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.extractId(null, "Note"));
            assertEquals("Note ID is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when id field is missing")
        void missingIdField_ThrowsException() {
            Map<String, Object> args = Map.of("other", "value");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.extractId(args, "Note"));
            assertEquals("Note ID is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when id value is null")
        void nullIdValue_ThrowsException() {
            Map<String, Object> args = new HashMap<>();
            args.put("id", null);
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.extractId(args, "Note"));
            assertEquals("Note ID is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when id is not a valid number")
        void invalidNumberFormat_ThrowsException() {
            Map<String, Object> args = Map.of("id", "not-a-number");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.extractId(args, "Note"));
            assertTrue(exception.getMessage().contains("Invalid note ID format"));
            assertTrue(exception.getMessage().contains("not-a-number"));
        }

        @Test
        @DisplayName("should use custom id field name")
        void customIdFieldName_Extracted() {
            Map<String, Object> args = Map.of("noteId", 123L);
            Long result = ValidationUtils.extractId(args, "noteId", "Note");
            assertEquals(123L, result);
        }

        @Test
        @DisplayName("should handle Double values")
        void doubleValue_ConvertedToLong() {
            Map<String, Object> args = Map.of("id", 42.0);
            Long result = ValidationUtils.extractId(args, "Note");
            assertEquals(42L, result);
        }

        @Test
        @DisplayName("should handle negative numbers")
        void negativeNumber_Parsed() {
            Map<String, Object> args = Map.of("id", "-42");
            Long result = ValidationUtils.extractId(args, "Note");
            assertEquals(-42L, result);
        }
    }

    // ========================================================================================
    // validateIdFormat() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateIdFormat()")
    class ValidateIdFormatTests {

        @Test
        @DisplayName("should pass for Long value")
        void longValue_Passes() {
            Map<String, Object> args = Map.of("contactId", 42L);
            assertDoesNotThrow(() -> ValidationUtils.validateIdFormat(args, "contactId"));
        }

        @Test
        @DisplayName("should pass for Integer value")
        void integerValue_Passes() {
            Map<String, Object> args = Map.of("contactId", 42);
            assertDoesNotThrow(() -> ValidationUtils.validateIdFormat(args, "contactId"));
        }

        @Test
        @DisplayName("should pass for valid String number")
        void validStringNumber_Passes() {
            Map<String, Object> args = Map.of("contactId", "42");
            assertDoesNotThrow(() -> ValidationUtils.validateIdFormat(args, "contactId"));
        }

        @Test
        @DisplayName("should throw when arguments is null")
        void nullArguments_ThrowsException() {
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateIdFormat(null, "contactId"));
            assertEquals("contactId is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field is missing")
        void missingField_ThrowsException() {
            Map<String, Object> args = Map.of("other", "value");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateIdFormat(args, "contactId"));
            assertEquals("contactId is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when field value is null")
        void nullFieldValue_ThrowsException() {
            Map<String, Object> args = new HashMap<>();
            args.put("contactId", null);
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateIdFormat(args, "contactId"));
            assertEquals("contactId is required", exception.getMessage());
        }

        @Test
        @DisplayName("should throw when value is not a valid number")
        void invalidNumber_ThrowsException() {
            Map<String, Object> args = Map.of("contactId", "invalid");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateIdFormat(args, "contactId"));
            assertEquals("contactId must be a valid number", exception.getMessage());
        }

        @Test
        @DisplayName("should use custom error message when provided")
        void customErrorMessage_UsedInException() {
            Map<String, Object> args = Map.of("contactId", "invalid");
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ValidationUtils.validateIdFormat(args, "contactId", "Contact ID format is invalid"));
            assertEquals("Contact ID format is invalid", exception.getMessage());
        }
    }

    // ========================================================================================
    // validateNumericRange() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateNumericRange()")
    class ValidateNumericRangeTests {

        @Test
        @DisplayName("should return value when within range")
        void withinRange_ReturnsValue() {
            assertEquals(50, ValidationUtils.validateNumericRange(50, 1, 100));
        }

        @Test
        @DisplayName("should return min when value is below range")
        void belowRange_ReturnsMin() {
            assertEquals(1, ValidationUtils.validateNumericRange(0, 1, 100));
            assertEquals(1, ValidationUtils.validateNumericRange(-10, 1, 100));
        }

        @Test
        @DisplayName("should return max when value is above range")
        void aboveRange_ReturnsMax() {
            assertEquals(100, ValidationUtils.validateNumericRange(200, 1, 100));
            assertEquals(100, ValidationUtils.validateNumericRange(Integer.MAX_VALUE, 1, 100));
        }

        @Test
        @DisplayName("should return min when value equals min")
        void atMin_ReturnsMin() {
            assertEquals(1, ValidationUtils.validateNumericRange(1, 1, 100));
        }

        @Test
        @DisplayName("should return max when value equals max")
        void atMax_ReturnsMax() {
            assertEquals(100, ValidationUtils.validateNumericRange(100, 1, 100));
        }
    }

    // ========================================================================================
    // validateLimit() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validateLimit()")
    class ValidateLimitTests {

        @Test
        @DisplayName("should return default when value is null")
        void nullValue_ReturnsDefault() {
            assertEquals(10, ValidationUtils.validateLimit(null));
        }

        @Test
        @DisplayName("should return valid limit within range")
        void validLimit_Returned() {
            assertEquals(50, ValidationUtils.validateLimit(50));
        }

        @Test
        @DisplayName("should clamp to minimum when below range")
        void belowMinimum_ClampsToMin() {
            assertEquals(1, ValidationUtils.validateLimit(0));
            assertEquals(1, ValidationUtils.validateLimit(-5));
        }

        @Test
        @DisplayName("should clamp to maximum when above range")
        void aboveMaximum_ClampsToMax() {
            assertEquals(100, ValidationUtils.validateLimit(200));
            assertEquals(100, ValidationUtils.validateLimit(Integer.MAX_VALUE));
        }

        @Test
        @DisplayName("should parse String value")
        void stringValue_Parsed() {
            assertEquals(25, ValidationUtils.validateLimit("25"));
        }

        @Test
        @DisplayName("should parse String with whitespace")
        void stringWithWhitespace_Parsed() {
            assertEquals(25, ValidationUtils.validateLimit("  25  "));
        }

        @Test
        @DisplayName("should return default for invalid String")
        void invalidString_ReturnsDefault() {
            assertEquals(10, ValidationUtils.validateLimit("invalid"));
        }

        @Test
        @DisplayName("should handle Integer value")
        void integerValue_Handled() {
            assertEquals(30, ValidationUtils.validateLimit(Integer.valueOf(30)));
        }

        @Test
        @DisplayName("should handle Long value")
        void longValue_Handled() {
            assertEquals(40, ValidationUtils.validateLimit(40L));
        }

        @Test
        @DisplayName("should handle Double value")
        void doubleValue_Handled() {
            assertEquals(35, ValidationUtils.validateLimit(35.7));
        }
    }

    // ========================================================================================
    // validatePage() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("validatePage()")
    class ValidatePageTests {

        @Test
        @DisplayName("should return 1 when value is null")
        void nullValue_ReturnsOne() {
            assertEquals(1, ValidationUtils.validatePage(null));
        }

        @Test
        @DisplayName("should return valid page number")
        void validPage_Returned() {
            assertEquals(5, ValidationUtils.validatePage(5));
        }

        @Test
        @DisplayName("should return 1 when page is 0")
        void zeroPage_ReturnsOne() {
            assertEquals(1, ValidationUtils.validatePage(0));
        }

        @Test
        @DisplayName("should return 1 when page is negative")
        void negativePage_ReturnsOne() {
            assertEquals(1, ValidationUtils.validatePage(-5));
        }

        @Test
        @DisplayName("should parse String value")
        void stringValue_Parsed() {
            assertEquals(3, ValidationUtils.validatePage("3"));
        }

        @Test
        @DisplayName("should parse String with whitespace")
        void stringWithWhitespace_Parsed() {
            assertEquals(3, ValidationUtils.validatePage("  3  "));
        }

        @Test
        @DisplayName("should return 1 for invalid String")
        void invalidString_ReturnsOne() {
            assertEquals(1, ValidationUtils.validatePage("invalid"));
        }

        @Test
        @DisplayName("should handle Integer value")
        void integerValue_Handled() {
            assertEquals(10, ValidationUtils.validatePage(Integer.valueOf(10)));
        }

        @Test
        @DisplayName("should handle Long value")
        void longValue_Handled() {
            assertEquals(20, ValidationUtils.validatePage(20L));
        }
    }

    // ========================================================================================
    // hasValue() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("hasValue()")
    class HasValueTests {

        @Test
        @DisplayName("should return true when field exists with non-null value")
        void fieldWithValue_ReturnsTrue() {
            Map<String, Object> args = Map.of("name", "John");
            assertTrue(ValidationUtils.hasValue(args, "name"));
        }

        @Test
        @DisplayName("should return false when arguments is null")
        void nullArguments_ReturnsFalse() {
            assertFalse(ValidationUtils.hasValue(null, "name"));
        }

        @Test
        @DisplayName("should return false when field is missing")
        void missingField_ReturnsFalse() {
            Map<String, Object> args = Map.of("other", "value");
            assertFalse(ValidationUtils.hasValue(args, "name"));
        }

        @Test
        @DisplayName("should return false when field value is null")
        void nullFieldValue_ReturnsFalse() {
            Map<String, Object> args = new HashMap<>();
            args.put("name", null);
            assertFalse(ValidationUtils.hasValue(args, "name"));
        }

        @Test
        @DisplayName("should return true for empty string")
        void emptyString_ReturnsTrue() {
            Map<String, Object> args = Map.of("name", "");
            assertTrue(ValidationUtils.hasValue(args, "name"));
        }
    }

    // ========================================================================================
    // hasNonEmptyString() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("hasNonEmptyString()")
    class HasNonEmptyStringTests {

        @Test
        @DisplayName("should return true for non-empty string")
        void nonEmptyString_ReturnsTrue() {
            Map<String, Object> args = Map.of("name", "John");
            assertTrue(ValidationUtils.hasNonEmptyString(args, "name"));
        }

        @Test
        @DisplayName("should return false when arguments is null")
        void nullArguments_ReturnsFalse() {
            assertFalse(ValidationUtils.hasNonEmptyString(null, "name"));
        }

        @Test
        @DisplayName("should return false when field is missing")
        void missingField_ReturnsFalse() {
            Map<String, Object> args = Map.of("other", "value");
            assertFalse(ValidationUtils.hasNonEmptyString(args, "name"));
        }

        @Test
        @DisplayName("should return false when field value is null")
        void nullFieldValue_ReturnsFalse() {
            Map<String, Object> args = new HashMap<>();
            args.put("name", null);
            assertFalse(ValidationUtils.hasNonEmptyString(args, "name"));
        }

        @Test
        @DisplayName("should return false for empty string")
        void emptyString_ReturnsFalse() {
            Map<String, Object> args = Map.of("name", "");
            assertFalse(ValidationUtils.hasNonEmptyString(args, "name"));
        }

        @Test
        @DisplayName("should return false for whitespace-only string")
        void whitespaceString_ReturnsFalse() {
            Map<String, Object> args = Map.of("name", "   ");
            assertFalse(ValidationUtils.hasNonEmptyString(args, "name"));
        }

        @Test
        @DisplayName("should return true for string with leading/trailing whitespace")
        void stringWithWhitespace_ReturnsTrue() {
            Map<String, Object> args = Map.of("name", "  John  ");
            assertTrue(ValidationUtils.hasNonEmptyString(args, "name"));
        }
    }

    // ========================================================================================
    // getLongValue() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("getLongValue()")
    class GetLongValueTests {

        @Test
        @DisplayName("should return Long value directly")
        void longValue_ReturnedDirectly() {
            Map<String, Object> args = Map.of("id", 42L);
            assertEquals(42L, ValidationUtils.getLongValue(args, "id"));
        }

        @Test
        @DisplayName("should convert Integer to Long")
        void integerValue_Converted() {
            Map<String, Object> args = Map.of("id", 42);
            assertEquals(42L, ValidationUtils.getLongValue(args, "id"));
        }

        @Test
        @DisplayName("should parse String to Long")
        void stringValue_Parsed() {
            Map<String, Object> args = Map.of("id", "42");
            assertEquals(42L, ValidationUtils.getLongValue(args, "id"));
        }

        @Test
        @DisplayName("should return null when arguments is null")
        void nullArguments_ReturnsNull() {
            assertNull(ValidationUtils.getLongValue(null, "id"));
        }

        @Test
        @DisplayName("should return null when field is missing")
        void missingField_ReturnsNull() {
            Map<String, Object> args = Map.of("other", "value");
            assertNull(ValidationUtils.getLongValue(args, "id"));
        }

        @Test
        @DisplayName("should return null when field value is null")
        void nullFieldValue_ReturnsNull() {
            Map<String, Object> args = new HashMap<>();
            args.put("id", null);
            assertNull(ValidationUtils.getLongValue(args, "id"));
        }

        @Test
        @DisplayName("should return null for invalid number String")
        void invalidString_ReturnsNull() {
            Map<String, Object> args = Map.of("id", "not-a-number");
            assertNull(ValidationUtils.getLongValue(args, "id"));
        }

        @Test
        @DisplayName("should handle Double value")
        void doubleValue_Converted() {
            Map<String, Object> args = Map.of("id", 42.9);
            assertEquals(42L, ValidationUtils.getLongValue(args, "id"));
        }
    }

    // ========================================================================================
    // getStringValue() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("getStringValue()")
    class GetStringValueTests {

        @Test
        @DisplayName("should return trimmed string value")
        void stringValue_ReturnedTrimmed() {
            Map<String, Object> args = Map.of("name", "  John  ");
            assertEquals("John", ValidationUtils.getStringValue(args, "name"));
        }

        @Test
        @DisplayName("should return null when arguments is null")
        void nullArguments_ReturnsNull() {
            assertNull(ValidationUtils.getStringValue(null, "name"));
        }

        @Test
        @DisplayName("should return null when field is missing")
        void missingField_ReturnsNull() {
            Map<String, Object> args = Map.of("other", "value");
            assertNull(ValidationUtils.getStringValue(args, "name"));
        }

        @Test
        @DisplayName("should return null when field value is null")
        void nullFieldValue_ReturnsNull() {
            Map<String, Object> args = new HashMap<>();
            args.put("name", null);
            assertNull(ValidationUtils.getStringValue(args, "name"));
        }

        @Test
        @DisplayName("should convert non-string to string")
        void nonStringValue_Converted() {
            Map<String, Object> args = Map.of("number", 42);
            assertEquals("42", ValidationUtils.getStringValue(args, "number"));
        }

        @Test
        @DisplayName("should return empty string for empty value")
        void emptyString_ReturnsEmpty() {
            Map<String, Object> args = Map.of("name", "");
            assertEquals("", ValidationUtils.getStringValue(args, "name"));
        }
    }

    // ========================================================================================
    // getBooleanValue() TESTS
    // ========================================================================================

    @Nested
    @DisplayName("getBooleanValue()")
    class GetBooleanValueTests {

        @Test
        @DisplayName("should return Boolean value directly")
        void booleanValue_ReturnedDirectly() {
            Map<String, Object> args = Map.of("completed", true);
            assertTrue(ValidationUtils.getBooleanValue(args, "completed", false));
        }

        @Test
        @DisplayName("should return default when arguments is null")
        void nullArguments_ReturnsDefault() {
            assertTrue(ValidationUtils.getBooleanValue(null, "completed", true));
            assertFalse(ValidationUtils.getBooleanValue(null, "completed", false));
        }

        @Test
        @DisplayName("should return default when field is missing")
        void missingField_ReturnsDefault() {
            Map<String, Object> args = Map.of("other", "value");
            assertTrue(ValidationUtils.getBooleanValue(args, "completed", true));
            assertFalse(ValidationUtils.getBooleanValue(args, "completed", false));
        }

        @Test
        @DisplayName("should return default when field value is null")
        void nullFieldValue_ReturnsDefault() {
            Map<String, Object> args = new HashMap<>();
            args.put("completed", null);
            assertTrue(ValidationUtils.getBooleanValue(args, "completed", true));
        }

        @ParameterizedTest
        @ValueSource(strings = {"true", "TRUE", "True"})
        @DisplayName("should parse 'true' string (case insensitive)")
        void trueString_ReturnsTrue(String value) {
            Map<String, Object> args = Map.of("completed", value);
            assertTrue(ValidationUtils.getBooleanValue(args, "completed", false));
        }

        @Test
        @DisplayName("should parse '1' as true")
        void oneString_ReturnsTrue() {
            Map<String, Object> args = Map.of("completed", "1");
            assertTrue(ValidationUtils.getBooleanValue(args, "completed", false));
        }

        @ParameterizedTest
        @ValueSource(strings = {"yes", "YES", "Yes"})
        @DisplayName("should parse 'yes' string (case insensitive)")
        void yesString_ReturnsTrue(String value) {
            Map<String, Object> args = Map.of("completed", value);
            assertTrue(ValidationUtils.getBooleanValue(args, "completed", false));
        }

        @ParameterizedTest
        @ValueSource(strings = {"false", "0", "no", "invalid", "anything"})
        @DisplayName("should return false for non-truthy strings")
        void nonTruthyString_ReturnsFalse(String value) {
            Map<String, Object> args = Map.of("completed", value);
            assertFalse(ValidationUtils.getBooleanValue(args, "completed", true));
        }

        @Test
        @DisplayName("should handle Boolean.FALSE value")
        void booleanFalse_ReturnsFalse() {
            Map<String, Object> args = Map.of("completed", Boolean.FALSE);
            assertFalse(ValidationUtils.getBooleanValue(args, "completed", true));
        }
    }
}
