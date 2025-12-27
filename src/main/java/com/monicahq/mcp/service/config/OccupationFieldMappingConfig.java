package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Occupation entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Occupation CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>companyId (client) -> company_id (API)</li>
 *   <li>salaryUnit (client) -> salary_unit (API)</li>
 *   <li>currentlyWorksHere (client) -> currently_works_here (API)</li>
 *   <li>startDate (client) -> start_date (API)</li>
 *   <li>endDate (client) -> end_date (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, title (must be non-empty string)
 * </p>
 */
@Component
public class OccupationFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/occupations";
    private static final String ENTITY_NAME = "Occupation";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "companyId", "company_id",
        "salaryUnit", "salary_unit",
        "currentlyWorksHere", "currently_works_here",
        "startDate", "start_date",
        "endDate", "end_date"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "company_id", "companyId",
        "salary_unit", "salaryUnit",
        "currently_works_here", "currentlyWorksHere",
        "start_date", "startDate",
        "end_date", "endDate",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Occupation creation.
     * Note: title validation requires non-empty string check, done in service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "title");

    @Override
    public String getEndpointPath() {
        return ENDPOINT_PATH;
    }

    @Override
    public String getEntityName() {
        return ENTITY_NAME;
    }

    @Override
    public Map<String, String> getToApiMappings() {
        return TO_API_MAPPINGS;
    }

    @Override
    public Map<String, String> getFromApiMappings() {
        return FROM_API_MAPPINGS;
    }

    @Override
    public Set<String> getRequiredCreateFields() {
        return REQUIRED_CREATE_FIELDS;
    }
}
