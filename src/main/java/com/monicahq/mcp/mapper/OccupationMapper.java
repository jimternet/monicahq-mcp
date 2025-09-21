package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Occupation;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class OccupationMapper {

    public Occupation toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Occupation.builder()
            .id(getLong(apiData, "id"))
            .contactId(getLong(apiData, "contact_id"))
            .companyId(getLong(apiData, "company_id"))
            .title(getString(apiData, "title"))
            .description(getString(apiData, "description"))
            .salary(getString(apiData, "salary"))
            .salaryUnit(getString(apiData, "salary_unit"))
            .currentlyWorksHere(getBoolean(apiData, "currently_works_here"))
            .startDate(getString(apiData, "start_date"))
            .endDate(getString(apiData, "end_date"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Occupation occupation) {
        if (occupation == null) {
            return null;
        }

        return Map.of(
            "contact_id", occupation.getContactId() != null ? occupation.getContactId() : "",
            "company_id", occupation.getCompanyId() != null ? occupation.getCompanyId() : "",
            "title", occupation.getTitle() != null ? occupation.getTitle() : "",
            "description", occupation.getDescription() != null ? occupation.getDescription() : "",
            "salary", occupation.getSalary() != null ? occupation.getSalary() : "",
            "salary_unit", occupation.getSalaryUnit() != null ? occupation.getSalaryUnit() : "",
            "currently_works_here", occupation.getCurrentlyWorksHere() != null ? occupation.getCurrentlyWorksHere() : false,
            "start_date", occupation.getStartDate() != null ? occupation.getStartDate() : "",
            "end_date", occupation.getEndDate() != null ? occupation.getEndDate() : ""
        );
    }

    public void updateFromDto(Occupation target, Occupation source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getContactId() != null) {
            target.setContactId(source.getContactId());
        }
        if (source.getCompanyId() != null) {
            target.setCompanyId(source.getCompanyId());
        }
        if (source.getTitle() != null) {
            target.setTitle(source.getTitle());
        }
        if (source.getDescription() != null) {
            target.setDescription(source.getDescription());
        }
        if (source.getSalary() != null) {
            target.setSalary(source.getSalary());
        }
        if (source.getSalaryUnit() != null) {
            target.setSalaryUnit(source.getSalaryUnit());
        }
        if (source.getCurrentlyWorksHere() != null) {
            target.setCurrentlyWorksHere(source.getCurrentlyWorksHere());
        }
        if (source.getStartDate() != null) {
            target.setStartDate(source.getStartDate());
        }
        if (source.getEndDate() != null) {
            target.setEndDate(source.getEndDate());
        }
    }

    private Long getLong(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof Number) return ((Number) value).longValue();
        try {
            return Long.parseLong(value.toString());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private Boolean getBoolean(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof Boolean) return (Boolean) value;
        try {
            return Boolean.parseBoolean(value.toString());
        } catch (Exception e) {
            return null;
        }
    }

    private String getString(Map<String, Object> map, String key) {
        Object value = map.get(key);
        return value != null ? value.toString() : null;
    }

    private java.time.LocalDateTime getLocalDateTime(Map<String, Object> map, String key) {
        String value = getString(map, key);
        if (value == null) return null;
        try {
            return java.time.LocalDateTime.parse(value.replace("Z", ""));
        } catch (Exception e) {
            return null;
        }
    }
}