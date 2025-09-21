package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.ActivityTypeCategory;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class ActivityTypeCategoryMapper {

    public ActivityTypeCategory toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return ActivityTypeCategory.builder()
            .id(getLong(apiData, "id"))
            .name(getString(apiData, "name"))
            .parentId(getLong(apiData, "parent_id"))
            .description(getString(apiData, "description"))
            .sortOrder(getInteger(apiData, "sort_order"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(ActivityTypeCategory category) {
        if (category == null) {
            return null;
        }

        return Map.of(
            "name", category.getName() != null ? category.getName() : "",
            "parent_id", category.getParentId() != null ? category.getParentId() : "",
            "description", category.getDescription() != null ? category.getDescription() : "",
            "sort_order", category.getSortOrder() != null ? category.getSortOrder() : ""
        );
    }

    public void updateFromDto(ActivityTypeCategory target, ActivityTypeCategory source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getName() != null) {
            target.setName(source.getName());
        }
        if (source.getParentId() != null) {
            target.setParentId(source.getParentId());
        }
        if (source.getDescription() != null) {
            target.setDescription(source.getDescription());
        }
        if (source.getSortOrder() != null) {
            target.setSortOrder(source.getSortOrder());
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

    private Integer getInteger(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof Number) return ((Number) value).intValue();
        try {
            return Integer.parseInt(value.toString());
        } catch (NumberFormatException e) {
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