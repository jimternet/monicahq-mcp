package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.ActivityType;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class ActivityTypeMapper {

    public ActivityType toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return ActivityType.builder()
            .id(getLong(apiData, "id"))
            .name(getString(apiData, "name"))
            .categoryId(getLong(apiData, "category_id"))
            .description(getString(apiData, "description"))
            .icon(getString(apiData, "icon"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(ActivityType activityType) {
        if (activityType == null) {
            return null;
        }

        return Map.of(
            "name", activityType.getName() != null ? activityType.getName() : "",
            "category_id", activityType.getCategoryId() != null ? activityType.getCategoryId() : "",
            "description", activityType.getDescription() != null ? activityType.getDescription() : "",
            "icon", activityType.getIcon() != null ? activityType.getIcon() : ""
        );
    }

    public void updateFromDto(ActivityType target, ActivityType source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getName() != null) {
            target.setName(source.getName());
        }
        if (source.getCategoryId() != null) {
            target.setCategoryId(source.getCategoryId());
        }
        if (source.getDescription() != null) {
            target.setDescription(source.getDescription());
        }
        if (source.getIcon() != null) {
            target.setIcon(source.getIcon());
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