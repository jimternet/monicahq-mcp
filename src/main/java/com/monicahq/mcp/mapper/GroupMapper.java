package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Group;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class GroupMapper {

    public Group toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Group.builder()
            .id(getLong(apiData, "id"))
            .name(getString(apiData, "name"))
            .description(getString(apiData, "description"))
            .accountId(getLong(apiData, "account_id"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Group group) {
        if (group == null) {
            return null;
        }

        return Map.of(
            "name", group.getName() != null ? group.getName() : "",
            "description", group.getDescription() != null ? group.getDescription() : ""
        );
    }

    public void updateFromDto(Group target, Group source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getName() != null) {
            target.setName(source.getName());
        }
        if (source.getDescription() != null) {
            target.setDescription(source.getDescription());
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