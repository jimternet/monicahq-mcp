package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Document;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class DocumentMapper {

    public Document toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Document.builder()
            .id(getLong(apiData, "id"))
            .contactId(getLong(apiData, "contact_id"))
            .filename(getString(apiData, "filename"))
            .originalFilename(getString(apiData, "original_filename"))
            .mimeType(getString(apiData, "mime_type"))
            .size(getLong(apiData, "size"))
            .description(getString(apiData, "description"))
            .downloadUrl(getString(apiData, "download_url"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Document document) {
        if (document == null) {
            return null;
        }

        return Map.of(
            "contact_id", document.getContactId() != null ? document.getContactId() : "",
            "filename", document.getFilename() != null ? document.getFilename() : "",
            "original_filename", document.getOriginalFilename() != null ? document.getOriginalFilename() : "",
            "mime_type", document.getMimeType() != null ? document.getMimeType() : "",
            "size", document.getSize() != null ? document.getSize() : "",
            "description", document.getDescription() != null ? document.getDescription() : ""
        );
    }

    public void updateFromDto(Document target, Document source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getContactId() != null) {
            target.setContactId(source.getContactId());
        }
        if (source.getFilename() != null) {
            target.setFilename(source.getFilename());
        }
        if (source.getOriginalFilename() != null) {
            target.setOriginalFilename(source.getOriginalFilename());
        }
        if (source.getMimeType() != null) {
            target.setMimeType(source.getMimeType());
        }
        if (source.getSize() != null) {
            target.setSize(source.getSize());
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