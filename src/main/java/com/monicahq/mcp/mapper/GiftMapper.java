package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Gift;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;

@Component
public class GiftMapper {

    public Gift toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Gift.builder()
            .id(getLong(apiData, "id"))
            .contactId(getLong(apiData, "contact_id"))
            .name(getString(apiData, "name"))
            .comment(getString(apiData, "comment"))
            .url(getString(apiData, "url"))
            .value(getBigDecimal(apiData, "value"))
            .valueInBaseCurrency(getBigDecimal(apiData, "value_in_base_currency"))
            .status(getString(apiData, "status"))
            .date(getString(apiData, "date"))
            .isFor(getString(apiData, "is_for"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Gift gift) {
        if (gift == null) {
            return null;
        }

        return Map.of(
            "contact_id", gift.getContactId() != null ? gift.getContactId() : "",
            "name", gift.getName() != null ? gift.getName() : "",
            "comment", gift.getComment() != null ? gift.getComment() : "",
            "url", gift.getUrl() != null ? gift.getUrl() : "",
            "value", gift.getValue() != null ? gift.getValue() : "",
            "status", gift.getStatus() != null ? gift.getStatus() : "",
            "date", gift.getDate() != null ? gift.getDate() : "",
            "is_for", gift.getIsFor() != null ? gift.getIsFor() : ""
        );
    }

    public void updateFromDto(Gift target, Gift source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getContactId() != null) {
            target.setContactId(source.getContactId());
        }
        if (source.getName() != null) {
            target.setName(source.getName());
        }
        if (source.getComment() != null) {
            target.setComment(source.getComment());
        }
        if (source.getUrl() != null) {
            target.setUrl(source.getUrl());
        }
        if (source.getValue() != null) {
            target.setValue(source.getValue());
        }
        if (source.getStatus() != null) {
            target.setStatus(source.getStatus());
        }
        if (source.getDate() != null) {
            target.setDate(source.getDate());
        }
        if (source.getIsFor() != null) {
            target.setIsFor(source.getIsFor());
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

    private BigDecimal getBigDecimal(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof BigDecimal) return (BigDecimal) value;
        if (value instanceof Number) return new BigDecimal(value.toString());
        try {
            return new BigDecimal(value.toString());
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