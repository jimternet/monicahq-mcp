package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Debt;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;

@Component
public class DebtMapper {

    public Debt toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Debt.builder()
            .id(getLong(apiData, "id"))
            .contactId(getLong(apiData, "contact_id"))
            .amount(getBigDecimal(apiData, "amount"))
            .currency(getString(apiData, "currency"))
            .inDebt(getString(apiData, "in_debt"))
            .status(getString(apiData, "status"))
            .reason(getString(apiData, "reason"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Debt debt) {
        if (debt == null) {
            return null;
        }

        return Map.of(
            "contact_id", debt.getContactId() != null ? debt.getContactId() : "",
            "amount", debt.getAmount() != null ? debt.getAmount() : "",
            "currency", debt.getCurrency() != null ? debt.getCurrency() : "",
            "in_debt", debt.getInDebt() != null ? debt.getInDebt() : "",
            "status", debt.getStatus() != null ? debt.getStatus() : "",
            "reason", debt.getReason() != null ? debt.getReason() : ""
        );
    }

    public void updateFromDto(Debt target, Debt source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getContactId() != null) {
            target.setContactId(source.getContactId());
        }
        if (source.getAmount() != null) {
            target.setAmount(source.getAmount());
        }
        if (source.getCurrency() != null) {
            target.setCurrency(source.getCurrency());
        }
        if (source.getInDebt() != null) {
            target.setInDebt(source.getInDebt());
        }
        if (source.getStatus() != null) {
            target.setStatus(source.getStatus());
        }
        if (source.getReason() != null) {
            target.setReason(source.getReason());
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