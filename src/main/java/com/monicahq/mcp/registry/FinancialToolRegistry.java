package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.DebtService;
import com.monicahq.mcp.service.GiftService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Financial Management operations.
 *
 * This registry handles financial-related MCP tools:
 * - Debt CRUD operations (create, get, update, delete, list)
 * - Gift CRUD operations (create, get, update, delete, list)
 *
 * Debts track money owed between the user and contacts (who owes whom).
 * Gifts track gift ideas, purchases, and given gifts for contacts.
 *
 * The registry delegates execution to:
 * - DebtService for debt operations
 * - GiftService for gift operations
 */
@Component
@Slf4j
public class FinancialToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Financial";
    private static final String DEBT_CATEGORY = "Financial Management";
    private static final String GIFT_CATEGORY = "Gift Management";

    private final DebtService debtService;
    private final GiftService giftService;

    public FinancialToolRegistry(
            DebtService debtService,
            GiftService giftService) {
        this.debtService = debtService;
        this.giftService = giftService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Debt CRUD operations (5)
        registerTool(
            "debt_create",
            "[Debt] Create a new debt record",
            createDebtSchema(),
            DEBT_CATEGORY
        );

        registerTool(
            "debt_get",
            "[Debt] Get a debt by ID",
            createIdSchema("Debt ID"),
            DEBT_CATEGORY
        );

        registerTool(
            "debt_update",
            "[Debt] Update an existing debt",
            createDebtUpdateSchema(),
            DEBT_CATEGORY
        );

        registerTool(
            "debt_delete",
            "[Debt] Delete a debt",
            createIdSchema("Debt ID"),
            DEBT_CATEGORY
        );

        registerTool(
            "debt_list",
            "[Debt] List debts with pagination",
            createListSchema(),
            DEBT_CATEGORY
        );

        // Gift CRUD operations (5)
        registerTool(
            "gift_create",
            "[Gift] Create a new gift record",
            createGiftSchema(),
            GIFT_CATEGORY
        );

        registerTool(
            "gift_get",
            "[Gift] Get a gift by ID",
            createIdSchema("Gift ID"),
            GIFT_CATEGORY
        );

        registerTool(
            "gift_update",
            "[Gift] Update an existing gift",
            createGiftUpdateSchema(),
            GIFT_CATEGORY
        );

        registerTool(
            "gift_delete",
            "[Gift] Delete a gift",
            createIdSchema("Gift ID"),
            GIFT_CATEGORY
        );

        registerTool(
            "gift_list",
            "[Gift] List gifts with pagination",
            createListSchema(),
            GIFT_CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Debt operations
            case "debt_create" -> debtService.createDebt(arguments);
            case "debt_get" -> debtService.getDebt(arguments);
            case "debt_update" -> debtService.updateDebt(arguments);
            case "debt_delete" -> debtService.deleteDebt(arguments);
            case "debt_list" -> debtService.listDebts(arguments);

            // Gift operations
            case "gift_create" -> giftService.createGift(arguments);
            case "gift_get" -> giftService.getGift(arguments);
            case "gift_update" -> giftService.updateGift(arguments);
            case "gift_delete" -> giftService.deleteGift(arguments);
            case "gift_list" -> giftService.listGifts(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Debt Schema Methods ==========

    /**
     * Creates the schema for debt creation.
     * Debts track money owed between the user and contacts.
     */
    private Map<String, Object> createDebtSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this debt belongs to (required)"
        ));
        properties.put("amount", Map.of(
            "type", "number",
            "description", "Debt amount (required)"
        ));
        properties.put("currency", Map.of(
            "type", "string",
            "description", "Currency code (optional, max 3 characters)",
            "maxLength", 3
        ));
        properties.put("inDebt", Map.of(
            "type", "string",
            "description", "Who is in debt - 'contact' or 'user' (optional)"
        ));
        properties.put("status", Map.of(
            "type", "string",
            "description", "Debt status (optional)",
            "maxLength", 255
        ));
        properties.put("reason", Map.of(
            "type", "string",
            "description", "Reason for the debt (optional)",
            "maxLength", 1000
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "amount"));

        return schema;
    }

    /**
     * Creates the schema for debt updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createDebtUpdateSchema() {
        return createUpdateSchema(createDebtSchema());
    }

    // ========== Gift Schema Methods ==========

    /**
     * Creates the schema for gift creation.
     * Gifts track gift ideas, purchases, and given gifts for contacts.
     */
    private Map<String, Object> createGiftSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this gift is for (required)"
        ));
        properties.put("name", Map.of(
            "type", "string",
            "description", "Gift name (required)",
            "maxLength", 255
        ));
        properties.put("comment", Map.of(
            "type", "string",
            "description", "Gift comment or notes (optional)",
            "maxLength", 1000
        ));
        properties.put("url", Map.of(
            "type", "string",
            "description", "Gift URL or link (optional)",
            "maxLength", 255
        ));
        properties.put("value", Map.of(
            "type", "number",
            "description", "Gift value/price (optional)"
        ));
        properties.put("status", Map.of(
            "type", "string",
            "description", "Gift status - 'idea', 'purchased', 'given' (optional)",
            "maxLength", 255
        ));
        properties.put("date", Map.of(
            "type", "string",
            "format", "date",
            "description", "Gift date in YYYY-MM-DD format (optional)"
        ));
        properties.put("isFor", Map.of(
            "type", "string",
            "description", "What occasion the gift is for (optional)"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "name"));

        return schema;
    }

    /**
     * Creates the schema for gift updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createGiftUpdateSchema() {
        return createUpdateSchema(createGiftSchema());
    }
}
