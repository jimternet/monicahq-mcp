package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.DocumentService;
import com.monicahq.mcp.service.PhotoService;
import com.monicahq.mcp.service.PetService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Content Management operations.
 *
 * This registry handles content-related MCP tools:
 * - Document CRUD operations (create, get, update, delete, list)
 * - Photo CRUD operations (create, get, update, delete, list)
 * - Pet CRUD operations (create, get, update, delete, list)
 *
 * Documents are files associated with contacts (PDFs, documents, etc.).
 * Photos are images associated with contacts.
 * Pets are pet records associated with contacts.
 *
 * The registry delegates execution to:
 * - DocumentService for document operations
 * - PhotoService for photo operations
 * - PetService for pet operations
 *
 * Note: Pet operations use root-level POST /pets endpoint (not /contacts/{id}/pets)
 * to avoid HTTP 405 errors. The contact_id is passed in the request body.
 */
@Component
@Slf4j
public class ContentToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Content";
    private static final String CATEGORY = "Content Management";
    private static final String PET_CATEGORY = "Pet Management";

    private final DocumentService documentService;
    private final PhotoService photoService;
    private final PetService petService;

    public ContentToolRegistry(
            DocumentService documentService,
            PhotoService photoService,
            PetService petService) {
        this.documentService = documentService;
        this.photoService = photoService;
        this.petService = petService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Document CRUD operations (5)
        registerTool(
            "document_create",
            "[Document] Create a new document",
            createDocumentSchema(),
            CATEGORY
        );

        registerTool(
            "document_get",
            "[Document] Get a document by ID",
            createIdSchema("Document ID"),
            CATEGORY
        );

        registerTool(
            "document_update",
            "[Document] Update an existing document",
            createDocumentUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "document_delete",
            "[Document] Delete a document",
            createIdSchema("Document ID"),
            CATEGORY
        );

        registerTool(
            "document_list",
            "[Document] List documents with pagination",
            createListSchema(),
            CATEGORY
        );

        // Photo CRUD operations (5)
        registerTool(
            "photo_create",
            "[Photo] Create a new photo",
            createPhotoSchema(),
            CATEGORY
        );

        registerTool(
            "photo_get",
            "[Photo] Get a photo by ID",
            createIdSchema("Photo ID"),
            CATEGORY
        );

        registerTool(
            "photo_update",
            "[Photo] Update an existing photo",
            createPhotoUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "photo_delete",
            "[Photo] Delete a photo",
            createIdSchema("Photo ID"),
            CATEGORY
        );

        registerTool(
            "photo_list",
            "[Photo] List photos with pagination",
            createListSchema(),
            CATEGORY
        );

        // Pet CRUD operations (5)
        // Note: Uses root-level POST /pets endpoint (not /contacts/{id}/pets) to avoid HTTP 405 errors
        registerTool(
            "pet_create",
            "[Pet] Create a new pet for a contact",
            createPetSchema(),
            PET_CATEGORY
        );

        registerTool(
            "pet_get",
            "[Pet] Get a pet by ID",
            createIdSchema("Pet ID"),
            PET_CATEGORY
        );

        registerTool(
            "pet_update",
            "[Pet] Update an existing pet",
            createPetUpdateSchema(),
            PET_CATEGORY
        );

        registerTool(
            "pet_delete",
            "[Pet] Delete a pet",
            createIdSchema("Pet ID"),
            PET_CATEGORY
        );

        registerTool(
            "pet_list",
            "[Pet] List pets with pagination",
            createListSchema(),
            PET_CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Document operations
            case "document_create" -> documentService.createDocument(arguments);
            case "document_get" -> documentService.getDocument(arguments);
            case "document_update" -> documentService.updateDocument(arguments);
            case "document_delete" -> documentService.deleteDocument(arguments);
            case "document_list" -> documentService.listDocuments(arguments);

            // Photo operations
            case "photo_create" -> photoService.createPhoto(arguments);
            case "photo_get" -> photoService.getPhoto(arguments);
            case "photo_update" -> photoService.updatePhoto(arguments);
            case "photo_delete" -> photoService.deletePhoto(arguments);
            case "photo_list" -> photoService.listPhotos(arguments);

            // Pet operations
            case "pet_create" -> petService.createPet(arguments);
            case "pet_get" -> petService.getPet(arguments);
            case "pet_update" -> petService.updatePet(arguments);
            case "pet_delete" -> petService.deletePet(arguments);
            case "pet_list" -> petService.listPets(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Document Schema Methods ==========

    /**
     * Creates the schema for document creation.
     * Documents are files associated with contacts (PDFs, documents, etc.).
     */
    private Map<String, Object> createDocumentSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this document belongs to (required)"
        ));
        properties.put("filename", Map.of(
            "type", "string",
            "description", "Document filename (required)",
            "maxLength", 255
        ));
        properties.put("originalFilename", Map.of(
            "type", "string",
            "description", "Original filename (optional)",
            "maxLength", 255
        ));
        properties.put("mimeType", Map.of(
            "type", "string",
            "description", "MIME type of the document (optional)",
            "maxLength", 100
        ));
        properties.put("size", Map.of(
            "type", "integer",
            "description", "File size in bytes (optional)"
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Document description (optional)",
            "maxLength", 1000
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "filename"));

        return schema;
    }

    /**
     * Creates the schema for document updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createDocumentUpdateSchema() {
        return createUpdateSchema(createDocumentSchema());
    }

    // ========== Photo Schema Methods ==========

    /**
     * Creates the schema for photo creation.
     * Photos are images associated with contacts.
     */
    private Map<String, Object> createPhotoSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this photo belongs to (required)"
        ));
        properties.put("filename", Map.of(
            "type", "string",
            "description", "Photo filename (required)",
            "maxLength", 255
        ));
        properties.put("originalFilename", Map.of(
            "type", "string",
            "description", "Original filename (optional)",
            "maxLength", 255
        ));
        properties.put("width", Map.of(
            "type", "integer",
            "description", "Image width in pixels (optional)"
        ));
        properties.put("height", Map.of(
            "type", "integer",
            "description", "Image height in pixels (optional)"
        ));
        properties.put("filesize", Map.of(
            "type", "integer",
            "description", "File size in bytes (optional)"
        ));
        properties.put("mimeType", Map.of(
            "type", "string",
            "description", "MIME type of the photo (optional)",
            "maxLength", 100
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "filename"));

        return schema;
    }

    /**
     * Creates the schema for photo updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createPhotoUpdateSchema() {
        return createUpdateSchema(createPhotoSchema());
    }

    // ========== Pet Schema Methods ==========

    /**
     * Creates the schema for pet creation.
     * Pets are pet records associated with contacts.
     * Uses root-level POST /pets endpoint with contact_id in request body.
     */
    private Map<String, Object> createPetSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this pet belongs to (required)"
        ));
        properties.put("petCategoryId", Map.of(
            "type", "integer",
            "description", "Pet category ID (required) - defines the type of pet (e.g., dog, cat, bird)"
        ));
        properties.put("name", Map.of(
            "type", "string",
            "description", "Pet's name (optional)",
            "maxLength", 255
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "petCategoryId"));

        return schema;
    }

    /**
     * Creates the schema for pet updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createPetUpdateSchema() {
        return createUpdateSchema(createPetSchema());
    }
}
