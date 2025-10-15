/**
 * Unit tests for MonicaApiClient
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import axios from 'axios';
import { MonicaApiClient } from '../../../src/clients/MonicaApiClient.js';

// Mock axios
vi.mock('axios', () => ({
  default: {
    create: vi.fn(() => ({
      interceptors: {
        request: {
          use: vi.fn()
        },
        response: {
          use: vi.fn()
        }
      },
      request: vi.fn()
    }))
  }
}));

describe('MonicaApiClient', () => {
  let client: MonicaApiClient;
  
  beforeEach(() => {
    vi.clearAllMocks();
    client = new MonicaApiClient({
      baseUrl: 'https://api.monica.test',
      token: 'test-token'
    });
  });

  describe('Constructor', () => {
    it('should create client with valid config', () => {
      const config = {
        baseUrl: 'https://api.monica.test',
        token: 'test-token',
        timeout: 10000,
        maxRetries: 3
      };
      
      const client = new MonicaApiClient(config);
      expect(client).toBeInstanceOf(MonicaApiClient);
    });

    it('should throw error for invalid base URL', () => {
      expect(() => new MonicaApiClient({
        baseUrl: '',
        token: 'test-token'
      })).toThrow('Base URL is required');
    });

    it('should handle missing token gracefully', () => {
      expect(() => new MonicaApiClient({
        baseUrl: 'https://api.monica.test'
      })).not.toThrow();
    });
  });

  describe('Authentication Tests', () => {
    it('should test authentication successfully', async () => {
      // Mock successful auth response
      const mockResponse = {
        data: { id: 1, name: 'Test User' },
        status: 200,
        headers: { 'content-type': 'application/json' }
      };

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            get: vi.fn().mockResolvedValue(mockResponse)
          })
        }
      }));

      const result = await client.testAuthentication();
      
      expect(result.success).toBe(true);
      expect(result.user).toBeDefined();
    });

    it('should handle authentication failure', async () => {
      const mockError = new Error('Unauthorized');
      (mockError as any).response = { status: 401 };

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            get: vi.fn().mockRejectedValue(mockError)
          })
        }
      }));

      const result = await client.testAuthentication();
      
      expect(result.success).toBe(false);
      expect(result.error).toContain('Authentication failed');
    });
  });

  describe('Connectivity Tests', () => {
    it('should test connectivity successfully', async () => {
      const mockResponse = {
        data: { version: '4.0.0' },
        status: 200,
        headers: { 'content-type': 'application/json' }
      };

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            get: vi.fn().mockResolvedValue(mockResponse)
          })
        }
      }));

      const startTime = Date.now();
      const result = await client.testConnectivity();
      
      expect(result.success).toBe(true);
      expect(result.responseTime).toBeGreaterThan(0);
      expect(result.apiVersion).toBe('4.0.0');
    });

    it('should handle connectivity failure', async () => {
      const mockError = new Error('Network error');

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            get: vi.fn().mockRejectedValue(mockError)
          })
        }
      }));

      const result = await client.testConnectivity();
      
      expect(result.success).toBe(false);
      expect(result.error).toBe('Network error');
    });
  });

  describe('HTTP Methods', () => {
    it('should perform GET request', async () => {
      const mockResponse = {
        data: [{ id: 1, name: 'Contact 1' }],
        status: 200,
        headers: {}
      };

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            get: vi.fn().mockResolvedValue(mockResponse)
          })
        }
      }));

      const result = await client.get('/contacts');
      
      expect(result.data).toEqual(mockResponse.data);
      expect(result.status).toBe(200);
    });

    it('should perform POST request', async () => {
      const mockResponse = {
        data: { id: 1, first_name: 'John' },
        status: 201,
        headers: {}
      };

      const payload = { first_name: 'John', last_name: 'Doe' };

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            post: vi.fn().mockResolvedValue(mockResponse)
          })
        }
      }));

      const result = await client.post('/contacts', payload);
      
      expect(result.data).toEqual(mockResponse.data);
      expect(result.status).toBe(201);
    });

    it('should handle HTTP errors', async () => {
      const mockError = new Error('Bad Request');
      (mockError as any).response = { 
        status: 400, 
        data: { error: 'Validation failed' } 
      };

      vi.doMock('axios', () => ({
        default: {
          create: () => ({
            get: vi.fn().mockRejectedValue(mockError)
          })
        }
      }));

      await expect(client.get('/invalid')).rejects.toThrow('Bad Request');
    });
  });

  describe('Retry Logic', () => {
    it('should retry on network failure', async () => {
      const mockAxios = {
        get: vi.fn()
          .mockRejectedValueOnce(new Error('Network error'))
          .mockRejectedValueOnce(new Error('Network error'))
          .mockResolvedValueOnce({ data: 'success', status: 200, headers: {} })
      };

      vi.doMock('axios', () => ({
        default: {
          create: () => mockAxios
        }
      }));

      const clientWithRetries = new MonicaApiClient({
        baseUrl: 'https://api.monica.test',
        token: 'test-token',
        maxRetries: 3
      });

      const result = await clientWithRetries.get('/test');
      
      expect(result.data).toBe('success');
      expect(mockAxios.get).toHaveBeenCalledTimes(3);
    });
  });
});