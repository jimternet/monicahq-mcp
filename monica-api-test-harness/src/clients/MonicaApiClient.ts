/**
 * Monica API Client with authentication and circuit breaker patterns
 * 
 * Provides a robust HTTP client for interacting with Monica API with
 * authentication, timeout handling, retry mechanisms, and error handling.
 */

import axios, { AxiosInstance, AxiosRequestConfig, AxiosResponse } from 'axios';

export interface MonicaApiClientConfig {
  readonly baseUrl: string;
  readonly token: string;
  readonly timeout?: number;
  readonly maxRetries?: number;
  readonly retryDelay?: number;
}

export interface ApiResponse<T = any> {
  readonly data: T;
  readonly meta?: Record<string, any>;
  readonly links?: Record<string, any>;
}

export interface ApiError {
  readonly message: string;
  readonly code?: string;
  readonly status: number;
  readonly errors?: Record<string, string[]>;
}

/**
 * Monica API Client implementation
 */
export class MonicaApiClient {
  private readonly axios: AxiosInstance;
  private readonly config: Required<MonicaApiClientConfig>;

  constructor(config: MonicaApiClientConfig) {
    this.config = {
      baseUrl: config.baseUrl.replace(/\/$/, ''), // Remove trailing slash
      token: config.token,
      timeout: config.timeout ?? 30000,
      maxRetries: config.maxRetries ?? 3,
      retryDelay: config.retryDelay ?? 1000
    };

    this.axios = axios.create({
      baseURL: this.config.baseUrl,
      timeout: this.config.timeout,
      headers: {
        'Authorization': `Bearer ${this.config.token}`,
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'User-Agent': 'Monica-API-Test-Harness/1.0.0'
      }
    });

    this.setupInterceptors();
  }

  /**
   * Sets up request and response interceptors
   */
  private setupInterceptors(): void {
    // Request interceptor for logging
    this.axios.interceptors.request.use(
      (config) => {
        console.log(`→ ${config.method?.toUpperCase()} ${config.url}`);
        return config;
      },
      (error) => {
        console.error('Request error:', error);
        return Promise.reject(error);
      }
    );

    // Response interceptor for logging and error handling
    this.axios.interceptors.response.use(
      (response) => {
        console.log(`← ${response.status} ${response.config.method?.toUpperCase()} ${response.config.url} (${response.headers['content-length'] || 'unknown'} bytes)`);
        return response;
      },
      (error) => {
        if (error.response) {
          console.error(`← ${error.response.status} ${error.config?.method?.toUpperCase()} ${error.config?.url}`);
        } else {
          console.error('Network error:', error.message);
        }
        return Promise.reject(this.transformError(error));
      }
    );
  }

  /**
   * Transforms axios errors into standardized ApiError format
   */
  private transformError(error: any): ApiError {
    if (error.response) {
      return {
        message: error.response.data?.message || error.message,
        code: error.response.data?.code,
        status: error.response.status,
        errors: error.response.data?.errors
      };
    }

    if (error.request) {
      return {
        message: 'Network error - no response received',
        status: 0
      };
    }

    return {
      message: error.message || 'Unknown error',
      status: 0
    };
  }

  /**
   * Makes a GET request with retry mechanism
   */
  public async get<T = any>(path: string, config?: AxiosRequestConfig): Promise<ApiResponse<T>> {
    return this.requestWithRetry('GET', path, undefined, config);
  }

  /**
   * Makes a POST request with retry mechanism
   */
  public async post<T = any>(
    path: string, 
    data?: any, 
    config?: AxiosRequestConfig
  ): Promise<ApiResponse<T>> {
    return this.requestWithRetry('POST', path, data, config);
  }

  /**
   * Makes a PUT request with retry mechanism
   */
  public async put<T = any>(
    path: string, 
    data?: any, 
    config?: AxiosRequestConfig
  ): Promise<ApiResponse<T>> {
    return this.requestWithRetry('PUT', path, data, config);
  }

  /**
   * Makes a DELETE request with retry mechanism
   */
  public async delete<T = any>(path: string, config?: AxiosRequestConfig): Promise<ApiResponse<T>> {
    return this.requestWithRetry('DELETE', path, undefined, config);
  }

  /**
   * Makes a request with automatic retry on transient failures
   */
  private async requestWithRetry<T = any>(
    method: string,
    path: string,
    data?: any,
    config?: AxiosRequestConfig,
    attempt: number = 1
  ): Promise<ApiResponse<T>> {
    try {
      const response: AxiosResponse<ApiResponse<T>> = await this.axios.request({
        method,
        url: path,
        data,
        ...config
      });

      return response.data;
    } catch (error: any) {
      // Retry on transient errors (5xx, network errors, timeouts)
      if (this.shouldRetry(error, attempt)) {
        console.log(`Retrying request (attempt ${attempt + 1}/${this.config.maxRetries + 1})...`);
        await this.delay(this.config.retryDelay * attempt);
        return this.requestWithRetry(method, path, data, config, attempt + 1);
      }

      throw error;
    }
  }

  /**
   * Determines if a request should be retried
   */
  private shouldRetry(error: any, attempt: number): boolean {
    if (attempt >= this.config.maxRetries) {
      return false;
    }

    // Retry on server errors (5xx), network errors, or timeouts
    if (!error.response) {
      return true; // Network error
    }

    const status = error.response.status;
    return status >= 500 || status === 429; // Server error or rate limit
  }

  /**
   * Delays execution for the specified number of milliseconds
   */
  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Tests authentication with the Monica API
   */
  public async testAuthentication(): Promise<{ success: boolean; error?: string; user?: any }> {
    try {
      const response = await this.get('/me');
      return {
        success: true,
        user: response.data
      };
    } catch (error: any) {
      return {
        success: false,
        error: error.message || 'Authentication failed'
      };
    }
  }

  /**
   * Tests connectivity to the Monica API
   */
  public async testConnectivity(): Promise<{ success: boolean; error?: string; responseTime: number; apiVersion?: string }> {
    const startTime = Date.now();
    
    try {
      // Try to fetch user info as a connectivity test
      const response = await this.get('/me');
      const responseTime = Date.now() - startTime;
      
      return {
        success: true,
        responseTime,
        apiVersion: response.meta?.version
      };
    } catch (error: any) {
      const responseTime = Date.now() - startTime;
      
      return {
        success: false,
        error: error.message || 'Connection failed',
        responseTime
      };
    }
  }

  /**
   * Gets the current configuration
   */
  public getConfig(): Omit<MonicaApiClientConfig, 'token'> {
    return {
      baseUrl: this.config.baseUrl,
      timeout: this.config.timeout,
      maxRetries: this.config.maxRetries,
      retryDelay: this.config.retryDelay
    };
  }

  /**
   * Updates the authentication token
   */
  public updateToken(newToken: string): void {
    this.axios.defaults.headers.common['Authorization'] = `Bearer ${newToken}`;
  }
}

/**
 * Factory for creating MonicaApiClient instances
 */
export class MonicaApiClientFactory {
  /**
   * Creates a MonicaApiClient from environment variables
   */
  public static fromEnvironment(): MonicaApiClient {
    const baseUrl = process.env.MONICA_API_URL;
    const token = process.env.MONICA_API_TOKEN;

    if (!baseUrl) {
      throw new Error('MONICA_API_URL environment variable is required');
    }

    if (!token) {
      throw new Error('MONICA_API_TOKEN environment variable is required');
    }

    return new MonicaApiClient({
      baseUrl,
      token,
      timeout: process.env.TIMEOUT_SECONDS ? parseInt(process.env.TIMEOUT_SECONDS) * 1000 : undefined,
      maxRetries: process.env.MAX_RETRIES ? parseInt(process.env.MAX_RETRIES) : undefined,
      retryDelay: process.env.RETRY_DELAY_MS ? parseInt(process.env.RETRY_DELAY_MS) : undefined
    });
  }

  /**
   * Creates a MonicaApiClient with custom configuration
   */
  public static create(config: MonicaApiClientConfig): MonicaApiClient {
    return new MonicaApiClient(config);
  }
}

/**
 * Type guards for API responses
 */
export class ApiResponseTypeGuards {
  /**
   * Type guard to check if a response is a valid API response
   */
  public static isApiResponse<T>(obj: any): obj is ApiResponse<T> {
    return obj && typeof obj === 'object' && 'data' in obj;
  }

  /**
   * Type guard to check if an error is an API error
   */
  public static isApiError(obj: any): obj is ApiError {
    return (
      obj &&
      typeof obj === 'object' &&
      typeof obj.message === 'string' &&
      typeof obj.status === 'number'
    );
  }
}