/**
 * @file ProcessComposeApiClient.ts
 * @description A TypeScript wrapper for the Process Compose API.
 * This client provides methods to interact with Process Compose to manage and query processes,
 * including their configuration, logs, and overall project state.
 * API documentation: http://localhost:8080/swagger/index.html
 */

/**
 * Interface for the detailed status of a single process.
 * Matches the GET /process/{name} and individual items in GET /processes responses.
 */
export interface ProcessStatus {
  age: number; // The age of the process in seconds.
  cpu: number; // CPU usage percentage.
  exit_code: number; // The exit code of the last run.
  IsRunning: boolean;
  is_elevated: boolean; // True if the process is process running with elevated privileges.
  is_ready: string;
  mem: number; // Memory usage in bytes.
  name: string;
  namespace: string; // The namespace the process belongs to.
  password_provided: boolean;
  pid: number;
  restarts: number; // Number of times the process has restarted.
  status: string;
  system_time: string;
}

/**
 * Interface for the response when getting all processes.
 * Matches the GET /processes response structure.
 */
export interface AllProcessesResponse {
  data: ProcessStatus[]; // An array of ProcessStatus objects.
}

/**
 * Generic API response for actions like start/stop, and for bad requests.
 * The API documentation suggests a generic object like `{ "additionalProp1": "string" }`.
 */
export type GenericApiResponse = Record<string, string>;

/**
 * Defines the structure for the body of the PATCH /processes/stop request.
 */
export type StopProcessesRequestBody = string[];

export interface ProcessProbeExec {
  command: string;
  workingDir: string;
}

export interface ProcessProbeHttpGet {
  host: string;
  numPort: number;
  path: string;
  port: string;
  scheme: string;
}

export interface ProcessProbe {
  exec?: ProcessProbeExec;
  failureThreshold?: number;
  httpGet?: ProcessProbeHttpGet;
  initialDelay?: number;
  periodSeconds?: number;
  successThreshold?: number;
  timeoutSeconds?: number;
}

export interface LoggerRotationConfig {
  compress?: boolean;
  directory?: string;
  filename?: string;
  maxAge?: number; // In days
  maxBackups?: number;
  maxSize?: number; // In megabytes
}

export interface LoggerConfig {
  addTimestamp?: boolean;
  disableJSON?: boolean;
  fieldsOrder?: string[];
  flushEachLine?: boolean;
  noColor?: boolean;
  noMetadata?: boolean;
  rotation?: LoggerRotationConfig;
  timestampFormat?: string;
}

export interface RestartPolicy {
  backoffSeconds?: number;
  exitOnEnd?: boolean;
  exitOnSkipped?: boolean;
  maxRestarts?: number;
  restart?: string;
}

export interface ShutdownParams {
  parentOnly?: boolean;
  shutDownCommand?: string;
  shutDownTimeout?: number; // In seconds
  signal?: number; // Signal number to send
}

export interface ProcessDependency {
  condition: string;
  extensions: Record<string, any>;
}

/**
 * Interface for process configuration.
 * Matches the POST /process and GET /process/info/{name} responses.
 */
export interface ProcessConfig {
  args?: string[];
  command?: string;
  dependsOn?: Record<string, ProcessDependency>;
  Description?: string;
  disableAnsiColors?: boolean;
  disabled?: boolean;
  entrypoint?: string[];
  environment?: string[];
  executable?: string;
  extensions?: Record<string, any>;
  isDaemon?: boolean;
  isElevated?: boolean;
  isForeground?: boolean;
  isTty?: boolean;
  livenessProbe?: ProcessProbe;
  logLocation?: string;
  loggerConfig?: LoggerConfig;
  name: string;
  namespace?: string;
  readinessProbe?: ProcessProbe;
  readyLogLine?: string;
  replicaName?: string;
  replicaNum?: number;
  replicas?: number;
  restartPolicy?: RestartPolicy;
  shutDownParams?: ShutdownParams;
  vars?: Record<string, string>;
  workingDir?: string;
  LaunchTimeout?: number;
  OriginalConfig?: string;
}

/**
 * Interface for process logs response.
 * Matches GET /process/logs/{name}/{endOffset}/{limit} response.
 */
export type ProcessLogs = Record<string, string[]>;

export interface ProjectMemoryState {
  allocated: number;
  gcCycles: number;
  systemMemory: number;
  totalAllocated: number;
}

/**
 * Interface for project state information.
 * Matches GET /project/state response.
 */
export interface ProjectState {
  fileNames: string[];
  hostName: string;
  memoryState: ProjectMemoryState;
  processNum: number;
  runningProcessNum: number;
  startTime: string;
  upTime: number; // Uptime in seconds
  userName: string;
  version: string;
}

/**
 * Interface for the Liveness Check endpoint response (GET /live).
 */
export interface LivenessStatus {
  status: string;
}

/**
 * Utility function to introduce a delay.
 * @param ms The number of milliseconds to wait.
 */
export const delay = (ms: number) =>
  new Promise((resolve) => setTimeout(resolve, ms));

/**
 * Class to interact with the Process Compose API.
 * Provides methods for starting, stopping, and retrieving the status of processes,
 * as well as managing and querying project configurations, logs, and overall project/server state.
 */
export class ProcessComposeApiClient {
  private baseUrl: string;

  /**
   * Constructs a new ProcessComposeApiClient instance.
   * @param baseUrl The base URL of the Process Compose API (e.g., "http://localhost:8080").
   */
  constructor(baseUrl: string) {
    if (!baseUrl) {
      throw new Error("Base URL cannot be empty.");
    }
    // Ensure the base URL does not end with a slash to prevent double slashes in paths.
    this.baseUrl = baseUrl.endsWith("/") ? baseUrl.slice(0, -1) : baseUrl;
  }

  /**
   * Helper method to make HTTP requests.
   * @param method The HTTP method (GET, POST, PATCH).
   * @param path The API endpoint path (e.g., "/process/start/my-process").
   * @param body Optional request body.
   * @returns A Promise resolving to the parsed JSON response.
   * @throws An error if the network request fails or the API returns an error status.
   */
  private async request<T>(
    method: "GET" | "POST" | "PATCH",
    path: string,
    body?: any,
  ): Promise<T> {
    const url = `${this.baseUrl}${path}`;
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
      Accept: "*/*",
      "User-Agent": "ProcessComposeApiClient/1.0",
    };

    const options: RequestInit = {
      method,
      headers,
    };

    if (body !== undefined) {
      // Check for undefined, allowing null or empty objects
      options.body = JSON.stringify(body);
    }
    // For debugging purposes:
    //console.log('Body:', body);
    //console.log('Making request to:', `${this.baseUrl}${path}`);
    //console.log('Headers:', headers);
    //console.log('Fetch options:', options);

    try {
      const response = await fetch(url, options);

      if (!response.ok) {
        let errorData: GenericApiResponse | string =
          `HTTP error! Status: ${response.status}`;
        try {
          // Attempt to parse JSON error response if available
          errorData = await response.json();
        } catch (parseError) {
          // If JSON parsing fails, use the plain text or default error message
          errorData = await response
            .text()
            .catch(() => `Unknown error from ${url}`);
        }
        throw new Error(
          `API Request Failed (${method} ${path}): ${JSON.stringify(errorData)}`,
        );
      }

      // Handle cases where the API might return an empty body for 200 OK
      // For instance, start/stop endpoints return a generic object, but sometimes it might be just status.
      const contentType = response.headers.get("content-type");
      if (contentType && contentType.includes("application/json")) {
        return (await response.json()) as T;
      } else {
        // If no JSON content, return a generic success message or null/undefined
        return {} as T; // Or a more appropriate empty object/value for T
      }
    } catch (error: any) {
      console.error(`Error during API request to ${url}:`, error);
      throw new Error(`Network or client error: ${error.message}`);
    }
  }

  /**
   * Starts a specific process by name.
   * @param name The name of the process to start.
   * @returns A Promise resolving to a GenericApiResponse indicating success or failure.
   */
  public async startProcess(name: string): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("POST", `/process/start/${name}`);
  }

  /**
   * Restarts a specific process by name.
   * @param name The name of the process to start.
   * @returns A Promise resolving to a GenericApiResponse indicating success or failure.
   */
  public async restartProcess(name: string): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("POST", `/process/restart/${name}`);
  }

  /**
   * Stops a specific process by name.
   * @param name The name of the process to stop.
   * @returns A Promise resolving to a GenericApiResponse indicating success or failure.
   */
  public async stopProcess(name: string): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("PATCH", `/process/stop/${name}`);
  }

  public async stopAndAwaitCompletion(
    name: string,
    timeoutMs: number = 30000,
    intervalMs: number = 1000,
  ): Promise<GenericApiResponse> {
    const stopResponse = await this.stopProcess(name);
    await this.waitForServiceStatus(
      name,
      "Completed",
      timeoutMs,
      intervalMs,
      false,
    );
    return stopResponse;
  }

  /**
   * Retrieves the current state of a specific process.
   * @param name The name of the process to retrieve.
   * @returns A Promise resolving to a ProcessStatus object.
   */
  public async getProcessState(name: string): Promise<ProcessStatus> {
    return this.request<ProcessStatus>("GET", `/process/${name}`);
  }

  /**
   * Retrieves the status of all configured processes.
   * @returns A Promise resolving to an array of ProcessStatus objects.
   */
  public async getAllProcesses(): Promise<ProcessStatus[]> {
    const response = await this.request<AllProcessesResponse>(
      "GET",
      "/processes",
    );
    return response.data;
  }

  /**
   * Sends a kill signal to a list of processes.
   * @param names An array of process names to stop.
   * @returns A Promise resolving to a GenericApiResponse indicating success or failure.
   * Note: The API documentation shows both 200 and 207 for success, but the example model is the same.
   * This implementation will resolve with the same GenericApiResponse for both successful statuses.
   */
  public async stopProcesses(
    names: StopProcessesRequestBody,
  ): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("PATCH", "/processes/stop", names);
  }

  /**
   * Updates the configuration of a process.
   * @param config The full configuration object for the process to update.
   * @returns A Promise resolving to a GenericApiResponse indicating success or failure.
   */
  public async updateProcessConfig(
    config: ProcessConfig,
  ): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("POST", "/process", config);
  }

  /**
   * Retrieves the configuration of a specific process.
   * @param name The name of the process whose configuration to retrieve.
   * @returns A Promise resolving to a ProcessConfig object.
   */
  public async getProcessConfig(name: string): Promise<ProcessConfig> {
    return this.request<ProcessConfig>("GET", `/process/info/${name}`);
  }

  /**
   * Retrieves logs for a specific process.
   * This method includes a delay before making the request.
   * @param name The name of the process.
   * @param endOffset Offset from the end of the log (0 for no offset).
   * @param limit Limit of lines to get (0 will get all lines till the end).
   * @returns A Promise resolving to a ProcessLogs object (Record<string, string[]>).
   */
  public async getProcessLogs(
    name: string,
    endOffset: number,
    limit: number,
  ): Promise<ProcessLogs> {
    // It seems that despite the service constantly producing logs, a brief pause before
    // the HTTP request allows Process Compose to correctly prepare and return the log data.
    // When performing many requests - retrieve logs request would result in an empty response.
    // It could be a slight race condition or processing delay.
    // Adding a delay before making the request was found to resolve the issue.
    const delayTime: number = 100;
    console.log(
      `Delaying for ${delayTime}ms before fetching logs for '${name}'...`,
    );
    await delay(delayTime);

    // Revert to using the generic request helper, which was shown to work with the delay.
    return this.request<ProcessLogs>(
      "GET",
      `/process/logs/${encodeURIComponent(name)}/${endOffset}/${limit}`,
    );
  }

  /**
   * Retrieves project state information.
   * @returns A Promise resolving to a ProjectState object.
   */
  public async getProjectState(): Promise<ProjectState> {
    return this.request<ProjectState>("GET", "/project/state");
  }

  /**
   * Stops all processes and shuts down the Process Compose server.
   * Use with caution as this will terminate the server.
   * @returns A Promise resolving to a GenericApiResponse indicating success or failure.
   */
  public async stopProjectAndServer(): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("POST", "/project/stop", {}); // Body can be empty per API docs
  }

  /**
   * Retrieves the hostname where Process Compose is running.
   * @returns A Promise resolving to a GenericApiResponse containing the hostname.
   */
  public async getHostname(): Promise<GenericApiResponse> {
    return this.request<GenericApiResponse>("GET", "/hostname");
  }

  /**
   * Performs a liveness check to determine if the server is responding.
   * @returns A Promise resolving to a LivenessStatus object.
   */
  public async livenessCheck(): Promise<LivenessStatus> {
    return this.request<LivenessStatus>("GET", "/live");
  }

  /**
   * Waits for a Process Compose service to reach a specific status.
   * This method polls the service's state until the expected status is reached or a timeout occurs.
   * @param serviceName The name of the service to check.
   * @param expectedStatus The desired status string (e.g., 'stopped', 'running').
   * @param timeoutMs The maximum time to wait in milliseconds.
   * @param intervalMs The polling interval in milliseconds.
   * @param expectedIsRunning Optional. The expected boolean state of 'IsRunning'.
   * If true, waits for IsRunning to be true.
   * If false, waits for IsRunning to be false.
   * If undefined, 'IsRunning' is not strictly checked.
   * @returns A Promise that resolves if the status is reached, or rejects if timeout occurs.
   */
  public async waitForServiceStatus(
    serviceName: string,
    expectedStatus: string,
    timeoutMs: number,
    intervalMs: number,
    expectedIsRunning?: boolean, // Added new parameter
  ): Promise<void> {
    const startTime = Date.now();
    while (Date.now() - startTime < timeoutMs) {
      try {
        const status: ProcessStatus = await this.getProcessState(serviceName);
        console.log(
          `Polling ${serviceName}: Current status '${status.status}', IsRunning: ${status.IsRunning}`,
        );

        let statusMatchesExpectedString = status.status === expectedStatus;

        // Special handling for 'stopped' status, also considering 'exited' or 'Completed' as stopped
        if (
          expectedStatus === "stopped" &&
          (status.status === "exited" || status.status === "Completed")
        ) {
          statusMatchesExpectedString = true;
        }

        // Check IsRunning if the expectation is provided
        let isRunningMatchesExpected = true; // Assume true if not specified
        if (expectedIsRunning !== undefined) {
          isRunningMatchesExpected = status.IsRunning === expectedIsRunning;
        }

        // The service is considered to have reached the expected state if:
        // 1. The status string matches OR it's a 'stopped' state (exited/completed)
        // AND
        // 2. If expectedIsRunning was provided, the actual IsRunning matches it.
        if (statusMatchesExpectedString && isRunningMatchesExpected) {
          console.log(
            `${serviceName} is now in the expected status: '${status.status}' (IsRunning: ${status.IsRunning}).`,
          );
          return;
        }
      } catch (error: any) {
        // If an error occurs and we expect 'stopped', this might be acceptable.
        // For example, if the service process completely disappears, getProcessState might throw.
        // If the expected status is 'stopped' and the error indicates no such process,
        // it might imply the service is indeed stopped. For now, continue polling.
        console.warn(
          `Error polling ${serviceName} status (expecting '${expectedStatus}', IsRunning: ${expectedIsRunning ?? "any"}): ${error.message}`,
        );
      }
      await delay(intervalMs);
    }
    throw new Error(
      `Timeout waiting for ${serviceName} to reach status '${expectedStatus}' (IsRunning: ${expectedIsRunning ?? "any"})`,
    );
  }
}

