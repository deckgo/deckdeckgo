export interface EnvironmentIC extends Record<string, string | boolean> {
  managerCanisterId: string;
  localIdentityCanisterId?: string;
  kitPath: string;
}
