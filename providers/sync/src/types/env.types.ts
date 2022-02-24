export interface EnvironmentCloudApi {
  cdn: string;
}

export interface EnvironmentCloudSignIn {
  cdn: string;
  tag: string;
}

export interface EnvironmentCloud {
  api: EnvironmentCloudApi;
  signIn: EnvironmentCloudSignIn;
}
