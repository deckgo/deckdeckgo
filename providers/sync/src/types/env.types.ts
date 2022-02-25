export interface EnvironmentCdn {
  cdn: string;
}

export interface EnvironmentComponent extends EnvironmentCdn {
  tag: string;
}

export interface EnvironmentCloud {
  api: EnvironmentCdn;
  signIn: EnvironmentComponent;
}

export interface Environment {
  cloud: EnvironmentCloud;
  jszip: EnvironmentCdn;
}
