export interface PublishInputs {
  name: string;
  description: string;
  tags: string[];
  github: boolean;
  canonical: string | undefined;
}

export interface PublishParams {
  inputs: PublishInputs;
  config?: Record<string, string>;
}
