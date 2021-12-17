export interface DeployData {
  status: 'scheduled' | 'failure' | 'successful';
  updated_at: Date | number | BigInt;
}

export interface Deploy {
  github?: DeployData;
  api?: DeployData;
}
