export interface SectionData {
  content?: string;

  nodeName: string;

  attributes?: Record<string, string | number | boolean | undefined>;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export interface Section {
  id: string;
  data: SectionData;
}
