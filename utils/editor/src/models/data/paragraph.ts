export interface ParagraphData {
  content?: string;

  nodeName: string;

  attributes?: Record<string, string | number | boolean | undefined>;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export interface Paragraph {
  id: string;
  data: ParagraphData;
}
