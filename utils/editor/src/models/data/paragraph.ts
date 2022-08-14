import type {DataRecord} from './data';

export interface ParagraphData {
  children?: string[];

  nodeName: string;

  attributes?: Record<string, string | number | boolean | undefined>;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export type Paragraph = DataRecord<ParagraphData>;
