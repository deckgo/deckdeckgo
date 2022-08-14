import type {DataRecord} from './data';
import type {Deploy} from './deploy';
import type {Meta} from './meta';

export interface DocData {
  name: string;

  owner_id: string | undefined;

  paragraphs?: string[];

  meta?: Meta;

  deploy?: Deploy;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export type Doc = DataRecord<DocData>;
