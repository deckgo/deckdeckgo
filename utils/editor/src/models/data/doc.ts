import {Meta} from './meta';
import {Deploy} from './deploy';

export interface DocData {
  name: string;

  owner_id: string | undefined;

  paragraphs?: string[];

  meta?: Meta;

  deploy?: Deploy;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export interface Doc {
  id: string;
  data: DocData;
}
