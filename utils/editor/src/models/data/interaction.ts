import type {DataRecord} from './data';

export interface LikeData {
  like: boolean;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export type Interaction = DataRecord<LikeData> & {author_id: string};
