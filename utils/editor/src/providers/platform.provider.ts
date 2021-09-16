import {Token} from '../models/data/token';

export interface MergeToken {
  (token: Token): Promise<void>;
}
