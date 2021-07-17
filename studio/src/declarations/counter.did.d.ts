import type { Principal } from '@dfinity/principal';
export interface _SERVICE {
  'getValue' : () => Promise<bigint>,
  'increment' : () => Promise<undefined>,
}