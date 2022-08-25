import type {Meta} from '@deckdeckgo/editor';

export type PublishInputs = Required<Pick<Meta, 'title' | 'description' | 'tags'>> & Pick<Meta, 'canonical' | 'lang'>;

export interface PublishParams {
  inputs: PublishInputs;
  config?: Record<string, string | boolean>;
}
