import {StyloConfig} from '@papyrs/stylo';

export const editorConfig: Partial<StyloConfig> = {
  excludeAttributes: ['id', 'hydrated', 'editable', 'paragraph_id', 'highlighted', 'custom-loader']
};
