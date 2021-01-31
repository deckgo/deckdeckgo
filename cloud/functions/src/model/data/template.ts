import {firestore} from 'firebase-admin';

export interface TemplateDataSlot {
  name: string;
  placeholder?: string;
  types?: string[];
}

export interface TemplateDataProp {
  name: string;
  type: 'string' | 'number' | 'boolean';
  placeholder?: string;
}

export interface TemplateDataAuthor {
  name: string;
  url?: string;
}

export interface TemplateData {
  owner_id: string;

  tag: string;
  cdn?: string;
  author?: TemplateDataAuthor;
  slots?: TemplateDataSlot[];
  props?: TemplateDataProp[];

  created_at?: firestore.Timestamp;
  updated_at?: firestore.Timestamp;
}

export interface Template {
  id: string;
  ref: firestore.DocumentReference;
  data: TemplateData;
}
