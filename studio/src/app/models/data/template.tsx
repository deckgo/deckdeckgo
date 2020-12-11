import firebase from 'firebase/app';

export interface TemplateDataSlot {
  name: string;
  placeholder?: string;
  types?: string[];
}

export interface TemplateDataProp {
  name: string;
  type: string;
  placeholder?: string;
}

export interface TemplateData {
  owner_id: string;

  tag: string;
  cdn?: string;
  slots?: TemplateDataSlot[];
  props?: TemplateDataProp[];

  created_at?: firebase.firestore.Timestamp;
  updated_at?: firebase.firestore.Timestamp;
}

export interface Template {
  id: string;
  data: TemplateData;
}
