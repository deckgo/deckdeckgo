import firebase from 'firebase/app';

export interface TemplateDataSlot {
  name: string;
  placeholder?: string;
  types?: string[];
}

export interface TemplateData {
  owner_id: string;

  cdn?: string;
  tag?: string;
  slots?: TemplateDataSlot[];

  created_at?: firebase.firestore.Timestamp;
  updated_at?: firebase.firestore.Timestamp;
}

export interface Template {
  id: string;
  data: TemplateData;
}
