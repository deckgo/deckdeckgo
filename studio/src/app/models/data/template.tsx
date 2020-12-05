import firebase from 'firebase/app';

export interface TemplateData {
  owner_id: string;

  cdn?: string;

  created_at?: firebase.firestore.Timestamp;
  updated_at?: firebase.firestore.Timestamp;
}

export interface Template {
  id: string;
  data: TemplateData;
}
