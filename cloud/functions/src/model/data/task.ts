import {firestore} from 'firebase-admin';

export interface TaskData {
  deckId: string;
  token: string | firestore.FieldValue;

  type: 'publish-all' | 'publish-deck' | 'push-github';

  status: 'scheduled' | 'failure' | 'successful';

  created_at: firestore.Timestamp;
  updated_at: firestore.Timestamp;
}

export interface Task {
  id: string;
  ref: firestore.DocumentReference;
  data: TaskData;
}
