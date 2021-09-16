import firebase from 'firebase/app';
import 'firebase/firestore';

import {Template, TemplateData} from '@deckdeckgo/editor';

export const getUserTemplates = (userId: string): Promise<Template[]> => {
  return new Promise<Template[]>(async (resolve, reject) => {
    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const snapshot: firebase.firestore.QuerySnapshot = await firestore
        .collection('templates')
        .where('owner_id', '==', userId)
        .orderBy('updated_at', 'desc')
        .get();

      const templates: Template[] = snapshot.docs.map((documentSnapshot: firebase.firestore.QueryDocumentSnapshot) => {
        return {
          id: documentSnapshot.id,
          data: documentSnapshot.data() as TemplateData
        };
      });

      resolve(templates);
    } catch (err) {
      reject(err);
    }
  });
};

export const createTemplate = (templateData: TemplateData): Promise<Template> => {
  return new Promise<Template>(async (resolve, reject) => {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
    templateData.created_at = now as unknown as Date;
    templateData.updated_at = now as unknown as Date;

    firestore
      .collection('templates')
      .add(templateData)
      .then(
        async (doc: firebase.firestore.DocumentReference) => {
          resolve({
            id: doc.id,
            data: templateData
          });
        },
        (err) => {
          reject(err);
        }
      );
  });
};

export const updateTemplate = (template: Template): Promise<Template> => {
  return new Promise<Template>(async (resolve, reject) => {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
    template.data.updated_at = now as unknown as Date;

    try {
      await firestore.collection('templates').doc(template.id).set(template.data, {merge: true});

      resolve(template);
    } catch (err) {
      reject(err);
    }
  });
};
