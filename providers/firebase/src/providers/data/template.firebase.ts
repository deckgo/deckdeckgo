import firebase from 'firebase/app';
import 'firebase/firestore';

import {Template, TemplateData, GetUserTemplates, CreateTemplate, UpdateTemplate} from '@deckdeckgo/editor';

export const getUserTemplates: GetUserTemplates = (userId: string): Promise<Template[]> => {
  return new Promise<Template[]>(async (resolve, reject) => {
    try {
      if (!userId) {
        resolve([]);
        return;
      }

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

export const createTemplate: CreateTemplate = (templateData: TemplateData): Promise<Template> => {
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

export const updateTemplate: UpdateTemplate = (template: Template): Promise<Template> => {
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
