import firebase from 'firebase/app';
import 'firebase/firestore';

import {Template, TemplateData} from '@deckdeckgo/editor';

import templatesStore from '../../../stores/templates.store';
import authStore from '../../../stores/auth.store';

import {firebase as firebaseEnabled} from '../../../utils/core/environment.utils';

export class TemplateProvider {
  private static instance: TemplateProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!TemplateProvider.instance) {
      TemplateProvider.instance = new TemplateProvider();
    }
    return TemplateProvider.instance;
  }

  async init() {
    if (!authStore.state.authUser || authStore.state.authUser.anonymous) {
      return;
    }

    if (templatesStore.state.user?.length > 0) {
      return;
    }

    if (!firebaseEnabled()) {
      return;
    }

    try {
      const templates: Template[] = await this.getUserTemplates();

      if (!templates) {
        return undefined;
      }

      templatesStore.state.user = [...templates];
    } catch (err) {
      console.error(err);
    }
  }

  private getUserTemplates(): Promise<Template[]> {
    return new Promise<Template[]>(async (resolve, reject) => {
      try {
        const firestore: firebase.firestore.Firestore = firebase.firestore();

        const userId: string = authStore.state.authUser.uid;

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
  }

  create(templateData: TemplateData): Promise<Template> {
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
  }

  update(template: Template): Promise<Template> {
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
  }
}
