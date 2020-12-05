import firebase from 'firebase/app';
import 'firebase/firestore';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import templatesStore from '../../../stores/templates.store';

import {Template, TemplateData} from '../../../models/data/template';

export class TemplateService {
  private static instance: TemplateService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!TemplateService.instance) {
      TemplateService.instance = new TemplateService();
    }
    return TemplateService.instance;
  }

  async init() {
    if (templatesStore.state.community?.length > 0) {
      return;
    }

    try {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      const res: Response = await fetch(`${config.globalAssetsUrl}/templates.json`);

      if (!res) {
        return undefined;
      }

      // templatesStore.state.community = await res.json();
    } catch (err) {
      console.error(err);
    }
  }

  getUserTemplates(userId: string): Promise<Template[]> {
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
            data: documentSnapshot.data() as TemplateData,
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
      templateData.created_at = now;
      templateData.updated_at = now;

      firestore
        .collection('templates')
        .add(templateData)
        .then(
          async (doc: firebase.firestore.DocumentReference) => {
            resolve({
              id: doc.id,
              data: templateData,
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
      template.data.updated_at = now;

      try {
        await firestore.collection('templates').doc(template.id).set(template.data, {merge: true});

        resolve(template);
      } catch (err) {
        reject(err);
      }
    });
  }
}
