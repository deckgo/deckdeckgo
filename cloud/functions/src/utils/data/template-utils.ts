import * as admin from 'firebase-admin';

import {Template} from '../../model/data/template';
import {SlideScope} from '../../model/data/slide';

import * as communityTemplates from '../../assets/templates.json';

export function findTemplates(ownerId: string): Promise<Template[]> {
  return new Promise<Template[]>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.QuerySnapshot = await admin.firestore().collection('templates').where('owner_id', '==', ownerId).get();

      if (!snapshot || !snapshot.docs) {
        resolve([]);
        return;
      }

      const templates: Template[] = snapshot.docs.map((doc) => {
        const id = doc.id;
        const ref = doc.ref;

        return {
          id: id,
          ref: ref,
          data: doc.data(),
        } as Template;
      });

      resolve(templates);
    } catch (err) {
      reject(err);
    }
  });
}

export async function getTemplate(userTemplates: Template[], scope: SlideScope | undefined, template: string): Promise<Template | undefined> {
  if (!scope || scope === SlideScope.DEFAULT) {
    return undefined;
  }

  const templates: Template[] = scope === SlideScope.COMMUNITY ? (communityTemplates.community as Template[]) : userTemplates;

  // TODO: remove
  console.log('SCOPE', scope, templates);

  return templates.find((filteredTemplate: Template) => filteredTemplate.data.tag === template);
}
