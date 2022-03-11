import * as admin from 'firebase-admin';
import * as communityTemplates from '../../assets/templates.json';
import {SlideScope} from '../../model/data/slide';
import {Template} from '../../model/data/template';

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

  return templates.find((filteredTemplate: Template) => filteredTemplate.data.tag === template);
}
