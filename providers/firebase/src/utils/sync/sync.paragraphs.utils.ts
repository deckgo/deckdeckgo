import firebase from 'firebase/app';

import {get} from 'idb-keyval';

import {SyncDataParagraph, docSelector, Paragraph} from '@deckdeckgo/editor';

import {uploadLocalImages} from './sync.storage.utils';
import {deleteParagraph, updateParagraph} from '../../providers/data/paragraph.firebase';

export const uploadParagraphs = ({data, userId}: {data: SyncDataParagraph[] | undefined; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    if (!data || data.length <= 0) {
      resolve();
      return;
    }

    try {
      const promises: Promise<void>[] = data.map(({docId, paragraphId}: SyncDataParagraph) =>
        uploadParagraph({docId, paragraphId, userId})
      );

      await Promise.all(promises);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadParagraph = async ({docId, paragraphId, userId}: {docId: string; paragraphId: string; userId: string}): Promise<void> => {
  await uploadParagraphLocalUserAssets({docId, paragraphId, userId});
  await uploadParagraphData(docId, paragraphId);
};

const uploadParagraphLocalUserAssets = ({
  docId,
  paragraphId,
  userId
}: {
  docId: string;
  paragraphId: string;
  userId: string;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    const paragraphElement: HTMLElement = document.querySelector(`${docSelector} > article *[paragraph_id="${paragraphId}"]`);

    if (!paragraphElement) {
      resolve();
      return;
    }

    try {
      const updateContent = ({data: paragraph, imgSrc, downloadUrl}: {data: Paragraph; imgSrc: string; downloadUrl: string}): Paragraph => {
        if (!paragraph.data.children) {
          return paragraph;
        }

        return {
          id: paragraph.id,
          data: {
            ...paragraph.data,
            children: paragraph.data.children.map((child: string) => {
              child = child.replace(`img-src="${imgSrc}"`, `img-src="${downloadUrl}"`);
              child = child.replace(`img-alt="${imgSrc}"`, `img-alt="${downloadUrl}"`);
              return child;
            })
          }
        };
      };

      await uploadLocalImages({element: paragraphElement, key: `/docs/${docId}/paragraphs/${paragraphId}`, userId, updateContent});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadParagraphData = (docId: string, paragraphId: string): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const paragraph: Paragraph = await get(`/docs/${docId}/paragraphs/${paragraphId}`);

      if (!paragraph || !paragraph.data) {
        // If upload process end up in error in a previous try, some paragraphs might have already been uploaded correctly and remove from the local db
        resolve();
        return;
      }

      if (paragraph.data.children === null || paragraph.data.children.length <= 0) {
        // @ts-ignore
        paragraph.data.children = firebase.firestore.FieldValue.delete();
      }

      await updateParagraph(docId, paragraph);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteParagraphs = async (data: SyncDataParagraph[] | undefined): Promise<void> => {
  if (!data || data.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = data.map(({docId, paragraphId}: SyncDataParagraph) => deleteParagraph(docId, paragraphId));
  await Promise.all(promises);
};
