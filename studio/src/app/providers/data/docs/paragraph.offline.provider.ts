import {set, get, del} from 'idb-keyval';

import {v4 as uuid} from 'uuid';

import {Paragraph, ParagraphData} from '@deckdeckgo/editor';

import {syncDeleteParagraph, syncUpdateParagraph} from '../../../utils/editor/sync.utils';

export const createOfflineParagraph = ({docId, paragraphData}: {docId: string; paragraphData: ParagraphData}): Promise<Paragraph> => {
  return new Promise<Paragraph>(async (resolve, reject) => {
    try {
      const paragraphId: string = uuid();

      const now: Date = new Date();

      const paragraph: Paragraph = {
        id: paragraphId,
        data: {
          ...paragraphData,
          created_at: now,
          updated_at: now
        }
      };

      await set(`/docs/${docId}/paragraphs/${paragraph.id}`, paragraph);

      await syncUpdateParagraph({docId, paragraphId: paragraph.id});

      resolve(paragraph);
    } catch (err) {
      reject(err);
    }
  });
};

export const getOfflineParagraph = ({docId, paragraphId}: {docId: string; paragraphId: string}): Promise<Paragraph | undefined> =>
  get(`/docs/${docId}/paragraphs/${paragraphId}`);

export const updateOfflineParagraph = ({docId, paragraph}: {docId: string; paragraph: Paragraph}): Promise<Paragraph> => {
  return new Promise<Paragraph>(async (resolve, reject) => {
    try {
      if (!paragraph || !paragraph.data) {
        reject('Invalid paragraph data');
        return;
      }

      paragraph.data.updated_at = new Date();

      await set(`/docs/${docId}/paragraphs/${paragraph.id}`, paragraph);

      await syncUpdateParagraph({docId, paragraphId: paragraph.id});

      resolve(paragraph);
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteOfflineParagraph = ({docId, paragraphId}: {docId: string; paragraphId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await del(`/docs/${docId}/paragraphs/${paragraphId}`);

      await syncDeleteParagraph({docId, paragraphId: paragraphId});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
