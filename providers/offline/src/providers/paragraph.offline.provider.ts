import type {Paragraph, ParagraphData} from '@deckdeckgo/editor';
import {del, get, set} from 'idb-keyval';

export const createOfflineParagraph = ({
  docId,
  paragraphData,
  paragraphId
}: {
  docId: string;
  paragraphData: ParagraphData;
  paragraphId: string;
}): Promise<Paragraph> => {
  return new Promise<Paragraph>(async (resolve, reject) => {
    try {
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

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
