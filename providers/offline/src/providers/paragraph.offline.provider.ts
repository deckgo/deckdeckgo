import type {Paragraph, ParagraphData} from '@deckdeckgo/editor';
import {del, get, set, update} from 'idb-keyval';

export const createOfflineParagraph = async ({
  docId,
  paragraphData,
  paragraphId
}: {
  docId: string;
  paragraphData: ParagraphData;
  paragraphId: string;
}): Promise<Paragraph> => {
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

  return paragraph;
};

export const getOfflineParagraph = ({docId, paragraphId}: {docId: string; paragraphId: string}): Promise<Paragraph | undefined> =>
  get(`/docs/${docId}/paragraphs/${paragraphId}`);

export const updateOfflineParagraph = ({docId, paragraph}: {docId: string; paragraph: Paragraph}): Promise<void> =>
  update(`/docs/${docId}/paragraphs/${paragraph.id}`, ({data}: Paragraph) => ({
    id: paragraph.id,
    data: {
      ...data,
      ...paragraph.data
    }
  }));

export const deleteOfflineParagraph = ({docId, paragraphId}: {docId: string; paragraphId: string}): Promise<void> =>
  del(`/docs/${docId}/paragraphs/${paragraphId}`);
