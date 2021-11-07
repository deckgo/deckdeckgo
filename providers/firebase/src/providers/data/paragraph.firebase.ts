import {DeleteParagraph, Paragraph, ParagraphData, UpdateParagraph} from '@deckdeckgo/editor';
import {deleteEntry, getEntry, updateEntry} from '../../utils/data/firestore.queries';

export const getParagraph = (docId: string, paragraphId: string): Promise<Paragraph> => {
  return getEntry<ParagraphData>({id: paragraphId, collection: `/docs/${docId}/paragraphs`});
};

export const updateParagraph: UpdateParagraph = (docId: string, paragraph: Paragraph): Promise<Paragraph> => {
  return updateEntry<Paragraph>({entry: paragraph, collection: `/docs/${docId}/paragraphs`});
};

export const deleteParagraph: DeleteParagraph = (docId: string, paragraphId: string): Promise<void> => {
  return deleteEntry({id: paragraphId, collection: `/docs/${docId}/paragraphs`});
};
