import {set, get, del} from 'idb-keyval';

import {v4 as uuid} from 'uuid';

import {Section, SectionData} from '@deckdeckgo/editor';

export const createOfflineSection = ({docId, sectionData}: {docId: string; sectionData: SectionData}): Promise<Section> => {
  return new Promise<Section>(async (resolve, reject) => {
    try {
      const sectionId: string = uuid();

      const now: Date = new Date();

      const section: Section = {
        id: sectionId,
        data: {
          ...sectionData,
          created_at: now,
          updated_at: now
        }
      };

      await set(`/docs/${docId}/sections/${section.id}`, section);

      // TODO: sync
      // await syncUpdateSlide({docId, slideId: slide.id});

      resolve(section);
    } catch (err) {
      reject(err);
    }
  });
};

export const getOfflineSection = ({docId, sectionId}: {docId: string; sectionId: string}): Promise<Section> =>
  get(`/docs/${docId}/sections/${sectionId}`);

export const updateOfflineSection = ({docId, section}: {docId: string; section: Section}): Promise<Section> => {
  return new Promise<Section>(async (resolve, reject) => {
    try {
      if (!section || !section.data) {
        reject('Invalid sectoin data');
        return;
      }

      section.data.updated_at = new Date();

      await set(`/docs/${docId}/sections/${section.id}`, section);

      // TODO sync
      // await syncUpdateSlide({docId, slideId: slide.id});

      resolve(section);
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteOfflineSection = ({docId, sectionId}: {docId: string; sectionId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await del(`/docs/${docId}/sections/${sectionId}`);

      // TOD sync
      // await syncDeleteSlide({deckId, slideId});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
