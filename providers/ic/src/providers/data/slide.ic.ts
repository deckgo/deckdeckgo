import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor, Data} from '../../canisters/data/data.did';

import {getIdentity} from '../auth/auth.ic';

import {fromArray, fromTimestamp} from '../../utils/did.utils';
import {getDataBucket} from '../../utils/manager.utils';

export const getSlide = (deckId: string, slideId: string): Promise<Slide> => {
  return new Promise<Slide>(async (resolve, reject) => {
    const identity: Identity | undefined = getIdentity();

    if (!identity) {
      reject('No internet identity.');
      return;
    }

    try {
      const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({identity});

      const slide: Data = await actor.get(`/decks/${deckId}/slides/${slideId}`);

      const data: SlideData = await fromArray<SlideData>(slide.data);

      resolve({
        id: slideId,
        data: {
          ...data,
          created_at: fromTimestamp(slide.created_at),
          updated_at: fromTimestamp(slide.updated_at)
        }
      });
    } catch (err) {
      reject(err);
    }
  });
};
