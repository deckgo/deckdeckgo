import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';
import {_SERVICE as DeckBucketActor, Slide as SlideIc} from '../../canisters/deck/deck.did';

import {getIdentity} from '../auth/auth.ic';
import {createDeckBucketActor, createManagerActor, initDeckBucket} from '../../utils/manager.utils';
import {fromArray, fromTimestamp} from '../../utils/did.utils';

export const getSlide = (deckId: string, slideId: string): Promise<Slide> => {
  return new Promise<Slide>(async (resolve, reject) => {
    const identity: Identity | undefined = getIdentity();

    if (!identity) {
      reject('No internet identity.');
      return;
    }

    try {
      const managerActor: ManagerActor = await createManagerActor({identity});

      const bucket: Principal = await initDeckBucket({managerActor, deckId});

      const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

      const slide: SlideIc = await deckBucket.getSlide(slideId);

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
