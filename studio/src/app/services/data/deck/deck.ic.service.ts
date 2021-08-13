import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckData} from '../../../models/data/deck';

import {_SERVICE as DecksActor} from '../../../canisters/decks/decks.did';

import {_SERVICE as DeckBucketActor, Deck as DeckIc} from '../../../canisters/deck/deck.did';

import {CanisterUtils} from '../../../utils/editor/canister.utils';
import {createDeckBucketActor, createDecksActor} from '../../../utils/core/ic.deck.utils';

import {DeckService} from './deck.service';
import {AuthIcService} from '../../auth/auth.ic.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';

export class DeckIcService implements DeckService {
  private static instance: DeckIcService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DeckIcService.instance) {
      DeckIcService.instance = new DeckIcService();
    }
    return DeckIcService.instance;
  }

  async uploadDeck({deck, decksActor, identity}: {deck: Deck; decksActor: DecksActor; identity: Identity}) {
    if (!deck) {
      return;
    }

    console.log('Deck IC about to SET');
    const t0 = performance.now();

    const bucket: Principal = await decksActor.init(deck.id);

    const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

    await deckBucket.set({
      deckId: deck.id,
      data: await CanisterUtils.toArray<DeckData>(deck.data),
      created_at: CanisterUtils.toTimestamp((deck.data.created_at as Date) || new Date()),
      updated_at: CanisterUtils.toTimestamp((deck.data.updated_at as Date) || new Date())
    });

    const t1 = performance.now();
    console.log('Deck IC SET done', t1 - t0);

    const t2 = performance.now();

    // TODO: remove, just for test
    console.log('Deck IC Get:', await deckBucket.get(), performance.now() - t2);
  }

  // @Override
  async entries(_userId: string): Promise<Deck[]> {
    const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

    if (!identity) {
      return [];
    }

    const decksActor: DecksActor = await createDecksActor({identity});

    console.log('Deck IC about to request entries');

    const buckets: Principal[] = await decksActor.entries();

    console.log('Deck IC entries done.', buckets);

    const promises: Promise<DeckIc | undefined>[] = buckets.map((bucket: Principal) => this.getDeckIc({bucket, identity}));

    const decksIc: DeckIc[] = await Promise.all(promises);

    console.log('Deck IC decks done.', decksIc);

    const decksPromises: Promise<Deck>[] = decksIc
      ?.filter((deck: DeckIc | undefined) => deck !== undefined)
      .map((deck: DeckIc) => this.fromDeck({deck, identity}));
    const decks: Deck[] = await Promise.all(decksPromises);

    return decks;
  }

  private async getDeckIc({bucket, identity}: {bucket: Principal; identity: Identity}): Promise<DeckIc | undefined> {
    try {
      const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

      return await deckBucket.get();
    } catch (err) {
      console.error('Deck cannot be found.', bucket);
      return undefined;
    }
  }

  // @Override
  async delete(deckId: string): Promise<void> {
    if (!deckId) {
      return;
    }

    const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

    if (!identity) {
      return;
    }

    const decksActor: DecksActor = await createDecksActor({identity});

    const bucket: Principal = await decksActor.init(deckId);

    const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

    console.log('Deck IC about to delete deck and its slides');

    await deckBucket.del();

    console.log('Deck IC delete');
  }

  private async fromDeck({deck, identity}: {deck: DeckIc; identity: Identity}): Promise<Deck> {
    const data: DeckData = await CanisterUtils.fromArray<DeckData>(deck.data);

    return {
      id: deck.deckId,
      data: {
        ...data,
        owner_id: identity.getPrincipal().toText(),
        created_at: CanisterUtils.fromTimestamp(deck.created_at),
        updated_at: CanisterUtils.fromTimestamp(deck.updated_at)
      }
    };
  }
}
