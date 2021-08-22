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

    // We delete the all canister of the related deckId

    const decksActor: DecksActor = await createDecksActor({identity});

    console.log('Deck IC about to delete deck and its slides');

    await decksActor.del(deckId);

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
