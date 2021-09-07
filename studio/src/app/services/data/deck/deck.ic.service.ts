import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckData} from '../../../models/data/deck';

import {_SERVICE as ManagerActor} from '../../../canisters/manager/manager.did';

import {_SERVICE as DeckBucketActor, Deck as DeckIc} from '../../../canisters/deck/deck.did';

import {CanisterUtils} from '../../../utils/editor/canister.utils';
import {createDeckBucketActor, createManagerActor} from '../../../utils/core/ic.deck.utils';

import {DeckService} from './deck.service';

import {AuthIcProvider} from '../../../providers/auth/auth.ic.provider';
import {AuthFactoryProvider} from '../../../providers/auth/auth.factory.provider';

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
    const identity: Identity | undefined = (AuthFactoryProvider.getInstance() as AuthIcProvider).getIdentity();

    if (!identity) {
      return [];
    }

    const managerActor: ManagerActor = await createManagerActor({identity});

    console.log('Deck IC about to request entries');

    const buckets: Principal[] = await managerActor.entries();

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

    const identity: Identity | undefined = (AuthFactoryProvider.getInstance() as AuthIcProvider).getIdentity();

    if (!identity) {
      return;
    }

    // We delete the all canister of the related deckId

    const managerActor: ManagerActor = await createManagerActor({identity});

    console.log('Deck IC about to delete deck and its slides');

    await managerActor.del(deckId);

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
