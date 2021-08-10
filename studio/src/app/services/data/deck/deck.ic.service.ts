import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckAttributes, DeckData, DeckGitHub, DeckGitHubRepo, DeckMeta, DeckMetaAuthor} from '../../../models/data/deck';
import {UserSocial} from '../../../models/data/user';

import {_SERVICE as DecksActor} from '../../../canisters/decks/decks.did';

import {
  _SERVICE as DeckBucketActor,
  Deck as DeckIc,
  DeckGitHub as DeckGitHubIc,
  DeckGitHubRepo as DeckGitHubRepoIc,
  DeckMeta as DeckMetaIc,
  DeckMetaAuthor as DeckMetaAuthorIc,
  UserSocial as UserSocialIc
} from '../../../canisters/deck/deck.did';

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
      data: {
        name: deck.data.name,
        attributes: CanisterUtils.toAttributes(deck.data.attributes),
        background: CanisterUtils.toNullable<string>(deck.data.background),
        header: CanisterUtils.toNullable<string>(deck.data.header),
        footer: CanisterUtils.toNullable<string>(deck.data.footer),
        slides: CanisterUtils.toNullable<string[]>(deck.data.slides),
        meta: CanisterUtils.toNullable<DeckMetaIc>(this.toDeckMeta(deck.data)),
        github: CanisterUtils.toNullable<DeckGitHubIc>(this.toDeckGitHub(deck.data)),
        created_at: CanisterUtils.toNullableTimestamp(deck.data.created_at as Date | undefined),
        updated_at: CanisterUtils.toNullableTimestamp(deck.data.updated_at as Date | undefined)
      }
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

    const promises: Promise<DeckIc>[] = buckets.map((bucket: Principal) => this.getDeckIc({bucket, identity}));

    const decks: DeckIc[] = await Promise.all(promises);

    console.log('Deck IC entries done.', decks);

    return decks?.map((deck: DeckIc) => this.fromDeck({deck, identity}));
  }

  private async getDeckIc({bucket, identity}: {bucket: Principal; identity: Identity}): Promise<DeckIc> {
    const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

    return deckBucket.get();
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

  private fromDeck({deck, identity}: {deck: DeckIc; identity: Identity}): Deck {
    return {
      id: deck.deckId,
      data: {
        name: deck.data.name,
        owner_id: identity.getPrincipal().toText(),
        attributes: CanisterUtils.fromAttributes<DeckAttributes>(deck.data.attributes),
        background: deck.data.background?.[0],
        header: deck.data.header?.[0],
        footer: deck.data.footer?.[0],
        slides: deck.data.slides?.[0],
        meta: this.fromDeckMeta(deck.data.meta),
        github: this.fromDeckGitHub(deck.data.github),
        created_at: CanisterUtils.fromNullableTimestamp(deck.data.created_at),
        updated_at: CanisterUtils.fromNullableTimestamp(deck.data.updated_at)
      }
    };
  }

  private toDeckMeta({meta}: DeckData): DeckMetaIc | undefined {
    if (!meta) {
      return undefined;
    }

    const {title, feed, tags, pathname, description, published, published_at, updated_at, author} = meta;

    const {name: authorName, photo_url} = author as DeckMetaAuthor;

    const metaAuthor: DeckMetaAuthorIc | undefined = author
      ? {
          name: authorName,
          photo_url: CanisterUtils.toNullable<string>(photo_url),
          social: CanisterUtils.toUserSocial<UserSocialIc>((author as DeckMetaAuthor).social as UserSocial)
        }
      : undefined;

    return {
      title,
      feed: CanisterUtils.toNullable<boolean>(feed),
      tags: CanisterUtils.toNullable<string[]>(tags as string[]),
      pathname: CanisterUtils.toNullable<string>(pathname),
      description: CanisterUtils.toNullable<string>(description as string),
      author: CanisterUtils.toNullable<DeckMetaAuthorIc>(metaAuthor),
      published: CanisterUtils.toNullable<boolean>(published),
      published_at: CanisterUtils.toNullableTimestamp(published_at as Date | undefined),
      updated_at: CanisterUtils.toTimestamp(updated_at as Date | undefined)
    };
  }

  private toDeckGitHub({github}: DeckData): DeckGitHubIc | undefined {
    if (!github) {
      return undefined;
    }

    const {repo, publish} = github;

    return {
      repo: CanisterUtils.toNullable<DeckGitHubRepoIc>(
        repo
          ? {
              id: repo.id,
              url: repo.url,
              name: repo.name,
              nameWithOwner: repo.nameWithOwner
            }
          : undefined
      ),
      publish
    };
  }

  private fromDeckGitHub(github: [] | [DeckGitHubIc]): DeckGitHub | undefined {
    if (!github || github.length <= 0) {
      return undefined;
    }

    const repo: DeckGitHubRepoIc | undefined = github[0].repo?.[0];

    const resultRepo: DeckGitHubRepo = Object.keys(repo || {}).reduce((acc: DeckGitHubRepo, key: string) => {
      const value = CanisterUtils.fromValue(github[0].repo[0][key]);
      if (value) {
        acc[key] = value;
      }
      return acc;
    }, {} as DeckGitHubRepo);

    return {
      publish: github[0].publish,
      repo: Object.keys(resultRepo).length ? resultRepo : undefined
    };
  }

  private fromDeckMeta(meta: [] | [DeckMetaIc]): DeckMeta | undefined {
    if (!meta || meta.length <= 0) {
      return undefined;
    }

    const author: DeckMetaAuthor | undefined =
      meta[0].author?.length > 0
        ? {
            name: meta[0].author[0].name,
            photo_url: CanisterUtils.fromNullable<string>(meta[0].author[0].photo_url),
            social: CanisterUtils.fromUserSocial<UserSocialIc>(meta[0].author[0].social)
          }
        : undefined;

    return {
      title: meta[0].title,
      feed: CanisterUtils.fromNullable<boolean>(meta[0].feed),
      tags: CanisterUtils.fromNullable<string[]>(meta[0].tags),
      pathname: CanisterUtils.fromNullable<string>(meta[0].pathname),
      description: CanisterUtils.fromNullable<string>(meta[0].description),
      author,
      published: CanisterUtils.fromNullable<boolean>(meta[0].published),
      published_at: CanisterUtils.fromNullableTimestamp(meta[0].published_at),
      updated_at: CanisterUtils.fromTimestamp(meta[0].updated_at)
    };
  }
}
