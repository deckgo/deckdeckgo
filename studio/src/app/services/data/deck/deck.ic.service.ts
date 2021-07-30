import {Identity} from '@dfinity/agent';

import {Deck, DeckData, DeckMetaAuthor} from '../../../models/data/deck';

import {UserSocial} from '../../../models/data/user';

import {idlFactory as DeckFactory} from '../../../functions/decks/decks.utils.did';
import {
  _SERVICE as DeckActor,
  DeckGitHub,
  DeckGitHubRepo,
  DeckMeta,
  DeckMetaAuthor as ActorDeckMetaAuthor,
  UserSocial as ActorUserSocial
} from '../../../functions/decks/decks.did';

import {CanisterUtils} from '../../../utils/editor/canister.utils';
import {createActor} from '../../../utils/core/ic.utils';

export class DeckIcService {
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

  createActor({identity}: {identity: Identity}): Promise<DeckActor> {
    return createActor<DeckActor>({canisterId: process.env.DECKS_CANISTER_ID, idlFactory: DeckFactory, identity});
  }

  async uploadDeck({deck, deckActor}: {deck: Deck; deckActor: DeckActor}) {
    if (!deck) {
      return;
    }

    await deckActor.set({
      deckId: deck.id,
      data: {
        name: deck.data.name,
        attributes: CanisterUtils.prepareAttributes(deck.data.attributes),
        background: CanisterUtils.toNullable<string>(deck.data.background),
        header: CanisterUtils.toNullable<string>(deck.data.header),
        footer: CanisterUtils.toNullable<string>(deck.data.footer),
        slides: CanisterUtils.toNullable<string[]>(deck.data.slides),
        meta: CanisterUtils.toNullable<DeckMeta>(this.convertDeckMeta(deck.data)),
        github: CanisterUtils.toNullable<DeckGitHub>(this.convertDeckGitHub(deck.data)),
        created_at: CanisterUtils.toNullableTimestamp(deck.data.created_at as Date | undefined),
        updated_at: CanisterUtils.toNullableTimestamp(deck.data.updated_at as Date | undefined)
      }
    });

    // TODO: remove, just for test
    console.log('Deck IC Get:', await deckActor.get(deck.id));
  }

  private convertDeckMeta({meta}: DeckData): DeckMeta | undefined {
    if (!meta) {
      return undefined;
    }

    const {title, feed, tags, pathname, description, published, published_at, updated_at, author} = meta;

    const {name: authorName, photo_url} = author as DeckMetaAuthor;

    const metaAuthor: ActorDeckMetaAuthor | undefined = author
      ? {
          name: authorName,
          photo_url: CanisterUtils.toNullable<string>(photo_url),
          social: CanisterUtils.toNullable<ActorUserSocial>(this.convertUserSocial(author as DeckMetaAuthor))
        }
      : undefined;

    return {
      title,
      feed: CanisterUtils.toNullable<boolean>(feed),
      tags: CanisterUtils.toNullable<string[]>(tags as string[]),
      pathname: CanisterUtils.toNullable<string>(pathname),
      description: CanisterUtils.toNullable<string>(description as string),
      author: CanisterUtils.toNullable<ActorDeckMetaAuthor>(metaAuthor),
      published: CanisterUtils.toNullable<boolean>(published),
      published_at: CanisterUtils.toNullableTimestamp(published_at as Date | undefined),
      updated_at: CanisterUtils.toTimestamp(updated_at as Date | undefined)
    };
  }

  private convertUserSocial({social}: DeckMetaAuthor): ActorUserSocial | undefined {
    if (!social) {
      return undefined;
    }

    const {dev, linkedin, twitter, custom_logo_url, custom, github, medium} = social as UserSocial;

    return {
      dev: CanisterUtils.toNullable<string>(dev),
      linkedin: CanisterUtils.toNullable<string>(linkedin),
      twitter: CanisterUtils.toNullable<string>(twitter),
      custom_logo_url: CanisterUtils.toNullable<string>(custom_logo_url),
      custom: CanisterUtils.toNullable<string>(custom),
      github: CanisterUtils.toNullable<string>(github),
      medium: CanisterUtils.toNullable<string>(medium)
    };
  }

  private convertDeckGitHub({github}: DeckData): DeckGitHub | undefined {
    if (!github) {
      return undefined;
    }

    const {repo, publish} = github;

    return {
      repo: CanisterUtils.toNullable<DeckGitHubRepo>(
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
}
