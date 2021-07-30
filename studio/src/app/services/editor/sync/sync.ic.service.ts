import {Identity} from '@dfinity/agent';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {UserSocial} from '../../../models/data/user';
import {Deck, DeckData, DeckMetaAuthor} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {idlFactory as DeckFactory} from '../../../functions/decks/decks.utils.did';
import {idlFactory as SlideFactory} from '../../../functions/slides/slides.utils.did';
import {
  _SERVICE as DeckActor,
  DeckGitHub,
  DeckGitHubRepo,
  DeckMeta,
  DeckMetaAuthor as ActorDeckMetaAuthor,
  UserSocial as ActorUserSocial
} from '../../../functions/decks/decks.did';
import {_SERVICE as SlideActor} from '../../../functions/slides/slides.did';

import {internetComputer} from '../../../utils/core/environment.utils';
import {createActor} from '../../../utils/core/ic.utils';
import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {SyncService} from './sync.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

// TODO: can we move this in a web worker? the IC SDK is compatible?

export class SyncIcService extends SyncService {
  // @Override
  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !navigator.onLine) {
        return;
      }

      // TODO: fix me. does not work if no changes are made after sign in or coming back

      if (!this.isSyncPending()) {
        return;
      }

      if (!internetComputer()) {
        return;
      }

      const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

      if (!identity) {
        return;
      }

      syncStore.state.sync = 'in_progress';

      const {updateDecks, updateSlides, deleteSlides} = syncData;

      await this.uploadDecks({updateDecks, identity});

      await this.uploadSlides({updateSlides, identity});

      await this.deleteSlides({deleteSlides, identity});

      // TODO: handle delete decks here?

      await this.clean(syncData);
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
  }

  private async uploadDecks({updateDecks, identity}: {updateDecks: SyncDataDeck[] | undefined; identity: Identity}) {
    if (!updateDecks || updateDecks.length <= 0) {
      return;
    }

    const deckActor: DeckActor = await this.createDeckActor({identity});

    const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => this.uploadDeck({deck, deckActor}));
    await Promise.all(promises);
  }

  private async uploadDeck({deck, deckActor}: {deck: Deck; deckActor: DeckActor}) {
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

  private async uploadSlides({updateSlides, identity}: {updateSlides: SyncDataSlide[] | undefined; identity: Identity}) {
    if (!updateSlides || updateSlides.length <= 0) {
      return;
    }

    const slideActor: SlideActor = await this.createSlideActor({identity});

    const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) => this.uploadSlide({slide, deckId, slideActor}));
    await Promise.all(promises);
  }

  private async uploadSlide({slide, deckId, slideActor}: {slide: Slide; deckId: string; slideActor: SlideActor}) {
    if (!slide) {
      return;
    }

    await slideActor.set({
      slideId: slide.id,
      deckId,
      data: {
        content: CanisterUtils.toNullable<string>(slide.data.content),
        template: slide.data.template,
        scope: CanisterUtils.toNullable<string>(slide.data.scope),
        attributes: CanisterUtils.prepareAttributes(slide.data.attributes),
        created_at: CanisterUtils.toNullableTimestamp(slide.data.created_at as Date | undefined),
        updated_at: CanisterUtils.toNullableTimestamp(slide.data.updated_at as Date | undefined)
      }
    });

    // TODO: remove, just for test
    console.log('Slide IC Get:', await slideActor.get(slide.id));
  }

  private async deleteSlides({deleteSlides, identity}: {deleteSlides: SyncDataSlide[] | undefined; identity: Identity}) {
    if (!deleteSlides || deleteSlides.length <= 0) {
      return;
    }

    const slideActor: SlideActor = await this.createSlideActor({identity});

    const promises: Promise<void>[] = deleteSlides.map(({slideId}: SyncDataSlide) => this.deleteSlide({slideId, slideActor}));
    await Promise.all(promises);
  }

  private async deleteSlide({slideId, slideActor}: {slideId: string; slideActor: SlideActor}) {
    if (!slideId) {
      return;
    }

    await slideActor.del(slideId);
  }

  private createDeckActor({identity}: {identity: Identity}): Promise<DeckActor> {
    return createActor<DeckActor>({canisterId: process.env.DECKS_CANISTER_ID, idlFactory: DeckFactory, identity});
  }

  private createSlideActor({identity}: {identity: Identity}): Promise<SlideActor> {
    return createActor<SlideActor>({canisterId: process.env.SLIDES_CANISTER_ID, idlFactory: SlideFactory, identity});
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
