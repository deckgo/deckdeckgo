import {Deck, Author, UserSocial, DeckPublish, PublishUrl, Meta} from '@deckdeckgo/editor';

import editorStore from '../../stores/editor.store';
import userStore from '../../stores/user.store';
import authStore from '../../stores/auth.store';

import {cloud} from '../../utils/core/environment.utils';
import {cloudProvider} from '../../utils/core/providers.utils';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';

export const publish = ({
  name,
  description,
  tags,
  github
}: {
  name: string;
  description: string;
  tags: string[];
  github: boolean;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!editorStore.state.deck || !editorStore.state.deck.id || !editorStore.state.deck.data) {
        reject('No deck found');
        return;
      }

      if (!cloud()) {
        reject('Publish is only available with a compatible cloud provider.');
        return;
      }

      const deck: Deck = updateDeckMeta({name, description, tags, github});

      const publishedDeck: Deck = await publishDeck(deck);

      editorStore.state.deck = {...publishedDeck};

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const publishUrl = async (meta: Meta | undefined): Promise<string> => {
  const {pathname, published} = meta || {};

  if (cloud() && published) {
    const {publishUrl}: {publishUrl: PublishUrl} = await cloudProvider<{publishUrl: PublishUrl}>();

    const url: string = await publishUrl();
    return `${url}${pathname}`;
  }

  const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
  return deckDeckGoConfig.website;
};

const publishDeck = async (deck: Deck): Promise<Deck> => {
  const {deckPublish}: {deckPublish: DeckPublish} = await cloudProvider<{deckPublish: DeckPublish}>();

  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');

  return deckPublish({deck, config: firebaseConfig});
};

const updateDeckMeta = ({name, description, tags, github}: {name: string; description: string; tags: string[]; github: boolean}): Deck => {
  if (!userStore.state.user || !userStore.state.user.data) {
    throw new Error('No user');
  }

  const now: Date = new Date();

  const deck: Deck = {...editorStore.state.deck};

  deck.data.name = name;

  if (!deck.data.meta) {
    deck.data.meta = {
      title: name,
      updated_at: now as unknown as Date
    };
  } else {
    deck.data.meta.title = name;
    deck.data.meta.updated_at = now as unknown as Date;
  }

  if (description && description !== undefined && description !== '') {
    deck.data.meta.description = description;
  } else {
    deck.data.meta.description = null;
  }

  if (!tags || tags.length <= 0) {
    deck.data.meta.tags = null;
  } else {
    deck.data.meta.tags = tags;
  }

  if (userStore.state.user?.data?.name) {
    if (!deck.data.meta.author) {
      deck.data.meta.author = {
        name: userStore.state.user.data.name
      };
    } else {
      (deck.data.meta.author as Author).name = userStore.state.user.data.name;
    }

    if (userStore.state.user.data.bio) {
      (deck.data.meta.author as Author).bio = userStore.state.user.data.bio;
    }

    if (userStore.state.user.data.photo_url) {
      (deck.data.meta.author as Author).photo_url = userStore.state.user.data.photo_url;
    }

    if (userStore.state.user.data.social) {
      (deck.data.meta.author as Author).social = Object.keys(userStore.state.user.data.social).reduce((acc: UserSocial, key: string) => {
        acc[key] =
          userStore.state.user.data.social[key] !== null && userStore.state.user.data.social[key] !== undefined
            ? userStore.state.user.data.social[key]
            : null;
        return acc;
      }, {} as UserSocial);
    } else {
      (deck.data.meta.author as Author).social = null;
    }
  } else if (deck.data.meta.author) {
    deck.data.meta.author = null;
  }

  // Update GitHub info (push or not) for GitHub users so next time user publish, the choice is kept
  if (authStore.state.gitHub) {
    if (deck.data.github) {
      deck.data.github.publish = github;
    } else {
      deck.data.github = {
        publish: github
      };
    }
  }

  return deck;
};
