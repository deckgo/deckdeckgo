import {Deck, Author, UserSocial, DeckPublish, PublishUrl, Meta, Doc, DocPublish} from '@deckdeckgo/editor';

import editorStore from '../../stores/editor.store';
import userStore from '../../stores/user.store';
import authStore from '../../stores/auth.store';

import {cloud} from '../../utils/core/environment.utils';
import {cloudProvider} from '../../utils/core/providers.utils';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';

interface PublishInputs {
  name: string;
  description: string;
  tags: string[];
  github: boolean;
}

export const publish = (inputs: PublishInputs): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!cloud()) {
        reject('Publish is only available with a compatible cloud provider.');
        return;
      }

      if (
        (!editorStore.state.deck || !editorStore.state.deck.id || !editorStore.state.deck.data) &&
        (!editorStore.state.doc || !editorStore.state.doc.id || !editorStore.state.doc.data)
      ) {
        reject('No publish data provided.');
        return;
      }

      if (editorStore.state.doc !== null) {
        await publishDoc(inputs);
        return;
      }

      await publishDeck(inputs);

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

const publishDoc = async (inputs: PublishInputs): Promise<void> => {
  const doc: Doc = updateDocMeta(inputs);

  const {docPublish}: {docPublish: DocPublish} = await cloudProvider<{docPublish: DocPublish}>();

  const publishedDoc: Doc = await docPublish({doc});

  editorStore.state.doc = {...publishedDoc};
};

const publishDeck = async (inputs: PublishInputs): Promise<void> => {
  const deck: Deck = updateDeckMeta(inputs);

  const {deckPublish}: {deckPublish: DeckPublish} = await cloudProvider<{deckPublish: DeckPublish}>();

  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');

  const publishedDeck: Deck = await deckPublish({deck, config: firebaseConfig});

  editorStore.state.deck = {...publishedDeck};
};

const updateDeckMeta = (inputs: PublishInputs): Deck => {
  const deck: Deck = {...editorStore.state.deck};

  const {name, github} = inputs;

  deck.data.name = name;

  deck.data.meta = updateMeta({inputs, meta: deck.data.meta});

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

const updateDocMeta = (inputs: PublishInputs): Doc => {
  const doc: Doc = {...editorStore.state.doc};

  const {name} = inputs;
  doc.data.name = name;

  doc.data.meta = updateMeta({inputs, meta: doc.data.meta});

  return doc;
};

const updateMeta = ({inputs, meta}: {inputs: PublishInputs; meta: Meta | undefined}): Meta => {
  const {name, description, tags} = inputs;

  if (!userStore.state.user || !userStore.state.user.data) {
    throw new Error('No user');
  }

  const now: Date = new Date();

  const updateMeta: Meta = !meta
    ? {
        title: name,
        updated_at: now as unknown as Date
      }
    : {
        ...meta,
        title: name,
        updated_at: now as unknown as Date
      };

  if (description && description !== undefined && description !== '') {
    updateMeta.description = description;
  } else {
    updateMeta.description = null;
  }

  if (!tags || tags.length <= 0) {
    updateMeta.tags = null;
  } else {
    updateMeta.tags = tags;
  }

  if (userStore.state.user?.data?.name) {
    if (!updateMeta.author) {
      updateMeta.author = {
        name: userStore.state.user.data.name
      };
    } else {
      (updateMeta.author as Author).name = userStore.state.user.data.name;
    }

    if (userStore.state.user.data.bio) {
      (updateMeta.author as Author).bio = userStore.state.user.data.bio;
    }

    if (userStore.state.user.data.photo_url) {
      (updateMeta.author as Author).photo_url = userStore.state.user.data.photo_url;
    }

    if (userStore.state.user.data.social) {
      (updateMeta.author as Author).social = Object.keys(userStore.state.user.data.social).reduce((acc: UserSocial, key: string) => {
        acc[key] =
          userStore.state.user.data.social[key] !== null && userStore.state.user.data.social[key] !== undefined
            ? userStore.state.user.data.social[key]
            : null;
        return acc;
      }, {} as UserSocial);
    } else {
      (updateMeta.author as Author).social = null;
    }
  } else if (updateMeta.author) {
    updateMeta.author = null;
  }

  return updateMeta;
};
