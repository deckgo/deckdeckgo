import {Author, Deck, DeckPublish, Doc, DocPublish, Meta, PublishUrl, throwError, UserSocial} from '@deckdeckgo/editor';
import {set} from 'idb-keyval';
import {EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import authStore from '../../stores/auth.store';
import editorStore from '../../stores/editor.store';
import userStore from '../../stores/user.store';
import {cloud} from '../../utils/core/environment.utils';
import {cloudProvider} from '../../utils/core/providers.utils';

interface PublishInputs {
  name: string;
  description: string;
  tags: string[];
  github: boolean;
  canonical: string | undefined;
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

  await docPublish({doc});
};

const publishDeck = async (inputs: PublishInputs): Promise<void> => {
  const deck: Deck = updateDeckMeta(inputs);

  const {deckPublish}: {deckPublish: DeckPublish} = await cloudProvider<{deckPublish: DeckPublish}>();

  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');

  await deckPublish({deck, config: firebaseConfig});
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

  // TODO: FIXME
  if (deck.data.owner_id === undefined && authStore.state.loggedIn) {
    deck.data.owner_id = authStore.state.authUser?.uid;
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
  const {name, description, tags, canonical} = inputs;

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

  if (canonical) {
    updateMeta.canonical = canonical;
  } else {
    updateMeta.canonical = null;
  }

  return updateMeta;
};

export const updatePublishedDocOffline = async (doc: Doc | undefined) => {
  if (!doc) {
    return;
  }

  try {
    await set(`/docs/${doc.id}`, doc);
  } catch (err) {
    throwError(err);
  }
};

export const updatePublishedDeckOffline = async (deck: Deck | undefined) => {
  if (!deck) {
    return;
  }

  try {
    await set(`/decks/${deck.id}`, deck);
  } catch (err) {
    throwError(err);
  }
};
