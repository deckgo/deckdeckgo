import {Author, Deck, DeckPublish, Doc, DocPublish, Meta, PublishUrl, UpdateLanding, UserSocial} from '@deckdeckgo/editor';
import {AuthStore} from '../stores/auth.store';
import {DeckStore} from '../stores/deck.store';
import {DocStore} from '../stores/doc.store';
import {EnvStore} from '../stores/env.store';
import {UserStore} from '../stores/user.store';
import {PublishInputs, PublishParams} from '../types/publish.types';
import {cloudProvider} from '../utils/providers.utils';

export const publish = (params: PublishParams): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!EnvStore.getInstance().cloud()) {
        reject('Publish is only available with a compatible cloud provider.');
        return;
      }

      if (
        (!DeckStore.getInstance().get() || !DeckStore.getInstance().get().id || !DeckStore.getInstance().get().data) &&
        (!DocStore.getInstance().get() || !DocStore.getInstance().get().id || !DocStore.getInstance().get().data)
      ) {
        reject('No publish data provided.');
        return;
      }

      if (DocStore.getInstance().get() !== null && DocStore.getInstance().get() !== undefined) {
        await publishDoc(params);

        resolve();
        return;
      }

      await publishDeck(params);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const publishUrl = async (meta: Meta | undefined): Promise<string | undefined> => {
  const {pathname, published} = meta || {};

  if (EnvStore.getInstance().cloud() && published) {
    const {publishUrl}: {publishUrl: PublishUrl} = await cloudProvider<{publishUrl: PublishUrl}>();

    const url: string = await publishUrl();
    return `${url}${pathname}`;
  }

  return undefined;
};

const publishDoc = async ({inputs, config}: PublishParams): Promise<void> => {
  const doc: Doc = updateDocMeta(inputs);

  const {docPublish}: {docPublish: DocPublish} = await cloudProvider<{docPublish: DocPublish}>();

  await docPublish({doc, config});
};

const publishDeck = async ({inputs, config}: PublishParams): Promise<void> => {
  const deck: Deck = updateDeckMeta({inputs, config});

  const {deckPublish}: {deckPublish: DeckPublish} = await cloudProvider<{deckPublish: DeckPublish}>();

  await deckPublish({deck, config});
};

export const updateLanding = async (): Promise<void> => {
  const {updateLanding: updateLandingPage}: {updateLanding: UpdateLanding} = await cloudProvider<{updateLanding: UpdateLanding}>();

  await updateLandingPage();
};

const updateDeckMeta = ({inputs, config}: PublishParams): Deck => {
  const deck: Deck = {...DeckStore.getInstance().get()};

  const {title} = inputs;
  const {github} = config;

  deck.data.name = title;

  deck.data.meta = updateMeta({inputs, meta: deck.data.meta});

  // Update GitHub info (push or not) for GitHub users so next time user publish, the choice is kept
  if (AuthStore.getInstance().get().gitHub) {
    if (deck.data.github) {
      deck.data.github.publish = github === true;
    } else {
      deck.data.github = {
        publish: github === true
      };
    }
  }

  // TODO: FIXME
  if (deck.data.owner_id === undefined && AuthStore.getInstance().isLoggedIn()) {
    deck.data.owner_id = AuthStore.getInstance().get()?.uid;
  }

  return deck;
};

const updateDocMeta = (inputs: PublishInputs): Doc => {
  const doc: Doc = {...DocStore.getInstance().get()};

  const {title} = inputs;
  doc.data.name = title;

  doc.data.meta = updateMeta({inputs, meta: doc.data.meta});

  return doc;
};

const updateMeta = ({inputs, meta}: {inputs: PublishInputs; meta: Meta | undefined}): Meta => {
  const {title, description, tags, canonical, lang} = inputs;

  if (!UserStore.getInstance().get() || !UserStore.getInstance().get().data) {
    throw new Error('No user');
  }

  const now: Date = new Date();

  const updateMeta: Meta = !meta
    ? {
        title,
        updated_at: now as unknown as Date
      }
    : {
        ...meta,
        title,
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

  if (UserStore.getInstance().get()?.data?.name) {
    if (!updateMeta.author) {
      updateMeta.author = {
        name: UserStore.getInstance().get().data.name
      };
    } else {
      (updateMeta.author as Author).name = UserStore.getInstance().get().data.name;
    }

    if (UserStore.getInstance().get().data.bio) {
      (updateMeta.author as Author).bio = UserStore.getInstance().get().data.bio;
    }

    if (UserStore.getInstance().get().data.photo_url) {
      (updateMeta.author as Author).photo_url = UserStore.getInstance().get().data.photo_url;
    }

    if (UserStore.getInstance().get().data.social) {
      (updateMeta.author as Author).social = Object.keys(UserStore.getInstance().get().data.social).reduce(
        (acc: UserSocial, key: string) => {
          acc[key] =
            UserStore.getInstance().get().data.social[key] !== null && UserStore.getInstance().get().data.social[key] !== undefined
              ? UserStore.getInstance().get().data.social[key]
              : null;
          return acc;
        },
        {} as UserSocial
      );
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

  if (lang !== null && lang !== undefined && lang !== '') {
    updateMeta.lang = lang;
  } else {
    updateMeta.lang = null;
  }

  return updateMeta;
};
