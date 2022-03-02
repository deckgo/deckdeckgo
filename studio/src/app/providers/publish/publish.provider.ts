import {Deck, Doc, Meta, throwError} from '@deckdeckgo/editor';
import {updateOfflineDeck, updateOfflineDoc} from '@deckdeckgo/offline';
import {publish as publishProvider, PublishInputs, publishUrl as publishUrlProvider} from '@deckdeckgo/sync';
import {EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const publish = (inputs: PublishInputs): Promise<void> => {
  const config: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');
  return publishProvider({inputs, config});
};

export const publishUrl = async (meta: Meta | undefined): Promise<string> => {
  const url: string = await publishUrlProvider(meta);

  if (!url) {
    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return deckDeckGoConfig.website;
  }

  return url;
};

export const updatePublishedDocOffline = async (doc: Doc | undefined) => {
  if (!doc) {
    return;
  }

  try {
    await updateOfflineDoc(doc);
  } catch (err) {
    throwError(err);
  }
};

export const updatePublishedDeckOffline = async (deck: Deck | undefined) => {
  if (!deck) {
    return;
  }

  try {
    await updateOfflineDeck(deck);
  } catch (err) {
    throwError(err);
  }
};
