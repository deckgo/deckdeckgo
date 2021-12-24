import {Deck, DeckData, DeckPublishData, deckPublishData, PublishData} from '@deckdeckgo/editor';

import {setData} from './data.utils';
import {initIndexHTML, initUpload, StorageUpload, updateMetaData, uploadPublishFileIC} from './publish.utils';
import {uploadSocialImage} from './publish.social.utils';

export const publishDeck = async ({
  deck: deckSource
}: {
  deck: Deck;
}): Promise<{deck: Deck; storageUpload: StorageUpload; publishData: PublishData}> => {
  const {id, data} = deckSource;
  const {meta} = data;

  // 1. Init and fill HTML
  const indexHTML: {html: string; publishData: DeckPublishData} = await initDeckIndexHTML({deck: deckSource});
  const {storageUpload, publishData} = await initUpload({indexHTML, folder: 'p', meta});

  // 2. Update deck published meta
  const deckData: DeckData = updateMetaData<DeckData>({data, meta: data.meta, name: data.name, storageUpload});

  // 3. Update deck meta information
  const deck: Deck = await setData<Deck, DeckData>({key: `/decks/${id}`, id, data: deckData});

  // 4. Upload
  await uploadPublishFileIC(storageUpload);

  // 5. Upload
  await uploadSocialImage({storageUpload, publishData});

  return {
    storageUpload,
    publishData,
    deck
  };
};

const initDeckIndexHTML = async ({deck}: {deck: Deck}): Promise<{html: string; publishData: DeckPublishData}> => {
  const publishData: DeckPublishData = await deckPublishData({deck});

  const {slides} = publishData;

  const updateTemplateContent = ({attr, template}: {attr: string | undefined; template: string}): string =>
    template.replace('<!-- DECKDECKGO_DECK -->', `<deckgo-deck id="slider" embedded="true" ${attr || ''}>${slides.join('')}</deckgo-deck>`);

  const {html}: {html: string} = await initIndexHTML({publishData, updateTemplateContent, sourceFolder: 'p'});

  return {
    html,
    publishData
  };
};

export const emitDeckPublished = (deck: Deck) => {
  const {id, data} = deck;

  const deployedDeck: Deck = {
    id,
    data: {
      ...data,
      deploy: {
        api: {
          status: 'successful',
          updated_at: new Date()
        }
      }
    }
  };

  const $event: CustomEvent<Deck> = new CustomEvent('deckPublished', {detail: deployedDeck});
  document.dispatchEvent($event);
};
