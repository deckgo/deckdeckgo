import {modalController, OverlayEventDetail} from '@ionic/core';

import {StyloPlugin, StyloPluginCreateParagraphsParams} from '@deckdeckgo/stylo';

import i18n from '../stores/i18n.store';

import {createParagraphImage} from '../utils/editor/plugin.utils';

const openModal = async ({container, paragraph}: StyloPluginCreateParagraphsParams) => {
  const modal: HTMLIonModalElement = await modalController.create({
    component: 'app-unsplash'
  });

  modal.onDidDismiss().then(({data: unsplashImage}: OverlayEventDetail) => {
    createParagraphImage({
      image: unsplashImage,
      container,
      paragraph
    });
  });

  await modal.present();
};

export const imgUnsplash: StyloPlugin = {
  text: i18n.state.editor.stock_photo,
  icon: 'img',
  createParagraphs: (params: StyloPluginCreateParagraphsParams) => openModal(params)
};
