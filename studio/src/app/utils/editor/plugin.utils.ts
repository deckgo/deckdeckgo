import type {OverlayEventDetail} from '@ionic/core';
import {modalController} from '@ionic/core';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';
import {createEmptyElement, StyloPluginCreateParagraphsParams, transformParagraph} from '@deckdeckgo/stylo';

import {SlotType} from '../../types/editor/slot-type';

import {initDeckgoLazyImgAttributes} from './image.utils';

export const createParagraphImage = ({
  image,
  paragraph,
  container
}: {
  image: UnsplashPhoto | TenorGif | StorageFile;
  paragraph: HTMLElement;
  container: HTMLElement;
}) => {
  const deckgoImg: HTMLDeckgoLazyImgElement = document.createElement(SlotType.IMG);

  const img: HTMLDeckgoLazyImgElement = initDeckgoLazyImgAttributes({
    element: deckgoImg,
    image
  });

  const emptyDiv: HTMLElement = createEmptyElement({nodeName: 'div'});

  transformParagraph({
    elements: [img, emptyDiv],
    paragraph,
    container,
    focus: 'last'
  });
};

export const openPluginModal = async ({
  pluginParams,
  componentTag
}: {
  pluginParams: StyloPluginCreateParagraphsParams;
  componentTag: 'app-gif' | 'app-unsplash';
}) => {
  const modal: HTMLIonModalElement = await modalController.create({
    component: componentTag
  });

  modal.onDidDismiss().then(({data: unsplashImage}: OverlayEventDetail) => {
    const {container, paragraph} = pluginParams;

    createParagraphImage({
      image: unsplashImage,
      container,
      paragraph
    });
  });

  await modal.present();
};
