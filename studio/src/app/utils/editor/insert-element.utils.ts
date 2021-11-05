import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

import {SlotType} from '../../types/editor/slot-type';

import {SlotUtils} from './slot.utils';
import {focusParagraph} from './paragraph.utils';
import {createHTMLElement} from './create-element.utils';
import {initDeckgoLazyImgAttributes} from './image.utils';

export const formatBlock = (slotType: SlotType) => document.execCommand('formatBlock', false, slotType.toLowerCase());

export const insertUnorderedList = () => document.execCommand('insertUnorderedList', false);

export const insertHTML = (slotType: SlotType) => {
  const element: HTMLElement = createHTMLElement({slotType});

  if (SlotUtils.isNodeEditable(element)) {
    element.setAttribute('editable', 'true');
  }

  document.execCommand('insertHTML', false, `${element.outerHTML}`);
};

export const insertImage = ({
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

  const br: HTMLBRElement = document.createElement('br');

  focusParagraph({paragraph});

  const onRender = async (mutations: MutationRecord[], observer: MutationObserver) => {
    observer.disconnect();

    const addedNodes: Node[] = mutations.reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);

    const imgNode: Node | undefined = addedNodes.find((node: Node) => node.nodeName?.toLowerCase() === SlotType.IMG);

    if (!imgNode) {
      return;
    }

    const element: HTMLDeckgoLazyImgElement = imgNode as HTMLDeckgoLazyImgElement;
    element.customLoader = true;
    await element.lazyLoad();
  };

  const docObserver: MutationObserver = new MutationObserver(onRender);
  docObserver.observe(container, {childList: true, subtree: true});

  document.execCommand('insertHTML', false, `${img.outerHTML}${br.outerHTML}`);
};
