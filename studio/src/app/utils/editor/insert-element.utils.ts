import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

import {SlotType} from '../../types/editor/slot-type';

import {SlotUtils} from './slot.utils';
import {focusParagraph} from './paragraph.utils';
import {createEmptyElement, createHTMLElement} from './create-element.utils';
import {initDeckgoLazyImgAttributes} from './image.utils';
import {NodeUtils} from './node.utils';

export const transformParagraph = ({
  slotType,
  paragraph,
  container,
  emptySibling
}: {
  slotType: SlotType;
  container: HTMLElement;
  paragraph: HTMLElement;
  emptySibling: boolean;
}) => {
  const element: HTMLElement = createHTMLElement({slotType});

  if (SlotUtils.isNodeEditable(element)) {
    element.setAttribute('editable', 'true');
  }

  const anchor: HTMLElement = NodeUtils.toHTMLElement(paragraph.previousSibling) || container;

  // We delete present paragraph and add the new element and assumes the mutation observer will trigger both delete and add in a single mutation.
  // Thanks to this, only one entry will be added in the undo-redo stack.
  container.removeChild(paragraph);

  if (emptySibling) {
    // We had a new dev after the element otherwise user might be trapped in new paragraph without being able to continue to write.
    // We also use DIV instead of SECTION because browsers seem to expect a DIV after a UL to be able to stop editing a list.
    const emptyDiv: HTMLElement = createEmptyElement({nodeName: 'div'});
    anchor.after(element, emptyDiv);
    return;
  }

  anchor.after(element);
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
