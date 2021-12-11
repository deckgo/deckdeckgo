import {isTextNode, StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';
import {moveCursorToEnd} from '@deckdeckgo/utils';

import {SlotType} from '../../types/editor/slot-type';

import {createEmptyElement, createHTMLElement} from './create-element.utils';
import {SlotUtils} from './slot.utils';
import {NodeUtils} from './node.utils';
import {initDeckgoLazyImgAttributes} from './image.utils';

export const findParagraph = ({element, container}: {element: Node; container: Node}): Node | undefined => {
  if (!container) {
    return undefined;
  }

  // Just in case
  if (container.nodeName.toUpperCase() === 'HTML' || container.nodeName.toUpperCase() === 'BODY') {
    return undefined;
  }

  if (!container.parentNode) {
    return undefined;
  }

  const {parentElement} = element;

  if (!parentElement) {
    return undefined;
  }

  if (parentElement.isEqualNode(container)) {
    return element;
  }

  return findParagraph({element: parentElement, container});
};

export const isParagraph = ({element, container}: {element: Node; container: Node}): boolean => {
  if (!element) {
    return false;
  }

  const {parentElement} = element;

  return parentElement?.isEqualNode(container);
};

export const focusParagraph = ({paragraph}: {paragraph: Node | undefined}) => {
  if (!isTextNode(paragraph)) {
    (paragraph as HTMLElement).focus();
  }

  moveCursorToEnd(paragraph);
};

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
