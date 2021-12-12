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

  const addObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
    addObserver.disconnect();

    const mutation: MutationRecord | undefined = mutations.find(
      ({addedNodes}: MutationRecord) => addedNodes[0]?.nodeName.toLowerCase() === slotType
    );

    moveCursorToEnd(mutation.addedNodes[0]);
  });

  addObserver.observe(container, {childList: true, subtree: true});

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

export const insertImage = ({image, paragraph}: {image: UnsplashPhoto | TenorGif | StorageFile; paragraph: HTMLElement}) => {
  const deckgoImg: HTMLDeckgoLazyImgElement = document.createElement(SlotType.IMG);

  const img: HTMLDeckgoLazyImgElement = initDeckgoLazyImgAttributes({
    element: deckgoImg,
    image
  });

  const emptyDiv: HTMLElement = createEmptyElement({nodeName: 'div'});

  focusParagraph({paragraph});

  paragraph.after(img, emptyDiv);
};
