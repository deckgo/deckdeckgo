import {moveCursorToEnd} from '@deckdeckgo/utils';

import {NodeUtils} from './node.utils';

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
  if (!NodeUtils.isTextNode(paragraph)) {
    (paragraph as HTMLElement).focus();
  }

  moveCursorToEnd(paragraph);
};
