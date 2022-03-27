import {attributes as getAttributes, convertStyle, isTextNode} from '@deckdeckgo/editor';
import {h, JSX} from '@stencil/core';
import {isContentEditableDeck} from './parse-deck-elements.utils';
import {contentEditableParagraphChildren} from './parse-doc-elements.utils';
import {isNodeReveal} from './slot.utils';

export type AttrType = Record<string, string | Record<string, string> | undefined>;

export const parseElements = ({
  element,
  root,
  type,
  contentEditable = false
}: {
  element: Node;
  root: boolean;
  type: 'doc' | 'deck';
  contentEditable?: boolean;
}): JSX.IntrinsicElements | JSX.IntrinsicElements[] | string | undefined => {
  if (!element) {
    return undefined;
  }

  if (isTextNode(element)) {
    return element.textContent;
  }

  if (element.hasChildNodes()) {
    const results = [];

    for (const elem of Array.from(element.childNodes)) {
      const result = parseElements({element: elem, root: false, type, contentEditable});
      results.push(result);
    }

    return root ? results : parseElement({element: element as HTMLElement, content: results, contentEditable, type});
  }

  return parseElement({element: element as HTMLElement, content: element.textContent, contentEditable, type});
};

const parseElement = ({
  element,
  content,
  contentEditable,
  type
}: {
  element: HTMLElement;
  content: JSX.IntrinsicElements[] | string;
  contentEditable: boolean;
  type: 'doc' | 'deck';
}): JSX.IntrinsicElements => {
  const Elem: string = element.nodeName.toLowerCase();

  let attributes: AttrType = getAttributes(element);
  if (attributes.style) {
    attributes.style = convertStyle(attributes.style as string);
  }

  // Set content editable for deck
  if (type === 'deck' && contentEditable && isContentEditableDeck(element, attributes)) {
    if (isNodeReveal(element) && element.firstElementChild) {
      element.firstElementChild.setAttribute('contenteditable', `${true}`);
    } else {
      attributes['contenteditable'] = `${true}`;
    }
  }

  // Doc has no content editable on paragraphs except for specific types of element - e.g. <figure contenteditable=false/> and <figurecaption contenteditable=true />
  if (type === 'doc') {
    attributes = {
      ...attributes,
      ...contentEditableParagraphChildren(element)
    };
  }

  if (element.nodeName.toLowerCase() === 'deckgo-lazy-img') {
    attributes['custom-loader'] = `${true}`;
  }

  return <Elem {...attributes}>{content}</Elem>;
};
