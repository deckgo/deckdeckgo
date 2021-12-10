import {isRTL} from '@deckdeckgo/utils';

import {ContentAlign, ContentList, FontSize} from '../types/enums';

export class DeckdeckgoInlineEditorUtils {
  static getBold(element: HTMLElement): 'bold' | 'initial' | undefined {
    if (this.isTag(element, 'b')) {
      return 'bold';
    }

    if (this.isTag(element, 'strong')) {
      return 'bold';
    }

    return element.style.fontWeight === 'bold' ? 'bold' : element.style.fontWeight === 'initial' ? 'initial' : undefined;
  }

  static getItalic(element: HTMLElement): 'italic' | 'initial' | undefined {
    if (this.isTag(element, 'i')) {
      return 'italic';
    }

    if (this.isTag(element, 'em')) {
      return 'italic';
    }

    if (element.style.fontStyle === 'italic') {
      return 'italic';
    }

    if (element.style.fontStyle === 'initial') {
      return 'initial';
    }

    if (!element.hasChildNodes()) {
      return undefined;
    }

    const children: HTMLCollection = element.children;
    if (children && children.length > 0) {
      const selectedChild: HTMLElement = Array.from(children).find((child: HTMLElement) => {
        return child.style.fontStyle === 'italic' || child.style.fontStyle === 'initial';
      }) as HTMLElement;

      if (selectedChild) {
        return selectedChild.style.fontStyle === 'italic' ? 'italic' : 'initial';
      }
    }

    return undefined;
  }

  static getUnderline(element: HTMLElement): 'underline' | 'initial' | undefined {
    if (this.isTag(element, 'u')) {
      return 'underline';
    }

    if (element.style.textDecoration?.indexOf('underline') > -1 || element.style.textDecorationLine?.indexOf('underline') > -1) {
      return 'underline';
    }

    if (element.style.textDecoration?.indexOf('initial') > -1 || element.style.textDecorationLine?.indexOf('initial') > -1) {
      return 'initial';
    }

    if (!element.hasChildNodes()) {
      return undefined;
    }

    const children: HTMLCollection = element.children;
    if (children && children.length > 0) {
      const selectedChild: HTMLElement = Array.from(children).find((child: HTMLElement) => {
        return (
          child.style.textDecoration?.indexOf('underline') > -1 ||
          child.style.textDecorationLine?.indexOf('underline') > -1 ||
          child.style.textDecorationLine?.indexOf('initial') > -1
        );
      }) as HTMLElement;

      if (selectedChild) {
        return selectedChild.style.fontStyle?.indexOf('underline') > -1 ? 'underline' : 'initial';
      }
    }

    return undefined;
  }

  static getStrikeThrough(element: HTMLElement): 'strikethrough' | 'initial' | undefined {
    if (this.isTag(element, 'strike')) {
      return 'strikethrough';
    }

    if (element.style.textDecoration?.indexOf('line-through') > -1 || element.style.textDecorationLine?.indexOf('line-through') > -1) {
      return 'strikethrough';
    }

    if (element.style.textDecoration?.indexOf('initial') > -1 || element.style.textDecorationLine?.indexOf('initial') > -1) {
      return 'initial';
    }

    if (!element.hasChildNodes()) {
      return undefined;
    }

    const children: HTMLCollection = element.children;
    if (children && children.length > 0) {
      const selectedChild: HTMLElement = Array.from(children).find((child: HTMLElement) => {
        return (
          child.style.textDecoration?.indexOf('line-through') > -1 ||
          child.style.textDecorationLine?.indexOf('line-through') > -1 ||
          child.style.textDecorationLine?.indexOf('initial') > -1
        );
      }) as HTMLElement;

      if (selectedChild) {
        return selectedChild.style.fontStyle?.indexOf('line-through') > -1 ? 'strikethrough' : 'initial';
      }
    }

    return undefined;
  }

  static getList(element: HTMLElement): ContentList | undefined {
    if (!element) {
      return undefined;
    }

    if (element.nodeName && element.nodeName.toLowerCase() === 'li' && element.parentElement && element.parentElement.nodeName) {
      return element.parentElement.nodeName.toLowerCase() === 'ol'
        ? ContentList.ORDERED
        : element.parentElement.nodeName.toLowerCase() === 'ul'
        ? ContentList.UNORDERED
        : undefined;
    }

    return undefined;
  }

  static getContentAlignment(element: HTMLElement): ContentAlign {
    const style: CSSStyleDeclaration = window.getComputedStyle(element);

    if (style.textAlign === 'center') {
      return ContentAlign.CENTER;
    } else if (style.textAlign === 'right') {
      return ContentAlign.RIGHT;
    } else if (style.textAlign === 'left') {
      return ContentAlign.LEFT;
    }

    return isRTL() ? ContentAlign.RIGHT : ContentAlign.LEFT;
  }

  private static isTag(element: HTMLElement, tagName: string): boolean {
    if (!element) {
      return false;
    }

    if (element.nodeName.toLowerCase() === tagName) {
      return true;
    }

    if (element.hasChildNodes()) {
      const children: HTMLCollection = element.getElementsByTagName(tagName);
      return children && children.length > 0;
    }

    return false;
  }

  static isContainer(containers: string, element: Node): boolean {
    const containerTypes: string[] = containers.toLowerCase().split(',');
    return element && element.nodeName && containerTypes.indexOf(element.nodeName.toLowerCase()) > -1;
  }

  static isAnchorImage(anchorEvent: MouseEvent | TouchEvent, imgAnchor: string): boolean {
    if (!anchorEvent) {
      return false;
    }

    if (!anchorEvent.target || !(anchorEvent.target instanceof HTMLElement)) {
      return false;
    }

    const target: HTMLElement = anchorEvent.target;

    return target.nodeName && target.nodeName.toLowerCase() === imgAnchor;
  }

  static findContainer(containers: string, element: HTMLElement | Node): HTMLElement | undefined {
    if (!element) {
      return undefined;
    }

    if (element.nodeName.toUpperCase() === 'HTML' || element.nodeName.toUpperCase() === 'BODY' || !element.parentElement) {
      return undefined;
    }

    if (DeckdeckgoInlineEditorUtils.isContainer(containers, element)) {
      return element as HTMLElement;
    }

    return this.findContainer(containers, element.parentElement);
  }

  static getFontSize(element: HTMLElement): FontSize | undefined {
    if (!element || !element.hasAttribute('size')) {
      return undefined;
    }

    return element.getAttribute('size') as FontSize;
  }
}
