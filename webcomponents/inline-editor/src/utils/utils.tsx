import {isRTL} from '@deckdeckgo/utils';

import {ContentAlign, ContentList} from '../types/enums';

export class DeckdeckgoInlineEditorUtils {
  static isBold(element: HTMLElement): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      let result: boolean = await this.isTag(element, 'b');

      if (result) {
        resolve(result);
        return;
      }

      result = await this.isTag(element, 'strong');

      resolve(result);
    });
  }

  static isItalic(element: HTMLElement): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      let result: boolean = await this.isTag(element, 'i');

      if (result) {
        resolve(result);
        return;
      }

      result = await this.isTag(element, 'em');

      if (result) {
        resolve(result);
        return;
      }

      if (!element.hasChildNodes()) {
        resolve(false);
        return;
      }

      // Sometimes it generates font-style: italic;
      result = element.style.fontStyle === 'italic';
      if (result) {
        resolve(result);
        return;
      }

      const children: HTMLCollection = element.children;
      if (children && children.length > 0) {
        const selectedChild: Element = Array.from(children).find((child: HTMLElement) => {
          return child.style.fontStyle === 'italic';
        });

        if (selectedChild) {
          resolve(true);
          return;
        }
      }

      resolve(result);
    });
  }

  static isUnderline(element: HTMLElement): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      let result: boolean = await this.isTag(element, 'u');

      if (result) {
        resolve(result);
        return;
      }

      if (!element.hasChildNodes()) {
        resolve(false);
        return;
      }

      // Sometimes it generates text-decoration-line: underline; too
      result = (element.style as any).textDecorationLine === 'underline';
      if (result) {
        resolve(result);
        return;
      }

      const children: HTMLCollection = element.children;
      if (children && children.length > 0) {
        const selectedChild: Element = Array.from(children).find((child: HTMLElement) => {
          return (child.style as any).textDecorationLine === 'underline';
        });

        if (selectedChild) {
          resolve(true);
          return;
        }
      }

      resolve(result);
    });
  }

  static isList(element: HTMLElement): Promise<ContentList | undefined> {
    return new Promise<ContentList | undefined>(async (resolve) => {
      if (!element) {
        resolve(undefined);
        return;
      }

      if (element.nodeName && element.nodeName.toLowerCase() === 'li' && element.parentElement && element.parentElement.nodeName) {
        resolve(
          element.parentElement.nodeName.toLowerCase() === 'ol'
            ? ContentList.ORDERED
            : element.parentElement.nodeName.toLowerCase() === 'ul'
            ? ContentList.UNORDERED
            : undefined
        );
        return;
      }

      resolve(undefined);
    });
  }

  static async getContentAlignment(element: HTMLElement): Promise<ContentAlign> {
    if (element.style.textAlign === 'center') {
      return ContentAlign.CENTER;
    } else if (element.style.textAlign === 'right') {
      return ContentAlign.RIGHT;
    } else if (element.style.textAlign === 'left') {
      return ContentAlign.LEFT;
    }

    return isRTL() ? ContentAlign.RIGHT : ContentAlign.LEFT;
  }

  private static isTag(element: HTMLElement, tagName: string): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!element) {
        resolve(false);
        return;
      }

      if (element.nodeName.toLowerCase() === tagName) {
        resolve(true);
        return;
      }

      if (element.hasChildNodes()) {
        const children: HTMLCollection = element.getElementsByTagName(tagName);
        resolve(children && children.length > 0);
      } else {
        resolve(false);
      }
    });
  }

  static isContainer(containers: string, element: Node): boolean {
    const containerTypes: string[] = containers.toLowerCase().split(',');
    return element && element.nodeName && containerTypes.indexOf(element.nodeName.toLowerCase()) > -1;
  }

  static isAnchorImage(anchorEvent: MouseEvent | TouchEvent, imgAnchor: string): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!anchorEvent) {
        resolve(false);
        return;
      }

      if (!anchorEvent.target || !(anchorEvent.target instanceof HTMLElement)) {
        resolve(false);
        return;
      }

      const target: HTMLElement = anchorEvent.target;

      resolve(target.nodeName && target.nodeName.toLowerCase() === imgAnchor);
    });
  }

  static execCommand(selection: Selection, command: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!selection || selection.rangeCount <= 0 || !document) {
        resolve();
        return;
      }

      const text: string = selection.toString();

      if (!text || text.length <= 0) {
        resolve();
        return;
      }

      document.execCommand(command);

      resolve();
    });
  }
}
