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

}
