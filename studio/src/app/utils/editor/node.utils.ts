export interface NodeColors {
  bgColor: string | undefined;
  color: string | undefined;
}

export class NodeUtils {
  static async childrenTextNode(elements: HTMLElement[]): Promise<HTMLElement[]> {
    return elements.reduce((acc: HTMLElement[], slot: HTMLElement) => {
      const text = Array.from(slot.childNodes).find((child) => child.nodeType === Node.TEXT_NODE);

      if (text !== null && text !== undefined && text.textContent.replace(/(?:\r\n|\r|\n|\s)/g, '') !== '') {
        acc.push(slot);
      }

      return acc;
    }, []);
  }

  static async children(elements: HTMLElement[]): Promise<HTMLElement[]> {
    return elements.reduce((acc: HTMLElement[], slot: HTMLElement) => {
      const children: NodeListOf<HTMLElement> = slot.querySelectorAll('*');

      const filteredChildren: HTMLElement[] | undefined = Array.from(children).filter((child) =>
        Array.from(child.childNodes).some((element) => element.nodeType === Node.TEXT_NODE)
      );

      if (filteredChildren && filteredChildren.length > 0) {
        acc.push(...Array.from(filteredChildren));
      }

      return acc;
    }, []);
  }

  static async findColors(
    node: HTMLElement,
    color: 'color' | 'background',
    deck: HTMLElement,
    slide: HTMLElement
  ): Promise<string | undefined> {
    // Just in case
    if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY') {
      return undefined;
    }

    if (!node.parentNode) {
      return undefined;
    }

    if (node.isEqualNode(deck)) {
      return deck.style.getPropertyValue(`--${color}`);
    }

    if (node.isEqualNode(slide) && (slide.style.getPropertyValue(`--${color}`) !== '' || slide.style[color] !== '')) {
      return slide.style.getPropertyValue(`--${color}`) !== '' ? slide.style.getPropertyValue(`--${color}`) : slide.style[color];
    }

    const styleAttr: string = color === 'background' ? 'background-color' : 'color';

    // initial act for background as inherit
    if (node.style[styleAttr] !== '' && ((color === 'background' && node.style[styleAttr] !== 'initial') || color === 'color')) {
      return node.style[styleAttr];
    }

    return await this.findColors(node.parentElement, color, deck, slide);
  }

  static nodeIndex(element: HTMLElement): number {
    return Array.from(element.parentNode.children).indexOf(element);
  }

  static isTextNode(element: Node | undefined): boolean {
    return element?.nodeType === Node.TEXT_NODE || element?.nodeType === Node.COMMENT_NODE;
  }

  static toHTMLElement(element: Node | undefined): HTMLElement | undefined | null {
    return this.isTextNode(element) ? element.parentElement : (element as HTMLElement);
  }
}
