import {ParseElementsUtils} from '@deckdeckgo/studio';
import {h, JSX} from '@stencil/core';

export class ParseDeckSlotsUtils {
  static async convert(htmlContent: string, slotName: 'background' | 'header' | 'footer'): Promise<JSX.IntrinsicElements | undefined> {
    if (!htmlContent || htmlContent === undefined || htmlContent === '') {
      return undefined;
    }

    const div = document.createElement('div');
    div.setAttribute('slot', slotName);
    div.innerHTML = htmlContent;

    const content = await ParseElementsUtils.parseElements(div, true, false);

    return (
      <div slot={slotName} contentEditable={false}>
        {content}
      </div>
    );
  }

  static async stickLastChildren(el: HTMLElement): Promise<void> {
    if (!el) {
      return;
    }

    const deck: HTMLElement = el.querySelector('deckgo-deck');

    if (!deck) {
      return;
    }

    const slotsPromises: Promise<Element>[] = ['background', 'header', 'footer'].map((slotName: 'background' | 'header' | 'footer') => {
      return this.getDeckSlot(deck, slotName);
    });
    const [background, header, footer] = await Promise.all(slotsPromises);

    if (!background || !header || !footer) {
      return;
    }

    // Wait for next new slide to be loaded
    // prettier-ignore
    deck.addEventListener('slideDidLoad', async () => {
      // Append the children to move to the last position (if exists, append act as a move to last child)
      if (background) {
        deck.appendChild(background);
      }

      if (header)  {
        deck.appendChild(header);
      }

      if (footer) {
        deck.appendChild(footer);
      }
    }, {once: true});
  }

  private static async getDeckSlot(el: HTMLElement, slotName: 'background' | 'header' | 'footer'): Promise<Element> {
    return el.querySelector(`deckgo-deck > [slot='${slotName}']`);
  }
}
