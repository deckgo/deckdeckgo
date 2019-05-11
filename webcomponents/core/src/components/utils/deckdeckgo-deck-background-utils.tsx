import {DeckdeckgoUtils} from './deckdeckgo-utils';

export class DeckdeckgoDeckBackgroundUtils {

  // Lazy load images from slot=background
  static lazyBackgroundImages(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const promises = [];

      promises.push(this.lazyBackgroundImgTags(el));
      promises.push(this.lazyBackgroundImgComponents(el));

      await Promise.all(promises);

      resolve();
    });
  }

  private static lazyBackgroundImgTags(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const allSlottedImages: NodeListOf<HTMLElement> = el.querySelectorAll('img[slot=\'background\']');
      const allShadowImages: NodeListOf<HTMLElement> = el.querySelectorAll('[slot=\'background\'] img');

      const images: HTMLElement[] = Array.from(allSlottedImages).concat(Array.from(allShadowImages));

      await DeckdeckgoUtils.lazyLoadSelectedImages(images);

      resolve();
    });
  }

  private static lazyBackgroundImgComponents(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const allShadowImagesComponents: NodeListOf<HTMLElement> = el.querySelectorAll(':scope > [slot=\'background\'] deckgo-lazy-img');

      const images: HTMLElement[] = Array.from(allShadowImagesComponents);

      await DeckdeckgoUtils.lazyLoadSelectedImages(images);

      resolve();
    });
  }

  static cloneAndLoadBackground(el: HTMLElement, slides: HTMLElement[], cloneBackground: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {

      const background: HTMLElement = el.querySelector(':scope > [slot=\'background\']');

      if (!background) {
        resolve();
        return;
      }

      await this.lazyBackgroundImages(el);

      if (cloneBackground) {
        await this.cloneSlots(el, slides, 'background');
      }

      await this.showHideBackgroundSlot(el, cloneBackground);

      resolve();
    });
  }

  private static showHideBackgroundSlot(el: HTMLElement, cloneBackground: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slider: HTMLElement = el.shadowRoot.querySelector('div.deckgo-deck');

      if (!slider) {
        resolve();
        return;
      }

      if (cloneBackground) {
        slider.style.setProperty('--background-display', 'none');
      } else {
        slider.style.removeProperty('--background-display');
      }

      resolve();
    });
  }

  static cloneSlots(el: HTMLElement, slides: HTMLElement[], slotName: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!slides || slides.length <= 0) {
        resolve();
        return;
      }

      const slotElement: HTMLElement = el.querySelector(':scope > [slot=\'' + slotName + '\']');

      if (slotElement) {
        slides.forEach((slide: Element) => {

          const custom: boolean = slide.hasAttribute('custom-' + slotName);

          if (!custom) {
            const currentSlotElement: HTMLElement = slide.querySelector(':scope > [slot=\'' + slotName + '\']');

            if (currentSlotElement) {
              slide.removeChild(currentSlotElement);
            }

            slide.appendChild(slotElement.cloneNode(true));
          }
        });
      }

      resolve();
    });
  }

}
