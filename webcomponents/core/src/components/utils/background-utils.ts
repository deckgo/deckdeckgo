import {lazyLoadSelectedImages, lazyLoadSelectedLazyImagesComponent} from '@deckdeckgo/utils';

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
      const allSlottedImages: NodeListOf<HTMLElement> = el.querySelectorAll("img[slot='background']");
      const allShadowImages: NodeListOf<HTMLElement> = el.querySelectorAll("[slot='background'] img");

      const images: HTMLElement[] = Array.from(allSlottedImages).concat(Array.from(allShadowImages));

      await lazyLoadSelectedImages(images);

      resolve();
    });
  }

  private static lazyBackgroundImgComponents(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const allShadowImagesComponents: NodeListOf<HTMLElement> = el.querySelectorAll(":scope > [slot='background'] deckgo-lazy-img");

      const images: HTMLElement[] = Array.from(allShadowImagesComponents);

      await lazyLoadSelectedLazyImagesComponent(images);

      resolve();
    });
  }

  static loadSlots(el: HTMLElement, slides: HTMLElement[], slotName: string, clone: boolean = true): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const background: HTMLElement = el.querySelector(`:scope > [slot='${slotName}']`);

      if (!background) {
        resolve();
        return;
      }

      await this.lazyBackgroundImages(el);

      if (clone) {
        await this.cloneSlots(el, slides, slotName);
      }

      await this.showHideBackgroundSlot(el, clone);

      resolve();
    });
  }

  static async loadSlot(el: HTMLElement, slide: HTMLElement, slotName: string, clone: boolean = true) {
    const slot: HTMLElement = el.querySelector(`:scope > [slot='${slotName}']`);

    if (!slot) {
      return;
    }

    await this.lazyBackgroundImages(el);

    if (clone) {
      await this.cloneSlot(el, slide, slotName);
    }

    await this.showHideBackgroundSlot(el, clone);
  }

  private static showHideBackgroundSlot(el: HTMLElement, cloneBackground: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slider: HTMLElement = el.shadowRoot.host as HTMLElement;

      if (!slider) {
        resolve();
        return;
      }

      if (!cloneBackground) {
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

      const slotElement: HTMLElement = el.querySelector(":scope > [slot='" + slotName + "']");

      if (slotElement) {
        slides.forEach((slide: Element) => {
          this.appendSlot(slotElement, slide, slotName);
        });
      }

      resolve();
    });
  }

  static async cloneSlot(el: HTMLElement, slide: HTMLElement, slotName: string) {
    if (!slide) {
      return;
    }

    const slotElement: HTMLElement = el.querySelector(":scope > [slot='" + slotName + "']");

    this.appendSlot(slotElement, slide, slotName);
  }

  static appendSlot(slotElement: HTMLElement, slide: Element, slotName: string) {
    if (!slotElement) {
      return;
    }

    const custom: boolean = slide.hasAttribute('custom-' + slotName);

    if (!custom) {
      const currentSlotElement: HTMLElement = slide.querySelector(":scope > [slot='" + slotName + "']");

      if (currentSlotElement) {
        slide.removeChild(currentSlotElement);
      }

      slide.appendChild(slotElement.cloneNode(true));
    }
  }

  static removeSlots(slides: HTMLElement[], slotName: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!slides || slides.length <= 0) {
        resolve();
        return;
      }

      slides.forEach((slide: Element) => {
        const custom: boolean = slide.hasAttribute('custom-' + slotName);

        if (!custom) {
          const currentSlotElement: HTMLElement = slide.querySelector(":scope > [slot='" + slotName + "']");

          if (currentSlotElement) {
            slide.removeChild(currentSlotElement);
          }
        }
      });

      resolve();
    });
  }
}
