export class DeckdeckgoUtils {

  static unifyEvent(e: any): any {
    return e.changedTouches ? e.changedTouches[0] : e;
  }

  static async lazyLoadImages(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      const images: HTMLElement[] = this.getAllImages(el);

      images.forEach((image: HTMLElement) => {
        if (image.getAttribute('data-src')) {
          image.setAttribute('src', image.getAttribute('data-src'));
          image.removeAttribute('data-src');

          // If image is part of a reveal group, let it be revealed with the reveal feature
          if (!image.classList.contains('deckgo-reveal')) {
            image.style.setProperty('visibility', 'initial');
          }
        }

        // Furthermore to lazy loading, we set pointer-events to none. Doing so we prevent images of being dragged.
        image.style.setProperty('pointer-events', 'none');
      });

      resolve();
    });
  };

  static getAllImages(el: HTMLElement): HTMLElement[] {
    const allSlotedImages: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] > img');
    const allShadowImages: NodeListOf<HTMLElement> = el.shadowRoot.querySelectorAll('img');

    return Array.from(allSlotedImages).concat(Array.from(allShadowImages));
  }

  static hideLazyLoadImages(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      let images: HTMLElement[] = DeckdeckgoUtils.getAllImages(el);

      if (!images) {
        resolve();
      } else {
        images = images.filter((image: HTMLElement) => image.getAttribute('data-src'));

        images.forEach((image: HTMLElement) => {
          image.style.setProperty('visibility', 'hidden');
        });

        resolve();
      }
    });
  }

  static debounce(func: Function, timeout?: number) {
    let timer: number;
    return (event) => {
      if (timer) {
        clearTimeout(timer);
      }

      timer = setTimeout(func, timeout > 0 ? timeout : 300, event);
    };
  }

}
