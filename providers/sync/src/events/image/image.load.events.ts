import type {DeckDeckGoCustomLoad} from '@deckdeckgo/lazy-img';
import {get} from 'idb-keyval';

export class ImageLoadEvents {
  init() {
    document.addEventListener('customLoad', this.onCustomLoad);
  }

  destroy() {
    document.removeEventListener('customLoad', this.onCustomLoad);
  }

  private onCustomLoad = async ($event: CustomEvent) => {
    if (!$event || !$event.detail || !$event.target || !($event.target instanceof HTMLElement)) {
      return;
    }

    await this.loadImage($event);
  };

  private loadImage($event: CustomEvent<DeckDeckGoCustomLoad>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const detail: DeckDeckGoCustomLoad = $event.detail;

      if (!detail.imgSrc || detail.imgSrc === undefined) {
        resolve();
        return;
      }

      if (detail.imgSrc.indexOf('http') === -1) {
        await this.loadLocalImage(detail);
      } else {
        await this.loadThirdPartyImage(detail);
      }

      resolve();
    });
  }

  private loadThirdPartyImage(detail: DeckDeckGoCustomLoad): Promise<void> {
    return new Promise<void>((resolve) => {
      detail.imgElement.setAttribute('src', detail.imgSrc);

      resolve();
    });
  }

  private loadLocalImage(detail: DeckDeckGoCustomLoad): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        const savedData: File = await get(detail.imgSrc);

        const URL = window.URL || window.webkitURL;
        const imgUrl: string = URL.createObjectURL(savedData);

        detail.imgElement.src = imgUrl;

        resolve();
      } catch (err) {
        // If error then no image is displayed
        console.error(err);
        resolve();
      }
    });
  }
}
