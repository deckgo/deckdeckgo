import {EventEmitter} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';
import {modalController} from '@ionic/core';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

import busyStore from '../../stores/busy.store';
import authStore from '../../stores/auth.store';

import {ImageAction} from '../../types/editor/image-action';
import {EditAction} from '../../types/editor/edit-action';
import {SlotUtils} from '../../utils/editor/slot.utils';
import {SlotType} from '../../types/editor/slot-type';
import {initDeckgoLazyImgAttributes} from '../../utils/editor/image.utils';

export class ImageHelper {
  constructor(
    private didChange: EventEmitter<HTMLElement>,
    private blockSlide: EventEmitter<boolean>,
    private signIn: EventEmitter<void>
  ) {}

  async imageAction(selectedTarget: HTMLElement, slide: boolean, deck: boolean, imageAction: ImageAction) {
    if (imageAction.action === EditAction.OPEN_UNSPLASH) {
      await this.openModal(selectedTarget, slide, deck, 'app-unsplash');
    } else if (imageAction.action === EditAction.DELETE_BACKGROUND) {
      await this.deleteBackground(selectedTarget, slide, deck);
    } else if (imageAction.action === EditAction.ADD_IMAGE && imageAction.image) {
      await this.appendImage(selectedTarget, slide, deck, imageAction.image);
    } else if (imageAction.action === EditAction.OPEN_GIFS) {
      await this.openModal(selectedTarget, slide, deck, 'app-gif');
    } else if (imageAction.action === EditAction.OPEN_CUSTOM) {
      await this.openModal(selectedTarget, slide, deck, 'app-storage-images', EditAction.OPEN_CUSTOM);
    } else if (imageAction.action === EditAction.OPEN_SVG_WAVES) {
      await this.openModal(selectedTarget, slide, deck, 'app-waves');
    }
  }

  async openModal(selectedTarget: HTMLElement, slide: boolean, deck: boolean, componentTag: string, action?: EditAction) {
    const modal: HTMLIonModalElement = await modalController.create({
      component: componentTag
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && selectedTarget) {
        if (action === EditAction.OPEN_CUSTOM_LOGO) {
          await this.updateSlideAttribute(selectedTarget, detail.data, 'img-src');
        } else if (action === EditAction.OPEN_DATA) {
          await this.updateSlideAttribute(selectedTarget, detail.data, 'src');
        } else {
          await this.appendImage(selectedTarget, slide, deck, detail.data);
        }
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  async openCustomModalRestricted(selectedTarget: HTMLElement, slide: boolean, deck: boolean, componentTag: string, action: EditAction) {
    if (!authStore.state.authUser) {
      this.signIn.emit();
      return;
    }

    await this.openModal(selectedTarget, slide, deck, componentTag, action);
  }

  private appendImage(
    selectedTarget: HTMLElement,
    slide: boolean,
    deck: boolean,
    image: UnsplashPhoto | TenorGif | StorageFile | Waves
  ): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!selectedTarget || !image || !document) {
        resolve();
        return;
      }

      busyStore.state.busy = true;

      if (slide || deck) {
        await this.appendBackgroundImg(selectedTarget, image as UnsplashPhoto | TenorGif | StorageFile, deck);
      } else {
        await this.appendContentImg(selectedTarget, image as UnsplashPhoto | TenorGif | StorageFile);
      }

      resolve();
    });
  }

  private deleteBackground(selectedTarget: HTMLElement, slide: boolean, deck: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!selectedTarget || !document) {
        resolve();
        return;
      }

      if (!slide && !deck) {
        resolve();
        return;
      }

      const currentSlotElement: HTMLElement = selectedTarget.querySelector(":scope > [slot='background']");

      if (currentSlotElement) {
        busyStore.state.busy = true;

        if (deck) {
          selectedTarget.removeChild(currentSlotElement);

          this.didChange.emit(selectedTarget);

          await (selectedTarget as HTMLDeckgoDeckElement).loadBackground();
        } else if (selectedTarget.hasAttribute('custom-background')) {
          selectedTarget.removeChild(currentSlotElement);

          selectedTarget.removeAttribute('custom-background');

          this.didChange.emit(selectedTarget);

          // Refresh background, deck might have one defined which need to be replicated on the slide which
          if (selectedTarget.parentElement) {
            await (selectedTarget.parentElement as HTMLDeckgoDeckElement).loadBackground();
          }
        }
      }

      resolve();
    });
  }

  private appendContentImg(selectedTarget: HTMLElement, image: UnsplashPhoto | TenorGif | StorageFile): Promise<void> {
    return new Promise<void>((resolve) => {
      let element: HTMLDeckgoLazyImgElement = SlotUtils.isNodeReveal(selectedTarget)
        ? (selectedTarget.firstElementChild as HTMLDeckgoLazyImgElement)
        : (selectedTarget as HTMLDeckgoLazyImgElement);

      if (element.nodeName?.toLowerCase() === SlotType.IMG) {
        element = initDeckgoLazyImgAttributes({element, image});
      } else {
        const deckgoImg: HTMLDeckgoLazyImgElement = document.createElement(SlotType.IMG);

        const img: HTMLDeckgoLazyImgElement = initDeckgoLazyImgAttributes({element: deckgoImg, image});

        element.appendChild(img);
      }

      let parent: HTMLElement = element.parentElement;

      if (SlotUtils.isNodeReveal(parent)) {
        parent = parent.parentElement;
      }

      this.didChange.emit(parent);

      resolve();
    });
  }

  private async appendBackgroundImg(
    selectedTarget: HTMLElement,
    image: UnsplashPhoto | TenorGif | StorageFile | Waves,
    deck: boolean
  ): Promise<void> {
    const currentSlotElement: HTMLElement = selectedTarget.querySelector(":scope > [slot='background']");

    if (currentSlotElement) {
      selectedTarget.removeChild(currentSlotElement);
    }

    const div: HTMLElement = document.createElement('div');
    div.setAttribute('slot', 'background');

    if (image.hasOwnProperty('viewBox')) {
      await this.appendChildSvg(selectedTarget, div, image as Waves, deck);
      return;
    }

    await this.appendChildImg(selectedTarget, div, image as UnsplashPhoto | TenorGif | StorageFile, deck);
  }

  private async appendChildSvg(selectedTarget: HTMLElement, div: HTMLElement, waves: Waves, deck: boolean) {
    const svgns = 'http://www.w3.org/2000/svg';

    const svg: SVGSVGElement = document.createElementNS(svgns, 'svg');
    const {path: _path, ...svgAttrs} = waves;

    Object.keys(svgAttrs).forEach((attr) => {
      svg.setAttribute(attr, svgAttrs[attr]);
    });

    const path: SVGPathElement = document.createElementNS(svgns, 'path');
    path.setAttribute('d', waves.path.d);

    svg.appendChild(path);

    div.appendChild(svg);

    selectedTarget.appendChild(div);

    if (deck) {
      await (selectedTarget as HTMLDeckgoDeckElement).loadBackground();
    } else {
      selectedTarget.setAttribute('custom-background', '');
    }

    this.didChange.emit(selectedTarget);
  }

  private async appendChildImg(
    selectedTarget: HTMLElement,
    div: HTMLElement,
    image: UnsplashPhoto | TenorGif | StorageFile,
    deck: boolean
  ) {
    const deckgoImg: HTMLDeckgoLazyImgElement = document.createElement(SlotType.IMG);

    const img: HTMLElement = initDeckgoLazyImgAttributes({element: deckgoImg, image, background: true});
    div.appendChild(img);

    selectedTarget.appendChild(div);

    if (deck) {
      // prettier-ignore
      img.addEventListener("lazyImgDidLoad", async () => {
        await (selectedTarget as HTMLDeckgoDeckElement).loadBackground();
        this.didChange.emit(selectedTarget);
      }, { once: true });
    } else {
      selectedTarget.setAttribute('custom-background', '');

      // prettier-ignore
      img.addEventListener("lazyImgDidLoad", async () => {
        this.didChange.emit(selectedTarget);
      }, { once: true });
    }
  }

  deleteSlideAttributeImgSrc(selectedTarget: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!selectedTarget) {
        resolve();
        return;
      }

      busyStore.state.busy = true;

      selectedTarget.removeAttribute('img-src');

      this.didChange.emit(selectedTarget);

      resolve();
    });
  }

  private updateSlideAttribute(selectedTarget: HTMLElement, image: StorageFile, attribute: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!selectedTarget || !image || !attribute) {
        resolve();
        return;
      }

      busyStore.state.busy = true;

      selectedTarget.setAttribute(attribute, image.downloadUrl);

      this.didChange.emit(selectedTarget);

      resolve();
    });
  }
}
