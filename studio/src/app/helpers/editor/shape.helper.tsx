import {EventEmitter} from '@stencil/core';

import {modalController, OverlayEventDetail} from '@ionic/core';

import busyStore from '../../stores/busy.store';
import authStore from '../../stores/auth.store';

import {ShapeAction, ShapeActionSVG} from '../../utils/editor/shape-action';
import {ImageAction} from '../../utils/editor/image-action';
import {SlotType} from '../../utils/editor/slot-type';
import {DeckgoImgAction, ImageActionUtils} from '../../utils/editor/image-action.utils';
import {EditAction} from '../../utils/editor/edit-action';

export class ShapeHelper {
  constructor(private didChange: EventEmitter<HTMLElement>, private signIn: EventEmitter<void>) {}

  async appendShape(slideElement: HTMLElement, shapeAction: ShapeAction) {
    if (shapeAction.svg) {
      await this.appendShapeSVG(slideElement, shapeAction.svg);
    } else if (shapeAction.img) {
      await this.appendShapeImage(slideElement, shapeAction.img);
    }
  }

  async appendText(slideElement: HTMLElement) {
    await this.appendContentText(slideElement);
  }

  private async appendShapeSVG(slideElement: HTMLElement, shapeAction: ShapeActionSVG) {
    busyStore.state.deckBusy = true;

    await this.appendContentShape(slideElement, shapeAction.ratio, shapeAction.src, shapeAction.label, 'svg');
  }

  private async appendShapeImage(slideElement: HTMLElement, imageAction: ImageAction) {
    if (imageAction.action === EditAction.OPEN_PHOTOS) {
      await this.openModal(slideElement, 'app-photo');
    } else if (imageAction.action === EditAction.OPEN_GIFS) {
      await this.openModal(slideElement, 'app-gif');
    } else if (imageAction.action === EditAction.OPEN_CUSTOM) {
      await this.openModalRestricted(slideElement);
    } else if (imageAction.action === EditAction.ADD_IMAGE) {
      await this.appendContentShapeImage(slideElement, imageAction.image);
    }
  }

  async cloneShape(shapeElement: HTMLElement) {
    busyStore.state.deckBusy = true;

    await this.cloneShapeElement(shapeElement);
  }

  private async appendContentShapeImage(slideElement: HTMLElement, image: UnsplashPhoto | TenorGif | StorageFile | SvgWaves) {
    const deckgImg: DeckgoImgAction | undefined = ImageActionUtils.extractAttributes(image);

    if (deckgImg !== undefined) {
      busyStore.state.deckBusy = true;

      await this.appendContentShape(slideElement, 1, deckgImg.src, deckgImg.label, 'img');
    }
  }

  private async openModalRestricted(slideElement: HTMLElement) {
    if (authStore.state.anonymous) {
      this.signIn.emit();
      return;
    }

    await this.openModal(slideElement, 'app-custom-images');
  }

  private async openModal(slideElement: HTMLElement, componentTag: string) {
    const modal: HTMLIonModalElement = await modalController.create({
      component: componentTag,
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.appendContentShapeImage(slideElement, detail.data);
    });

    await modal.present();
  }

  private appendContentShape(slideElement: HTMLElement, ratio: number, src: string, label: string, type: 'svg' | 'img'): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckGoDrr: HTMLElement = this.initDeckGoDrr();

      const size: number = 10; // percent

      if (typeof (slideElement as any).getContainer === 'function') {
        const container: HTMLElement = await (slideElement as any).getContainer();
        const height: number = (container.offsetWidth * size * ratio) / container.offsetHeight;

        deckGoDrr.style.setProperty('--height', `${height}`);
      } else {
        deckGoDrr.style.setProperty('--height', `${size}`);
      }

      deckGoDrr.style.setProperty('--width', `${size}`);
      deckGoDrr.style.setProperty('--left', `${50 - size / 2}`); // vw center
      deckGoDrr.style.setProperty('--top', `${50 - size / 2}`); // vh center

      this.addShape(deckGoDrr, slideElement, src, label, type);

      resolve();
    });
  }

  private async appendContentText(slideElement: HTMLElement) {
    const deckGoDrr: HTMLElement = this.initDeckGoDrr();

    deckGoDrr.setAttribute('text', 'true');

    const size: number = 10; // percent

    deckGoDrr.style.setProperty('--left', `${50 - size / 2}`); // vw center
    deckGoDrr.style.setProperty('--top', `${50 - size / 2}`); // vh center

    const section = document.createElement(SlotType.SECTION);
    section.setAttribute('contentEditable', 'true');

    this.addSection(deckGoDrr, slideElement, section);
  }

  private async cloneShapeElement(shapeElement: HTMLElement) {
    const deckGoDrr: HTMLElement = this.initDeckGoDrr();

    deckGoDrr.setAttribute('style', shapeElement.getAttribute('style'));

    const img: HTMLElement = shapeElement.querySelector(SlotType.IMG);

    if (img) {
      const type: 'svg' | 'img' = (img as any).imgSrc !== undefined && (img as any).imgSrc !== '' ? 'img' : 'svg';
      const src: string = type === 'img' ? (img as any).imgSrc : (img as any).svgSrc;
      const label: string = type === 'img' ? (img as any).svgAlt : (img as any).svgAlt;

      this.addShape(deckGoDrr, shapeElement.parentElement, src, label, type);

      return;
    }

    const section: HTMLElement = shapeElement.querySelector(SlotType.SECTION);

    if (section) {
      deckGoDrr.setAttribute('text', 'true');

      this.addSection(deckGoDrr, shapeElement.parentElement, section.cloneNode(true));
    }
  }

  private initDeckGoDrr(): HTMLElement {
    const deckGoDrr: HTMLElement = document.createElement(SlotType.DRAG_RESIZE_ROTATE);

    deckGoDrr.setAttribute('slot', '');
    deckGoDrr.setAttribute('contentEditable', 'false');

    return deckGoDrr;
  }

  private addShape(deckGoDrr: HTMLElement, slideElement: HTMLElement, src: string, label: string, type: 'svg' | 'img') {
    const deckgoImg: HTMLElement = document.createElement(SlotType.IMG);

    if (type === 'img') {
      (deckgoImg as any).imgSrc = src;
      (deckgoImg as any).imgAlt = label;
    } else {
      (deckgoImg as any).svgSrc = src;
      (deckgoImg as any).svgAlt = label;
    }

    deckGoDrr.appendChild(deckgoImg);

    slideElement.appendChild(deckGoDrr);

    this.didChange.emit(slideElement);
  }

  private addSection(deckGoDrr: HTMLElement, slideElement: HTMLElement, section: HTMLElement | Node) {
    deckGoDrr.appendChild(section);

    slideElement.appendChild(deckGoDrr);

    this.didChange.emit(slideElement);
  }
}
