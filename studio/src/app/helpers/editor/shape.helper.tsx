import {EventEmitter} from '@stencil/core';

import {modalController, OverlayEventDetail} from '@ionic/core';

import {ShapeAction} from '../../utils/editor/shape-action';
import {ImageAction} from '../../utils/editor/image-action';
import {SlotType} from '../../utils/editor/slot-type';
import {DeckgoImgAction, ImageActionUtils} from '../../utils/editor/image-action.utils';
import {EditAction} from '../../utils/editor/edit-action';

import {BusyService} from '../../services/editor/busy/busy.service';

export class ShapeHelper {
  private busyService: BusyService;

  constructor(private didChange: EventEmitter<HTMLElement>) {
    this.busyService = BusyService.getInstance();
  }

  async appendShape(slideElement: HTMLElement, shapeAction: ShapeAction) {
    this.busyService.deckBusy(true);

    await this.appendContentShape(slideElement, shapeAction);
  }

  async appendShapeImage(slideElement: HTMLElement, imageAction: ImageAction) {
    if (imageAction.action === EditAction.OPEN_PHOTOS) {
      await this.openModal(slideElement);
    } else if (imageAction.action === EditAction.ADD_IMAGE) {
      await this.appendContentShapeImage(slideElement, imageAction.image);
    }
  }

  async cloneShape(shapeElement: HTMLElement) {
    this.busyService.deckBusy(true);

    await this.cloneShapeElement(shapeElement);
  }

  private async appendContentShapeImage(slideElement: HTMLElement, image: UnsplashPhoto | TenorGif | StorageFile) {
    const deckgImg: DeckgoImgAction | undefined = ImageActionUtils.extractAttributes(image);

    if (deckgImg !== undefined) {
      this.busyService.deckBusy(true);

      await this.appendContentShape(slideElement, {
        src: deckgImg.src,
        label: deckgImg.label,
        ratio: 1,
        type: 'img'
      });
    }
  }

  private async openModal(slideElement: HTMLElement) {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-photo'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.appendContentShapeImage(slideElement, detail.data);
    });

    await modal.present();
  }

  private appendContentShape(slideElement: HTMLElement, shapeAction: ShapeAction): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckGoDrr: HTMLElement = document.createElement(SlotType.DRAG_RESIZE_ROTATE);

      const size: number = 10; // percent

      if (typeof (slideElement as any).getContainer === 'function') {
        const container: HTMLElement = await (slideElement as any).getContainer();
        const height: number = (container.offsetWidth * size * shapeAction.ratio) / container.offsetHeight;

        deckGoDrr.style.setProperty('--height', `${height}`);
      } else {
        deckGoDrr.style.setProperty('--height', `${size}`);
      }

      deckGoDrr.style.setProperty('--width', `${size}`);
      deckGoDrr.style.setProperty('--left', `${50 - size / 2}`); // vw center
      deckGoDrr.style.setProperty('--top', `${50 - size / 2}`); // vh center

      this.addShape(deckGoDrr, slideElement, shapeAction.src, shapeAction.label, shapeAction.type);

      resolve();
    });
  }

  private cloneShapeElement(shapeElement: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckGoDrr: HTMLElement = document.createElement(SlotType.DRAG_RESIZE_ROTATE);

      deckGoDrr.setAttribute('style', shapeElement.getAttribute('style'));

      const img: HTMLElement = shapeElement.querySelector(SlotType.IMG);

      if (!img) {
        resolve();
        return;
      }

      const type: 'svg' | 'img' = (img as any).imgSrc !== undefined && (img as any).imgSrc !== '' ? 'img' : 'svg';
      const src: string = type === 'img' ? (img as any).imgSrc : (img as any).svgSrc;
      const label: string = type === 'img' ? (img as any).svgAlt : (img as any).svgAlt;

      this.addShape(deckGoDrr, shapeElement.parentElement, src, label, type);

      resolve();
    });
  }

  private addShape(deckGoDrr: HTMLElement, slideElement: HTMLElement, src: string, label: string, type: 'svg' | 'img') {
    deckGoDrr.setAttribute('slot', '');

    deckGoDrr.setAttribute('contentEditable', 'false');

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
}
