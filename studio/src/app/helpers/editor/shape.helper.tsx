import {EventEmitter} from '@stencil/core';

import {ShapeAction} from '../../utils/editor/shape-action';
import {SlotType} from '../../utils/editor/slot-type';

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

  async cloneShape(shapeElement: HTMLElement) {
    this.busyService.deckBusy(true);

    await this.cloneShapeElement(shapeElement);
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

      this.addShape(deckGoDrr, slideElement, shapeAction.src, shapeAction.label);

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

      this.addShape(deckGoDrr, shapeElement.parentElement, (img as any).svgSrc, (img as any).svgAlt);

      resolve();
    });
  }

  private addShape(deckGoDrr: HTMLElement, slideElement: HTMLElement, src: string, label: string) {
    deckGoDrr.setAttribute('slot', '');

    deckGoDrr.setAttribute('contentEditable', 'false');

    const deckgoImg: HTMLElement = document.createElement(SlotType.IMG);

    (deckgoImg as any).svgSrc = src;
    (deckgoImg as any).svgAlt = label;

    deckGoDrr.appendChild(deckgoImg);

    slideElement.appendChild(deckGoDrr);

    this.didChange.emit(slideElement);
  }
}
