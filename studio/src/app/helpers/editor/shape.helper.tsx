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

  private appendContentShape(slideElement: HTMLElement, shapeAction: ShapeAction): Promise<void> {
    return new Promise<void>((resolve) => {
      const deckGoDnr: HTMLElement = document.createElement(SlotType.DRAGGABLE_RESIZABLE);

      const size: number = 8; // percent

      deckGoDnr.setAttribute('width', `${size}`);
      deckGoDnr.setAttribute('height', `${size}`);
      deckGoDnr.setAttribute('left', `${50 - size / 2}`); // vw center
      deckGoDnr.setAttribute('top', `${50 - size / 2}`); // vh center

      deckGoDnr.setAttribute('slot', '');

      const deckgoImg: HTMLElement = document.createElement(SlotType.IMG);

      (deckgoImg as any).svgSrc = shapeAction.src;
      (deckgoImg as any).svgAlt = shapeAction.label;

      deckGoDnr.appendChild(deckgoImg);

      slideElement.appendChild(deckGoDnr);

      this.didChange.emit(slideElement);

      resolve();
    });
  }
}
