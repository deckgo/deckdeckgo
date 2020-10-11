import {EventEmitter} from '@stencil/core';

import busyStore from '../../stores/busy.store';

export class SvgWavesHelper {
  constructor(private didChange: EventEmitter<HTMLElement>) {}

  async appendSvgWaves(slideElement: HTMLElement, svgWaves: SvgWaves, deck: boolean): Promise<void> {
    busyStore.state.deckBusy = true;

    const currentSlotElement: HTMLElement = slideElement.querySelector(":scope > [slot='background']");

    if (currentSlotElement) {
      slideElement.removeChild(currentSlotElement);
    }

    const div: HTMLElement = document.createElement('div');
    div.setAttribute('slot', 'background');

    const svgns = 'http://www.w3.org/2000/svg';

    const svg: SVGSVGElement = document.createElementNS(svgns, 'svg');
    const {style, path: _path, ...svgAttrs} = svgWaves;
    Object.keys(svgAttrs).forEach((attr) => {
      svg.setAttribute(attr, svgAttrs[attr]);
    });
    Object.keys(style).forEach((s) => {
      svg.style[s] = style[s];
    });

    const path: SVGPathElement = document.createElementNS(svgns, 'path');
    path.setAttribute('d', svgWaves.path.d);

    svg.appendChild(path);
    div.appendChild(svg);
    slideElement.appendChild(div);

    if (deck) {
      await (slideElement as any).loadBackground();
    } else {
      slideElement.setAttribute('custom-background', '');
    }

    this.didChange.emit(slideElement);
  }
}
