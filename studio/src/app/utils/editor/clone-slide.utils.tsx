import {convertStyle, SlideTemplate} from '@deckdeckgo/editor';
import {getAttributes} from '@deckdeckgo/studio';
import {h, JSX} from '@stencil/core';
import {v4 as uuid} from 'uuid';
import {InitTemplate} from './create-slides.utils';

export class CloneSlideUtils {
  static async toggleTemplate(selectedTarget: HTMLElement, initTemplate: InitTemplate): Promise<JSX.IntrinsicElements> {
    const SlideElement: string = `deckgo-slide-${(initTemplate.template as SlideTemplate).toLowerCase()}`;

    const attributes: any = {
      ...getAttributes(selectedTarget),
      ...(initTemplate.attributes && {...initTemplate.attributes})
    };

    delete attributes['class'];

    if (attributes.style) {
      attributes.style = convertStyle(attributes.style);
    }

    if (initTemplate.style) {
      attributes.style = {
        ...attributes.style,
        ...initTemplate.style
      };
    }

    // If we toggle to a CONTENT template from a one aligned bottom, we should think about removing the alignment to get the default one
    if (initTemplate.template === SlideTemplate.CONTENT && !initTemplate.style) {
      delete attributes.style?.['--slide-content-justify-content'];
    }

    // If we toggle to a SPLIT template from a one vertical, we should think about removing the vertical alignment to get the default one
    if (initTemplate.template === SlideTemplate.SPLIT && !initTemplate.attributes) {
      delete attributes['vertical'];
    }

    // Toggle slot names if needed
    if (![SlideTemplate.CONTENT, SlideTemplate.TITLE].includes(initTemplate.template as SlideTemplate)) {
      this.toggleSlotName(selectedTarget, 'title', 'start');
      this.toggleSlotName(selectedTarget, 'content', 'end');
    } else if (![SlideTemplate.SPLIT].includes(initTemplate.template as SlideTemplate)) {
      this.toggleSlotName(selectedTarget, 'start', 'title');
      this.toggleSlotName(selectedTarget, 'end', 'content');
    }

    return <SlideElement key={uuid()} {...attributes} innerHTML={selectedTarget.innerHTML}></SlideElement>;
  }

  private static toggleSlotName(selectedTarget: HTMLElement, currentSlot: string, newSlot: string) {
    const slot: HTMLElement | null = selectedTarget.querySelector(`:scope > [slot="${currentSlot}"]`);
    slot?.setAttribute('slot', newSlot);
  }
}
