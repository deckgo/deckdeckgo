import {Component, h, Host, Listen, State} from '@stencil/core';

import {ContrastUtils, ParentsColors} from '../../../utils/editor/contrast.utils';

@Component({
  tag: 'app-slide-contrast',
  styleUrl: 'app-slide-contrast.scss',
})
export class AppSlideContrast {
  private readonly lowestAACompliantLevel: number = 1 / 3;

  @State()
  private warning: boolean = false;

  @Listen('slidesDidLoad', {target: 'document'})
  async onSlidesDidLoad() {
    await this.analyzeContrast();
  }

  @Listen('slideDidUpdate', {target: 'document'})
  async onSlideDidUpdate() {
    await this.analyzeContrast();
  }

  @Listen('deckDidChange', {target: 'document'})
  async onDeckDidChange() {
    await this.analyzeContrast();
  }

  @Listen('slideNextDidChange', {target: 'document'})
  @Listen('slidePrevDidChange', {target: 'document'})
  @Listen('slideToChange', {target: 'document'})
  async onSlideNavigate() {
    await this.analyzeContrast();
  }

  private async analyzeContrast() {
    this.warning = await this.hasLowContrast();
  }

  private async hasLowContrast(): Promise<boolean> {
    const deck: HTMLElement = document.querySelector('main > deckgo-deck');

    if (!deck) {
      return false;
    }

    const index = await (deck as any).getActiveIndex();

    const slide: HTMLElement = deck.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    if (!slide) {
      return false;
    }

    const slots: NodeListOf<HTMLElement> = slide.querySelectorAll(
      '[slot="title"]:not(:empty),[slot="content"]:not(:empty),[slot="start"]:not(:empty),[slot="end"]:not(:empty),[slot="header"]:not(:empty),[slot="footer"]:not(:empty)'
    );

    // TODO drr
    // const slots: NodeListOf<HTMLElement> = slide.querySelectorAll('[slot="title"],[slot="content"],[slot="start"],[slot="end"],[slot="header"],[slot="footer"],deckgo-drr');

    if (!slots || slots.length <= 0) {
      return false;
    }

    const slotsChildren = Array.from(slots).reduce((acc: HTMLElement[], slot: HTMLElement) => {
      const children: NodeListOf<HTMLElement> = slot.querySelectorAll('*');

      if (children && children.length > 0) {
        acc.push(...Array.from(children));
      }

      return acc;
    }, []);

    const elements: HTMLElement[] = slotsChildren && slotsChildren.length > 0 ? [...Array.from(slots), ...slotsChildren] : Array.from(slots);

    const parentsColors: ParentsColors = {
      slideBgColor: slide.style.background,
      slideColor: slide.style.color,
      deckBgColor: deck.style.getPropertyValue('--background'),
      deckColor: deck.style.getPropertyValue('--color'),
    };

    const promises: Promise<number>[] = Array.from(elements).map((slot: HTMLElement) => ContrastUtils.calculateContrastRatio(slot, parentsColors));

    const contrasts: number[] = await Promise.all(promises);

    if (!contrasts || contrasts.length <= 0) {
      return false;
    }

    const lowContrast: number | undefined = contrasts.find((contrast: number) => contrast > this.lowestAACompliantLevel);

    return lowContrast !== undefined;
  }

  render() {
    return (
      <Host
        class={{
          warning: this.warning,
        }}>
        <ion-label>Low contrast</ion-label>
        <ion-icon name="warning-outline"></ion-icon>
      </Host>
    );
  }
}
