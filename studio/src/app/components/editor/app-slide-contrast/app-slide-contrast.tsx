import {Component, h, Host, Listen, State} from '@stencil/core';

import {popoverController} from '@ionic/core';

import {ContrastUtils} from '../../../utils/editor/contrast.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';
import {SlotUtils} from '../../../utils/editor/slot.utils';

@Component({
  tag: 'app-slide-contrast',
  styleUrl: 'app-slide-contrast.scss',
})
export class AppSlideContrast {
  private readonly lowestAACompliantLevel: number = 3;

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
      '[slot="title"]:not(:empty),[slot="content"]:not(:empty),[slot="start"]:not(:empty),[slot="end"]:not(:empty),[slot="header"]:not(:empty),[slot="footer"]:not(:empty),[slot="author"]:not(:empty),[slot="top"]:not(:empty),[slot="bottom"]:not(:empty),deckgo-drr > section:not(:empty)'
    );

    if (!slots || slots.length <= 0) {
      return false;
    }

    const filteredSlots: HTMLElement[] = Array.from(slots).filter((element: HTMLElement) => {
      return !SlotUtils.isNodeCode(element) && !SlotUtils.isNodeWordCloud(element);
    });

    if (!filteredSlots || filteredSlots.length <= 0) {
      return false;
    }

    // Slots with direct text children
    const slotsWithText: HTMLElement[] = await NodeUtils.childrenTextNode(filteredSlots);

    // All children (<span/>) of the slots
    const children: HTMLElement[] = await NodeUtils.children(filteredSlots);

    const elements: HTMLElement[] =
      children && children.length > 0
        ? slotsWithText && slotsWithText.length > 0
          ? [...Array.from(slotsWithText), ...children]
          : [...children]
        : slotsWithText && slotsWithText.length > 0
        ? [...slotsWithText]
        : null;

    if (!elements) {
      return false;
    }

    const promises: Promise<number>[] = Array.from(elements).map((element: HTMLElement) => this.calculateRatio(element, deck, slide));

    const contrasts: number[] = await Promise.all(promises);

    if (!contrasts || contrasts.length <= 0) {
      return false;
    }

    const lowContrast: number | undefined = contrasts.find((contrast: number) => contrast < this.lowestAACompliantLevel);

    return lowContrast !== undefined;
  }

  private async calculateRatio(element: HTMLElement, deck: HTMLElement, slide: HTMLElement) {
    const bgColor = await NodeUtils.findColors(element, 'background', deck, slide);
    const color = await NodeUtils.findColors(element, 'color', deck, slide);

    return ContrastUtils.calculateContrastRatio(bgColor, color);
  }

  private async openInformation($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-contrast-info',
      event: $event,
      mode: 'ios',
      cssClass: 'info',
    });

    await popover.present();
  }

  render() {
    return (
      <Host
        class={{
          warning: this.warning,
        }}>
        <button class="ion-activatable" onClick={($event: UIEvent) => this.openInformation($event)}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-label>Low contrast</ion-label>
          <ion-icon name="warning-outline"></ion-icon>
        </button>
      </Host>
    );
  }
}
