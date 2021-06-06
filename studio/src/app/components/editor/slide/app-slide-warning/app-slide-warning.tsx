import {Component, h, Host, Listen, State} from '@stencil/core';

import {popoverController} from '@ionic/core';

import {debounce} from '@deckdeckgo/utils';

import i18n from '../../../../stores/i18n.store';

import settingsStore from '../../../../stores/settings.store';

import {ContrastUtils} from '../../../../utils/editor/contrast.utils';
import {NodeUtils} from '../../../../utils/editor/node.utils';
import {SlotUtils} from '../../../../utils/editor/slot.utils';

@Component({
  tag: 'app-slide-warning',
  styleUrl: 'app-slide-warning.scss',
})
export class AppSlideWarning {
  private readonly lowestAACompliantLevel: number = 3;

  @State()
  private warningLowContrast: boolean = false;

  @State()
  private warningOverflow: boolean = false;

  private readonly debounceResizeWarningOverflow: () => void = debounce(async () => {
    this.warningOverflow = await this.hasOverflow();
  }, 500);

  @Listen('slidesDidLoad', {target: 'document'})
  async onSlidesDidLoad() {
    await this.detectWarnings();
  }

  @Listen('slideDidUpdate', {target: 'document'})
  async onSlideDidUpdate() {
    await this.detectWarnings();
  }

  @Listen('deckDidChange', {target: 'document'})
  async onDeckDidChange() {
    await this.detectWarnings();
  }

  @Listen('remoteSlideDidChange', {target: 'document'})
  async onRemoteSlideDidChange() {
    await this.detectWarnings();
  }

  @Listen('slideNextDidChange', {target: 'document'})
  @Listen('slidePrevDidChange', {target: 'document'})
  @Listen('slideToChange', {target: 'document'})
  async onSlideNavigate() {
    await this.detectWarnings();
  }

  @Listen('resize', {target: 'window'})
  async onSlideDidChange() {
    this.debounceResizeWarningOverflow();
  }

  private async detectWarnings() {
    this.warningLowContrast = await this.hasLowContrast();
    this.debounceResizeWarningOverflow();
  }

  private async hasOverflow(): Promise<boolean> {
    const {slide} = await this.getCurrentSlide();

    if (!slide) {
      return false;
    }

    const slots: NodeListOf<HTMLElement> = slide.querySelectorAll(
      '[slot="title"]:not(:empty),[slot="content"]:not(:empty),[slot="start"]:not(:empty),[slot="end"]:not(:empty),[slot="author"]:not(:empty),[slot="top"]:not(:empty),[slot="bottom"]:not(:empty)'
    );

    if (!slots || slots.length <= 0) {
      return false;
    }

    const filteredSlots: HTMLElement[] = Array.from(slots).filter((element: HTMLElement) => {
      return !SlotUtils.isNodeEditable(element) || SlotUtils.isNodeMarkdown(element);
    });

    if (!filteredSlots || filteredSlots.length <= 0) {
      return false;
    }

    const promises: Promise<boolean>[] = Array.from(filteredSlots).map((element: HTMLElement) => this.isOverflown(element, slide));

    const overflows: boolean[] = await Promise.all(promises);

    if (!overflows || overflows.length <= 0) {
      return false;
    }

    const hasOverflow: boolean | undefined = overflows.find((overflow: boolean) => overflow);

    return hasOverflow === true;
  }

  private async hasLowContrast(): Promise<boolean> {
    const {deck, slide} = await this.getCurrentSlide();

    if (!slide || !deck) {
      return false;
    }

    const slots: NodeListOf<HTMLElement> = slide.querySelectorAll(
      '[slot="title"]:not(:empty),[slot="content"]:not(:empty),[slot="start"]:not(:empty),[slot="end"]:not(:empty),[slot="header"]:not(:empty),[slot="footer"]:not(:empty),[slot="author"]:not(:empty),[slot="top"]:not(:empty),[slot="bottom"]:not(:empty),deckgo-drr > section:not(:empty)'
    );

    if (!slots || slots.length <= 0) {
      return false;
    }

    const filteredSlots: HTMLElement[] = Array.from(slots).filter((element: HTMLElement) => {
      return !SlotUtils.isNodeEditable(element);
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

  private async calculateRatio(element: HTMLElement, deck: HTMLElement, slide: HTMLElement): Promise<number> {
    const bgColor = await NodeUtils.findColors(element, 'background', deck, slide);
    const color = await NodeUtils.findColors(element, 'color', deck, slide);

    return ContrastUtils.calculateContrastRatio(bgColor, color);
  }

  private async isOverflown(element: HTMLElement, slide: HTMLElement): Promise<boolean> {
    if (typeof (element as any).getContainer === 'function') {
      const shadowedContainer: HTMLElement | null = await (element as any).getContainer();
      return element && shadowedContainer.offsetHeight > element.clientHeight;
    }

    return this.isHTMLElementOverflown(element, slide);
  }

  private async isHTMLElementOverflown(element: HTMLElement | null, slide: HTMLElement): Promise<boolean> {
    if (!element) {
      return false;
    }

    const {scrollHeight, offsetTop} = element;

    return offsetTop < 0 || offsetTop + scrollHeight > slide.scrollHeight;
  }

  private async getCurrentSlide(): Promise<{deck: HTMLElement | null; slide: HTMLElement | null}> {
    const deck: HTMLElement = document.querySelector('main > deckgo-deck');

    if (!deck) {
      return {
        deck: null,
        slide: null,
      };
    }

    const index = await (deck as any).getActiveIndex();

    return {
      deck,
      slide: deck.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')') as HTMLElement | null,
    };
  }

  private async openInformation($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-slide-warning-info',
      componentProps: {
        lowContrast: this.warningLowContrast,
        overflow: this.warningOverflow,
      },
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
          warning: (settingsStore.state.contrastWarning === 'on' && this.warningLowContrast) || this.warningOverflow,
        }}>
        <button class="ion-activatable" onClick={($event: UIEvent) => this.openInformation($event)}>
          <ion-ripple-effect></ion-ripple-effect>
          {this.renderMsg()}
          <ion-icon name="warning-outline"></ion-icon>
        </button>
      </Host>
    );
  }

  private renderMsg() {
    if ((settingsStore.state.contrastWarning === 'on' && this.warningLowContrast) && this.warningOverflow) {
      return (
        <ion-label>
          {i18n.state.warning.low_contrast} + {i18n.state.warning.overflow}
        </ion-label>
      );
    } else if (settingsStore.state.contrastWarning === 'on' && this.warningLowContrast) {
      return <ion-label>{i18n.state.warning.low_contrast}</ion-label>;
    } else if (this.warningOverflow) {
      return <ion-label>{i18n.state.warning.overflow}</ion-label>;
    } else {
      return undefined;
    }
  }
}
