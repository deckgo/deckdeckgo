import {Component, h, Host, Listen, State, Event, EventEmitter, Element, Prop} from '@stencil/core';

import {isSlide} from '@deckdeckgo/deck-utils';
import {debounce, isIOS, isLandscape} from '@deckdeckgo/utils';
import {selectSlide} from '@deckdeckgo/editor';

import {SlotUtils} from '../../../../../utils/editor/slot.utils';

@Component({
  tag: 'app-slide-preview',
  styleUrl: 'app-slide-preview.scss'
})
export class AppSlidePreview {
  @Element() el: HTMLElement;

  @Prop()
  deckRef!: HTMLDeckgoDeckElement;

  @State()
  private preview: boolean = false;

  @State()
  private iosPositionTop: string | undefined = undefined;

  @State()
  private slideElement: HTMLElement | undefined = undefined;

  @Event({bubbles: false}) private previewAttached: EventEmitter<void>;

  private readonly debounceUpdatePreview: () => void;

  constructor() {
    this.debounceUpdatePreview = debounce(async () => {
      await this.updatePreview();
    }, 750);
  }

  @Listen('ionKeyboardDidShow', {target: 'window'})
  onKeyboardDidShow(_$event: CustomEvent<{keyboardHeight: number}>) {}

  componentDidUpdate() {
    if (this.preview) {
      this.previewAttached.emit();
    }
  }

  @Listen('slideDidUpdate', {target: 'document'})
  async onSlideDidUpdate() {
    if (this.preview) {
      this.debounceUpdatePreview();
    }
  }

  @Listen('resetted', {target: 'document'})
  async onResetElement() {
    this.preview = false;
  }

  @Listen('elementFocus', {target: 'document'})
  async onElementFocus($event: CustomEvent<HTMLElement>) {
    if (!$event || !$event.detail) {
      return;
    }

    const selectedTarget: HTMLElement = $event.detail;

    await this.stickyIOS(selectedTarget);

    this.preview =
      isSlide(selectedTarget?.parentElement) &&
      SlotUtils.isNodeEditable(selectedTarget) &&
      !SlotUtils.isNodeCode(selectedTarget) &&
      !SlotUtils.isNodeWordCloud(selectedTarget);

    if (this.preview) {
      this.el.addEventListener('previewAttached', async () => await this.updateSlide(selectedTarget.parentElement), {once: true});

      this.deckRef.addEventListener('keypress', () => this.debounceUpdatePreview(), {passive: true});
      this.deckRef.addEventListener('paste', () => this.debounceUpdatePreview(), {passive: true});
    } else {
      this.deckRef.removeEventListener('keypress', () => this.debounceUpdatePreview(), true);
      this.deckRef.removeEventListener('paste', () => this.debounceUpdatePreview(), true);
    }
  }

  private async stickyIOS(selectedTarget: HTMLElement) {
    if (isIOS()) {
      this.iosPositionTop = isLandscape() ? `calc(${selectedTarget.offsetTop}px - (128px * 9 / 16) - 32px)` : undefined;
    }
  }

  private async updateSlide(slide: HTMLElement | undefined) {
    this.slideElement = slide?.cloneNode(true) as HTMLElement | undefined;
  }

  private async updatePreview() {
    const index = await this.deckRef?.getActiveIndex();

    if (index < 0) {
      return;
    }

    const slideElement: HTMLElement | undefined = selectSlide({deck: this.deckRef, index});

    await this.updateSlide(slideElement);
  }

  render() {
    const style = {...(this.iosPositionTop && {'--ios-top': this.iosPositionTop})};

    return (
      <Host
        style={style}
        class={{
          preview: this.preview
        }}>
        {this.renderPreview()}
      </Host>
    );
  }

  private renderPreview() {
    if (!this.preview) {
      return undefined;
    }

    return <app-slide-thumbnail slide={this.slideElement} deck={this.deckRef}></app-slide-thumbnail>;
  }
}
