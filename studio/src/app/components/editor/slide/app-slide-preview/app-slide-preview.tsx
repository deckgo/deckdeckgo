import {Component, h, Host, Listen, State, Event, EventEmitter, Element, Prop} from '@stencil/core';

import {cleanContent, isSlide} from '@deckdeckgo/deck-utils';
import {debounce, isIOS, isLandscape} from '@deckdeckgo/utils';

import {SlotUtils} from '../../../../utils/editor/slot.utils';

@Component({
  tag: 'app-slide-preview',
  styleUrl: 'app-slide-preview.scss',
})
export class AppSlidePreview {
  @Element() el: HTMLElement;

  @Prop()
  deckRef!: HTMLDeckgoDeckElement;

  @State()
  private preview: boolean = false;

  @State()
  private iosPositionTop: string | undefined = undefined;

  private deckPreviewRef!: HTMLDeckgoDeckElement;

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

    const selectedElement: HTMLElement = $event.detail;

    await this.stickyIOS(selectedElement);

    this.preview = isSlide(selectedElement?.parentElement) && SlotUtils.isNodeEditable(selectedElement) && !SlotUtils.isNodeWordCloud(selectedElement);

    if (this.preview) {
      await this.initDeckPreview();

      this.el.addEventListener('previewAttached', async () => await this.updateSlide(selectedElement.parentElement), {once: true});

      this.deckRef.addEventListener('keypress', () => this.debounceUpdatePreview(), {passive: true});
      this.deckRef.addEventListener('paste', () => this.debounceUpdatePreview(), {passive: true});
    } else {
      this.deckRef.removeEventListener('keypress', () => this.debounceUpdatePreview(), true);
      this.deckRef.removeEventListener('paste', () => this.debounceUpdatePreview(), true);
    }
  }

  private async stickyIOS(selectedElement: HTMLElement) {
    if (isIOS()) {
      this.iosPositionTop = isLandscape() ? `calc(${selectedElement.offsetTop}px - (128px * 9 / 16) - 32px)` : undefined;
    }
  }

  async initDeckPreview() {
    if (!this.deckRef) {
      return;
    }

    this.deckPreviewRef?.setAttribute('style', this.deckRef.style.cssText);

    await this.deckPreviewRef?.initSlideSize();
  }

  async updateSlide(slide: HTMLElement | undefined) {
    if (!slide || !this.deckPreviewRef) {
      return;
    }

    const content: string = await cleanContent(slide.outerHTML);

    this.deckPreviewRef.innerHTML = content;
  }

  private async blockSlide() {
    await this.deckPreviewRef?.blockSlide(true);
  }

  private async updatePreview() {
    const index = await this.deckRef?.getActiveIndex();

    if (index < 0) {
      return;
    }

    const slideElement: HTMLElement | undefined = this.deckRef?.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    await this.updateSlide(slideElement);
  }

  render() {
    const style = {...(this.iosPositionTop && {'--ios-top': this.iosPositionTop})};

    return (
      <Host
        style={style}
        class={{
          preview: this.preview,
        }}>
        <article>{this.renderPreview()}</article>
      </Host>
    );
  }

  private renderPreview() {
    if (!this.preview) {
      return undefined;
    }

    return (
      <deckgo-deck
        embedded={true}
        keyboard={false}
        ref={(el) => (this.deckPreviewRef = el as HTMLDeckgoDeckElement)}
        onSlidesDidLoad={() => this.blockSlide()}></deckgo-deck>
    );
  }
}
