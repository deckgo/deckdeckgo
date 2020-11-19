import {Component, h, Host, Listen, State, Event, EventEmitter, Element, Prop} from '@stencil/core';

import {cleanContent} from '@deckdeckgo/deck-utils';
import {debounce} from '@deckdeckgo/utils';

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

  private deckPreviewRef!: HTMLDeckgoDeckElement;

  @Event({bubbles: false}) private previewAttached: EventEmitter<void>;

  private readonly debounceUpdatePreview: () => void;

  constructor() {
    this.debounceUpdatePreview = debounce(async () => {
      await this.updatePreview();
    }, 250);
  }

  componentDidUpdate() {
    if (this.preview) {
      this.previewAttached.emit();
    }
  }

  @Listen('elementFocus', {target: 'document'})
  async onElementFocus($event: CustomEvent<HTMLElement>) {
    if (!$event || !$event.detail) {
      return;
    }

    const selectedElement: HTMLElement = $event.detail;

    this.preview = selectedElement?.parentElement?.nodeName?.toLowerCase().indexOf('deckgo-slide') >= 0;

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

  async initDeckPreview() {
    if (!this.deckRef) {
      return;
    }

    this.deckPreviewRef?.setAttribute('style', this.deckRef.style.cssText);

    await this.deckPreviewRef?.initSlideSize();
  }

  async updateSlide(slide: HTMLElement | undefined) {
    if (!slide) {
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
    return (
      <Host
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
