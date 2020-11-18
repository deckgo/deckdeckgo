import {Component, h, Host, Listen, State, Event, EventEmitter, Element} from '@stencil/core';

@Component({
  tag: 'app-slide-preview',
  styleUrl: 'app-slide-preview.scss',
})
export class AppSlidePreview {
  @Element() el: HTMLElement;

  @State()
  private preview: boolean = false;

  private deckPreviewRef!: HTMLDeckgoDeckElement;

  @Event({bubbles: false}) private previewAttached: EventEmitter<void>;

  componentDidUpdate() {
    if (this.preview) {
      this.previewAttached.emit();
    }
  }

  @Listen('slideDidUpdate', {target: 'document'})
  async onSlideDidUpdate($event: CustomEvent<HTMLElement>) {
    if (!$event || !$event.detail) {
      return;
    }

    const slide: HTMLElement = $event.detail;

    await this.updateSlide(slide);
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
    }
  }

  async initDeckPreview() {
    const deck: HTMLElement = document.querySelector('main > deckgo-deck');

    if (!deck) {
      return;
    }

    this.deckPreviewRef?.setAttribute('style', deck.style.cssText);

    await this.deckPreviewRef?.initSlideSize();
  }

  async updateSlide(slide: HTMLElement | undefined) {
    if (!slide) {
      return;
    }

    this.deckPreviewRef?.replaceChild(slide.cloneNode(true), this.deckPreviewRef.firstChild);
  }

  private async blockSlide() {
    await this.deckPreviewRef?.blockSlide(true);
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
      <deckgo-deck embedded={true} keyboard={false} ref={(el) => (this.deckPreviewRef = el as HTMLDeckgoDeckElement)} onSlidesDidLoad={() => this.blockSlide()}>
        <div>{/* Placeholder for replace first child */}</div>
      </deckgo-deck>
    );
  }
}
