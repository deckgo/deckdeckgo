import {Component, h, Listen, State} from '@stencil/core';

@Component({
  tag: 'app-slide-preview',
  styleUrl: 'app-slide-preview.scss',
})
export class AppSlidePreview {
  @State()
  private preview: boolean = false;

  private containerRef!: HTMLDeckgoDeckElement;

  @Listen('slideDidUpdate', {target: 'document'})
  async onSlideDidUpdate($event: CustomEvent<HTMLElement>) {
    if (!$event || !$event.detail) {
      return;
    }

    if (!this.containerRef) {
      return;
    }

    const slide: HTMLElement = $event.detail;

    this.containerRef.setAttribute('style', $event.detail.parentElement.style.cssText);

    this.containerRef.replaceChild(slide.cloneNode(true), this.containerRef.firstChild);

    await this.containerRef.initSlideSize();
  }

  @Listen('elementFocus', {target: 'document'})
  async onElementFocus($event: CustomEvent<HTMLElement>) {
    if (!$event || !$event.detail) {
      return;
    }

    const selectedElement: HTMLElement = $event.detail;

    this.preview = selectedElement?.parentElement?.nodeName?.toLowerCase().indexOf('deckgo-slide') >= 0;
  }

  render() {
    return <article>{this.renderPreview()}</article>;
  }

  private renderPreview() {
    if (!this.preview) {
      return undefined;
    }

    return (
      <deckgo-deck embedded={true} ref={(el) => (this.containerRef = el as HTMLDeckgoDeckElement)}>
        <div>{/* Placeholder for replace first child */}</div>
      </deckgo-deck>
    );
  }
}
