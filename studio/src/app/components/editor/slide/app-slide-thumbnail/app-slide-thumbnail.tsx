import {Component, h, Host, Element, Prop} from '@stencil/core';

import {cleanContent} from '@deckdeckgo/deck-utils';
import {debounce} from '@deckdeckgo/utils';

@Component({
  tag: 'app-slide-thumbnail',
  styleUrl: 'app-slide-thumbnail.scss'
})
export class AppSlideThumbnail {
  @Element() el: HTMLElement;

  @Prop()
  slide: HTMLElement;

  @Prop()
  deck: HTMLDeckgoDeckElement;

  private deckPreviewRef!: HTMLDeckgoDeckElement;

  private readonly debounceUpdateThumbnail: () => void;

  constructor() {
    this.debounceUpdateThumbnail = debounce(async () => {
      this.updateDeckStyle();

      await this.updateThumbnail();
    }, 150);
  }

  componentDidUpdate() {
    this.debounceUpdateThumbnail();
  }

  async componentDidLoad() {
    await this.initDeckThumbnail();

    this.debounceUpdateThumbnail();
  }

  private async initDeckThumbnail() {
    if (!this.slide || !this.deck) {
      return;
    }

    this.updateDeckStyle();

    await this.deckPreviewRef?.initSlideSize();
  }

  private updateDeckStyle() {
    if (!this.deck) {
      return;
    }

    this.deckPreviewRef?.setAttribute('style', this.deck.style.cssText);
  }

  private async updateThumbnail() {
    if (!this.slide || !this.deckPreviewRef) {
      return;
    }

    const content: string = await cleanContent(this.slide.outerHTML);

    this.deckPreviewRef.innerHTML = content;

    await Promise.all([this.lazyLoadImages(), this.lazyLoadCharts()]);
  }

  private async blockSlide($event: CustomEvent) {
    $event.stopPropagation();

    await this.deckPreviewRef?.blockSlide(true);
  }

  private async lazyLoadImages() {
    const images: NodeListOf<HTMLDeckgoLazyImgElement> = this.deckPreviewRef.querySelectorAll('deckgo-lazy-img');
    const promises: Promise<void>[] = Array.from(images).map((img: HTMLDeckgoLazyImgElement) => {
      img.customLoader = true;
      return img.lazyLoad();
    });
    await Promise.all(promises);
  }

  private async lazyLoadCharts() {
    const imgs: NodeListOf<HTMLDeckgoSlideChartElement> = this.deckPreviewRef.querySelectorAll('deckgo-slide-chart');
    const promises: Promise<void>[] = Array.from(imgs).map((img: HTMLDeckgoSlideChartElement) => {
      img.customLoader = true;
      return img.lazyLoadContent();
    });
    await Promise.all(promises);
  }

  render() {
    return <Host>{this.renderMiniature()}</Host>;
  }

  private renderMiniature() {
    if (!this.slide) {
      return undefined;
    }

    return (
      <deckgo-deck
        embedded={true}
        keyboard={false}
        ref={(el) => (this.deckPreviewRef = el as HTMLDeckgoDeckElement)}
        onSlidesDidLoad={($event: CustomEvent) => this.blockSlide($event)}
        onDeckDidLoad={($event: CustomEvent) => $event.stopPropagation()}></deckgo-deck>
    );
  }
}
