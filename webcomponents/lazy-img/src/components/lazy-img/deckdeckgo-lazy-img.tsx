import {Component, Element, Prop} from '@stencil/core';

@Component({
  tag: 'deckgo-lazy-img',
  styleUrl: 'deckdeckgo-lazy-img.scss',
  shadow: true
})
export class DeckdeckgoLazyImg {

  @Element() el: HTMLElement;

  @Prop()
  imgSrc: string;

  @Prop()
  imgAlt: string;

  @Prop()
  observerRootMargin: string = '100px 0px';

  @Prop()
  observerThreshold: number | number[];

  private observer: IntersectionObserver;

  componentDidLoad() {
    const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

    if (img) {
      this.observer = new IntersectionObserver(this.onIntersection, {
        rootMargin: this.observerRootMargin,
        threshold: this.observerThreshold
      });

      this.observer.observe(img);
    }
  }

  private onIntersection = async (entries: IntersectionObserverEntry[]) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    await this.handleIntersection(entries[0]);
  };

  private handleIntersection(entry: IntersectionObserverEntry): Promise<void> {
    return new Promise<void>((resolve) => {
      if (entry.isIntersecting) {

        if (this.observer) {
          this.observer.disconnect();
        }

        if (entry.target.getAttribute('data-src')) {
          entry.target.setAttribute('src', entry.target.getAttribute('data-src'));
          entry.target.removeAttribute('data-src');
        }
      }

      resolve();
    });
  }

  render() {
    return <img data-src={this.imgSrc} alt={this.imgAlt ? this.imgAlt : this.imgSrc}/>;
  }
}
