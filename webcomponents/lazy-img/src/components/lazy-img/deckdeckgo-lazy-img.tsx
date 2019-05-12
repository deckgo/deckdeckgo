import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

@Component({
  tag: 'deckgo-lazy-img',
  styleUrl: 'deckdeckgo-lazy-img.scss',
  shadow: true
})
export class DeckdeckgoLazyImg {

  @Element() el: HTMLElement;

  @Event() lazyImgDidLoad: EventEmitter;

  @Prop({reflectToAttr: true})
  imgSrc: string;

  @Prop({reflectToAttr: true})
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

    this.lazyImgDidLoad.emit();
  }

  @Method()
  lazyLoad(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');
      await this.load(img);

      resolve();
    });
  }

  private onIntersection = async (entries: IntersectionObserverEntry[]) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    await this.handleIntersection(entries[0]);
  };

  private handleIntersection(entry: IntersectionObserverEntry): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (entry.isIntersecting) {

        if (this.observer) {
          this.observer.disconnect();
        }

        await this.load(entry.target);
      }

      resolve();
    });
  }

  private load(img: HTMLImageElement | Element): Promise<void> {
    return new Promise<void>((resolve) => {
      if (img && img.hasAttribute('data-src')) {
        img.setAttribute('src', img.getAttribute('data-src'));
        img.removeAttribute('data-src');
      }

      resolve();
    });
  }

  render() {
    return <img data-src={this.imgSrc} alt={this.imgAlt ? this.imgAlt : this.imgSrc}/>;
  }
}
