import {Component, Element, Event, EventEmitter, Method, Prop, Watch, h, State, Host} from '@stencil/core';

import {getSvgContent} from '../utils/request';

@Component({
  tag: 'deckgo-lazy-img',
  styleUrl: 'deckdeckgo-lazy-img.scss',
  shadow: true
})
export class DeckdeckgoLazyImg {

  @Element() el: HTMLElement;

  @Event() lazyImgDidLoad: EventEmitter;

  @Prop({reflect: true})
  imgSrc: string;

  @Prop({reflect: true})
  imgSrcSet: string;

  @Prop({reflect: true})
  imgAlt: string;

  @Prop({reflect: true})
  imgSizes: string;

  @Prop()
  observerRootMargin: string = '100px 0px';

  @Prop()
  observerThreshold: number | number[];

  @Prop()
  imgErrorSrc: string;

  @Prop({reflect: true})
  svgSrc: string;

  @Prop({reflect: true})
  ariaLabel: string;

  @Prop()
  intrinsicsize: string;

  @State()
  private svgContent: string;

  private observer: IntersectionObserver;

  @State()
  private imgLoaded: boolean = false;

  async componentDidLoad() {
    await this.init();

    this.lazyImgDidLoad.emit();
  }

  @Watch('imgSrc')
  async handleAttrImgSrc() {
    await this.init();
  }

  private async init() {
    if ('loading' in HTMLImageElement.prototype) {
      // In this case, loadImmediately apply the attributes but the platform will takes care of lazy loading the images
      await this.loadImmediately();
    } else if (window && 'IntersectionObserver' in window) {
      await this.deferIntersectionObserverLoad();
    } else {
      await this.loadImmediately();
    }
  }

  private loadImmediately(): Promise<void> {
    return this.load();
  }

  private deferIntersectionObserverLoad(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.observer = new IntersectionObserver(this.onIntersection, {
        rootMargin: this.observerRootMargin,
        threshold: this.observerThreshold
      });

      this.observer.observe(this.el.shadowRoot.host);

      resolve();
    });
  }

  @Method()
  lazyLoad(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.load();

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

        await this.load();
      }

      resolve();
    });
  }

  private load(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.svgSrc) {
        await this.loadSvg();
      } else {
        await this.loadImg();
      }

      resolve();
    });
  }

  private loadImg(): Promise<void> {
    return new Promise<void>((resolve) => {
      const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

      if (!img) {
        resolve();
        return;
      }

      if (this.imgSrc) {
        img.setAttribute('src', this.imgSrc);
      }

      if (this.imgSrcSet) {
        img.setAttribute('srcset', this.imgSrcSet);
      }

      resolve();
    });
  }

  private loadSvg(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        this.svgContent = await getSvgContent(this.svgSrc);
      } catch (err) {
        console.error(err);
      }

      resolve();
    });
  }

  private loadError(): Promise<void> {
    return new Promise<void>((resolve) => {
      const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

      if (!img) {
        resolve();
        return;
      }

      if (!this.imgErrorSrc || img.src === this.imgErrorSrc) {
        resolve();
        return;
      }

      if (img.src === this.imgSrc || img.srcset === this.imgSrcSet) {
        img.src = this.imgErrorSrc;
      }

      resolve();
    });
  }

  render() {
    if (this.svgContent) {
      return <Host>
        <div innerHTML={this.svgContent}></div>
      </Host>
    } else {
      return <Host>
        {this.renderImage()}
      </Host>;
    }
  }

  private renderImage() {
    // @ts-ignore
    return <img alt={this.imgLoaded ? (this.imgAlt ? this.imgAlt : this.imgSrc) : ''} loading="lazy" sizes={this.imgSizes ? this.imgSizes : undefined} intrinsicsize={this.intrinsicsize}
                onLoad={() => this.imgLoaded = true} onError={() => this.loadError()}/>
  }
}
