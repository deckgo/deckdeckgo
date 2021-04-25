import {Component, Element, Event, EventEmitter, Method, Prop, Watch, h, State, Host} from '@stencil/core';

import {getSvgContent} from '../utils/request';

import {DeckDeckGoCustomLoad} from '../interfaces/custom-load';

@Component({
  tag: 'deckgo-lazy-img',
  styleUrl: 'deckdeckgo-lazy-img.scss',
  shadow: true,
})
export class DeckdeckgoLazyImg {
  @Element() el: HTMLElement;

  @Event() lazyImgDidLoad: EventEmitter;

  /**
   * The image source (= src) to lazy load
   */
  @Prop({reflect: true})
  imgSrc: string;

  /**
   * The attribute "srcset" (= multiple URI) to lazy load in case you would like to provide multiple images for responsiveness
   */
  @Prop({reflect: true})
  imgSrcSet: string;

  /**
   * The image alternate text
   */
  @Prop({reflect: true})
  imgAlt: string;

  /**
   * 	The set of media conditions to indicates what image size would be best to choose
   */
  @Prop({reflect: true})
  imgSizes: string;

  /**
   * A string which specifies a set of offsets to add to the root's bounding_box when calculating intersections, effectively shrinking or growing the root for calculation purposes.
   */
  @Prop()
  observerRootMargin: string = '300px';

  /**
   * Either a single number or an array of numbers between 0.0 and 1.0, specifying a ratio of intersection area to total bounding box area for the observed target.
   */
  @Prop()
  observerThreshold: number | number[] = 0.25;

  /**
   * An optional image which could be displayed in case the main image would not be resolved
   */
  @Prop()
  imgErrorSrc: string;

  /**
   * The SVG image source (= URI) to lazy load and to parse (no <img/> tag will be use to render the svg) aria-label	string
   */
  @Prop({reflect: true})
  svgSrc: string;

  /**
   * If you are using the above SVG option, provide the accessibility information using this attribute
   */
  @Prop({reflect: true})
  ariaLabel: string;

  /**
   * An intrinsicsize for the native lazy-loading
   */
  @Prop()
  intrinsicsize: string;

  /**
   * The image width
   */
  @Prop()
  imgWidth: number;

  /**
   * The image height
   */
  @Prop()
  imgHeight: number;

  /**
   * In case you would like to take care by yourself to apply the load of the image. If turn to true then the component will emit an event customLoad when the image intersect the viewport instead of displaying it (doesn't apply for svg but only for img-src and img-src-set)
   */
  @Prop()
  customLoader: boolean = false;

  /**
   * If set to lazy, the web native lazy capability of the browser, if available, will be used to lazy load the image
   */
  @Prop()
  loading: 'lazy' | 'eager' = 'eager';

  @State()
  private svgContent: string;

  private observer: IntersectionObserver;

  @State()
  private imgLoaded: boolean = false;

  /**
   * Emitted if component property custom-loader is set to true and if an image (img-src or img-src-set) as to be loaded.
   */
  @Event()
  customLoad: EventEmitter<DeckDeckGoCustomLoad>;

  async componentDidLoad() {
    await this.init();

    this.lazyImgDidLoad.emit();
  }

  @Watch('imgSrc')
  async handleAttrImgSrc() {
    await this.init();
  }

  private async init() {
    if ('loading' in HTMLImageElement.prototype && !this.svgSrc && this.loading === 'lazy') {
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
        threshold: this.observerThreshold,
      });

      this.observer.observe(this.el.shadowRoot.host);

      resolve();
    });
  }

  /**
   * This component also export an async method lazyLoad() in case you would like to trigger "manually" the loading of the image
   */
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

      if (this.customLoader) {
        this.customLoad.emit({
          imgElement: img,
          imgSrc: this.imgSrc,
          imgSrcSet: this.imgSrcSet,
        });

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
    const hostClass: string = this.imgLoaded || this.svgContent ? 'loaded' : '';

    if (this.svgContent) {
      return (
        <Host class={hostClass}>
          <div innerHTML={this.svgContent} class="svg-container"></div>
        </Host>
      );
    } else {
      return <Host class={hostClass}>{this.renderImage()}</Host>;
    }
  }

  private renderImage() {
    // prettier-ignore
    // @ts-ignore
    return <img alt={this.imgLoaded ? (this.imgAlt ? this.imgAlt : this.imgSrc) : ''} loading={this.loading} sizes={this.imgSizes ? this.imgSizes : undefined} intrinsicsize={this.intrinsicsize}
                width={this.imgWidth} height={this.imgHeight}
                onLoad={() => this.imgLoaded = true} onError={() => this.loadError()}/>
  }
}
