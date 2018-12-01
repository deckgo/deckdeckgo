import {Component, Element, Event, EventEmitter, Method, Prop, State} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-slide-youtube',
  styleUrl: 'deckdeckgo-slide-youtube.scss',
  shadow: true
})
export class DeckdeckgoSlideYoutube implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() src: string;
  @Prop() width: number;
  @Prop() height: number;

  @Prop() fullSize: boolean = false;

  @State() videoWidth: number;
  @State() videoHeight: number;

  async componentDidLoad() {
    await DeckdeckgoSlideUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    this.initWindowResize();

    await this.initSize();
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  lazyLoadImages(): Promise<void> {
    return DeckdeckgoUtils.lazyLoadImages(this.el);
  }

  private initSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.fullSize) {
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-youtube-container');

        if (container) {
          this.videoWidth = container.clientWidth;
          this.videoHeight = container.clientHeight;
        } else {
          this.videoWidth = this.width;
          this.videoHeight = this.height;
        }
      } else {
        this.videoWidth = this.width;
        this.videoHeight = this.height;
      }

      resolve();
    });
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', DeckdeckgoUtils.debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();

    const element: HTMLDeckgoYoutubeElement = this.el.shadowRoot.querySelector('deckgo-youtube');

    if (element) {
      await element.updateIFrame(this.videoWidth, this.videoHeight);
    }
  };

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-youtube-container">
        {this.renderVideo()}
      </div>
    </div>
  }

  private renderVideo() {
    if (!this.videoWidth || !this.videoHeight) {
      return undefined;
    } else {
      return <deckgo-youtube src={this.src} width={this.videoWidth} height={this.videoHeight}></deckgo-youtube>
    }
  }

  hostData() {
    return {
      class: {
        'deckgo-slide-container': true
      }
    }
  }

}
