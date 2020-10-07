import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlidePlay, hideLazyLoadImages, lazyLoadContent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-youtube',
  styleUrl: 'deckdeckgo-slide-youtube.scss',
  shadow: true,
})
export class DeckdeckgoSlideYoutube implements DeckdeckgoSlidePlay {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true}) src: string;
  @Prop() width: number;
  @Prop() height: number;

  @Prop() allowFullscreen: boolean = true;

  @State() videoWidth: number;
  @State() videoHeight: number;

  @State() frameTitle: string;

  private isPlaying: boolean = false;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    await this.initFrameTitle();

    await this.initSize();

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true);
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // Stop video after swipe to prev or next slide
      await this.playPauseVideo(false);
      resolve();
    });
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await lazyLoadContent(this.el);

      await this.initSize();
      await this.resizeVideoContent();

      resolve();
    });
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  async play() {
    await this.playPauseVideo(true);
  }

  @Method()
  async pause() {
    await this.playPauseVideo(false);
  }

  @Method()
  async toggle() {
    await this.playPauseVideo(!this.isPlaying);
  }

  private playPauseVideo(play: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const element: any = this.el.shadowRoot.querySelector('deckgo-youtube');

      if (!element) {
        resolve();
        return;
      }

      if (play) {
        await element.play();
      } else {
        await element.pause();
      }

      this.isPlaying = play;

      resolve();
    });
  }

  private initFrameTitle(): Promise<string> {
    return new Promise<string>((resolve) => {
      const title: HTMLElement = this.el.querySelector("[slot='title']");

      if (title) {
        this.frameTitle = title.innerHTML;
      }

      resolve();
    });
  }

  private initSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      // If width and height, use them otherwise full size
      if (this.width > 0 && this.height > 0) {
        this.videoWidth = this.width;
        this.videoHeight = this.height;
      } else {
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-youtube-container');

        if (container) {
          this.videoWidth = container.clientWidth;
          this.videoHeight = container.clientHeight;
        }
      }

      resolve();
    });
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();

    await this.resizeVideoContent();
  };

  private async resizeVideoContent() {
    const element: any = this.el.shadowRoot.querySelector('deckgo-youtube');

    if (element) {
      await element.updateIFrame(this.videoWidth, this.videoHeight);
    }
  }

  @Method()
  resizeContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.onResizeContent();

      resolve();
    });
  }

  render() {
    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <slot name="content"></slot>
          <div class="deckgo-youtube-container">{this.renderVideo()}</div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }

  private renderVideo() {
    return (
      <deckgo-youtube
        src={this.src}
        width={this.videoWidth}
        height={this.videoHeight}
        frame-title={this.frameTitle}
        allowFullscreen={this.allowFullscreen}></deckgo-youtube>
    );
  }
}
