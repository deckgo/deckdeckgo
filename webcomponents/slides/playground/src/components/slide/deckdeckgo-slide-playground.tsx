import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlideResize, hideLazyLoadImages, lazyLoadComponentContent, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

import {DeckdeckgoPlaygroundTheme} from '../../declarations/deckdeckgo-playground-theme';

@Component({
  tag: 'deckgo-slide-playground',
  styleUrl: 'deckdeckgo-slide-playground.scss',
  shadow: true,
})
export class DeckdeckgoSlidePlayground implements DeckdeckgoSlideResize {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true}) src: string;
  @Prop({reflect: true}) theme: DeckdeckgoPlaygroundTheme = DeckdeckgoPlaygroundTheme.DEFAULT;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() allowFullscreen: boolean = true;

  @State() videoWidth: number;
  @State() videoHeight: number;

  @State() frameTitle: string;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    await this.initFrameTitle();

    await this.initSize();

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return Promise.resolve(true);
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await lazyLoadContent(this.el);

      await lazyLoadComponentContent(this.el, 'deckgo-playground');

      await this.initSize();

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
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-playground-container');

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
      window.addEventListener('resize', debounce(this.onResizeContent, 500));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();
  };

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
          <div class="deckgo-playground-container">{this.renderPlayground()}</div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }

  private renderPlayground() {
    return (
      <deckgo-playground
        src={this.src}
        width={this.videoWidth}
        height={this.videoHeight}
        frame-title={this.frameTitle}
        theme={this.theme}
        allowFullscreen={this.allowFullscreen}></deckgo-playground>
    );
  }
}
