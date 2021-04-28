import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlideResize, hideLazyLoadImages, lazyLoadComponentContent, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

import {DeckdeckgoPlaygroundTheme} from '../../declarations/deckdeckgo-playground-theme';

/**
 * @slot title - A title
 * @slot content - Some more optional content
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-playground',
  styleUrl: 'deckdeckgo-slide-playground.scss',
  shadow: true,
})
export class DeckdeckgoSlidePlayground implements DeckdeckgoSlideResize {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event() slideDidLoad: EventEmitter<void>;

  /**
   * The full link to your Pen, Fiddle oder WebComponents.dev. The component will take care of converting the link to an embeddable one
   */
  @Prop({reflect: true}) src: string;
  /**
   * The theming option if it can be applied respectivelly if supported by the third party playground, otherwise, 'default'
   */
  @Prop({reflect: true}) theme: DeckdeckgoPlaygroundTheme = DeckdeckgoPlaygroundTheme.DEFAULT;

  /**
   * Per default the playground width will be calculated according the content size available. Using this option you would be able to define your own width.
   */
  @Prop() width: number;
  /**
   * Per default the playground height will be calculated according the content size available. Using this option you would be able to define your own height.
   */
  @Prop() height: number;
  /**
   * Allow toggle to fullscreen
   */
  @Prop() allowFullscreen: boolean = true;

  /**
   * If you define a background for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customBackground: boolean = false;

  /**
   * If you provide actions for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customActions: boolean = false;

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
