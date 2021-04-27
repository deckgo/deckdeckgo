import {Component, Element, Event, EventEmitter, Method, Prop, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

import {DeckdeckgoHighlightCodeTerminal, DeckdeckgoHighlightCodeCarbonTheme} from '@deckdeckgo/highlight-code';

/**
 * @slot title - A title
 * @slot code - A block of code to highlight if src is not used
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-code',
  styleUrl: 'deckdeckgo-slide-code.scss',
  shadow: true,
})
export class DeckdeckgoSlideCode implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event() slideDidLoad: EventEmitter<void>;

  /**
   * An event triggered when the code is scrolled
   */
  @Event()
  scrolling: EventEmitter<boolean>;

  /**
   * The web url to the source code you would like to showcase
   */
  @Prop() src: string;

  /**
   * The anchor identifier which will be use to find the next anchor to scroll too using findNextAnchor()
   */
  @Prop() anchor: string = '// DeckDeckGo';
  /**
   * The anchor identifier which will be use to find the next anchor to zoom inside your code using findNextAnchor()
   */
  @Prop() anchorZoom: string = '// DeckDeckGoZoom';
  /**
   * Set this attribute to false in case you would like to actually display the anchor value too
   */
  @Prop() hideAnchor: boolean = true;

  /**
   * Define the language to be used for the syntax highlighting. The list of supported languages is defined by Prism.js
   */
  @Prop({reflect: true}) language: string = 'javascript';

  /**
   * Present the code in a stylish "windowed" card
   */
  @Prop()
  terminal: DeckdeckgoHighlightCodeTerminal = DeckdeckgoHighlightCodeTerminal.CARBON;
  /**
   * The theme of the selected terminal (applied only in case of carbon)
   */
  @Prop()
  theme: DeckdeckgoHighlightCodeCarbonTheme = DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

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

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    await this.moveSlots();
  }

  private moveSlots(): Promise<void> {
    return new Promise<void>((resolve) => {
      const code: HTMLElement = this.el.querySelector("[slot='code']");

      const codeComponent = this.el.shadowRoot.querySelector('deckgo-highlight-code');

      if (codeComponent && code) {
        codeComponent.appendChild(code);
      }

      resolve();
    });
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const couldSwipe: boolean = await this.scrollToNext(_enter);

      if (couldSwipe) {
        await this.zoomCode(false);
      }

      resolve(couldSwipe);
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  private scrollToNext(enter: boolean): Promise<boolean> {
    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-highlight-code');

    if (element && 'findNextAnchor' in element) {
      return new Promise<boolean>(async (resolve) => {
        const nextAnchor: any = await (element as any).findNextAnchor(enter);

        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-code-container');

        if (nextAnchor && container) {
          const previousScrollTop: number = container.scrollTop;
          container.scrollTop = nextAnchor.offsetTop;

          if ('zoomCode' in element) {
            await (element as any).zoomCode(nextAnchor.hasLineZoom);
          }

          resolve(nextAnchor.offsetTop === 0 && previousScrollTop === 0);
        } else {
          resolve(true);
        }
      });
    } else {
      return new Promise<boolean>((resolve) => {
        resolve(true);
      });
    }
  }

  private zoomCode(zoom: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-highlight-code');

      if (element && 'zoomCode' in element) {
        await (element as any).zoomCode(zoom);
      }

      const title: HTMLElement = this.el.querySelector("[slot='title']");
      if (title) {
        title.style.setProperty('opacity', zoom ? '0' : '1');
      }

      resolve();
    });
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return lazyLoadContent(this.el);
  }

  // DeckDeckGo
  render() {
    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <div class="deckgo-slide-code-container">
            <deckgo-highlight-code
              src={this.src}
              anchor={this.anchor}
              anchorZoom={this.anchorZoom}
              hideAnchor={this.hideAnchor}
              terminal={this.terminal}
              theme={this.theme}
              language={this.language}></deckgo-highlight-code>
          </div>
          <slot name="code"></slot>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }
}
