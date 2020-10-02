import {Component, Element, Event, EventEmitter, Method, Prop, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

import {DeckdeckgoHighlightCodeTerminal, DeckdeckgoHighlightCodeCarbonTheme} from '@deckdeckgo/highlight-code';

@Component({
  tag: 'deckgo-slide-code',
  styleUrl: 'deckdeckgo-slide-code.scss',
  shadow: true,
})
export class DeckdeckgoSlideCode implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Event() scrolling: EventEmitter<boolean>;

  @Prop() src: string;

  @Prop() anchor: string = '// DeckDeckGo';
  @Prop() anchorZoom: string = '// DeckDeckGoZoom';
  @Prop() hideAnchor: boolean = true;

  @Prop() language: string = 'javascript';

  @Prop() terminal: DeckdeckgoHighlightCodeTerminal = DeckdeckgoHighlightCodeTerminal.CARBON;
  @Prop() theme: DeckdeckgoHighlightCodeCarbonTheme = DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

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
