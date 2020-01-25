import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host} from '@stencil/core';

import {isMobile} from '@deckdeckgo/utils';
import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

enum DeckdeckgoSlideCodeAction {
  SWIPE,
  SCROLL
}

@Component({
  tag: 'deckgo-slide-code',
  styleUrl: 'deckdeckgo-slide-code.scss',
  shadow: true
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

  @State()
  private mobile: boolean = false;

  private action: DeckdeckgoSlideCodeAction = DeckdeckgoSlideCodeAction.SWIPE;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  componentWillLoad() {
    this.mobile = isMobile();
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    await this.moveSlots();

    await this.showInfo();
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

  private showInfo(): Promise<void> {
    return new Promise<void>((resolve) => {
      // Only on mobile devices
      if (isMobile()) {
        const info: HTMLElement = this.el.querySelector("[slot='info']");

        if (info) {
          info.classList.add('deckgo-show-info');
        }
      }

      resolve();
    });
  }

  private hideInfo(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      // Only on mobile devices
      if (isMobile()) {
        const info: HTMLElement = this.el.querySelector("[slot='info']");

        if (info && info.classList.contains('deckgo-show-info')) {
          info.classList.remove('deckgo-show-info');
          info.style.setProperty('pointer-events', 'none');

          resolve(true);
          return;
        }
      }

      resolve(false);
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

  private switchAction(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.mobile) {
        // Scrolling is allowed on not mobile devices
        resolve();
        return;
      }

      const infoRemoved: boolean = await this.hideInfo();
      if (infoRemoved) {
        // On the first click, we just want to hide the info
        resolve();
        return;
      }

      this.action = this.action === DeckdeckgoSlideCodeAction.SWIPE ? DeckdeckgoSlideCodeAction.SCROLL : DeckdeckgoSlideCodeAction.SWIPE;

      this.scrolling.emit(this.action === DeckdeckgoSlideCodeAction.SCROLL);

      if (this.action === DeckdeckgoSlideCodeAction.SCROLL) {
        this.unlockScroll();
      } else {
        this.lockScroll();
      }

      resolve();
    });
  }

  private lockScroll() {
    const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-code-container');
    container.style.setProperty('overflow-y', 'hidden');
  }

  private unlockScroll() {
    const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-code-container');
    container.style.setProperty('overflow-y', 'auto');
  }

  // DeckDeckGo
  render() {
    let containerStyle: string = 'deckgo-slide-code-container';
    if (this.mobile) {
      containerStyle += ' deckgo-slide-code-container-mobile';
    }

    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide" onClick={() => this.switchAction()}>
          <slot name="title"></slot>
          <div class={containerStyle}>
            <deckgo-highlight-code
              src={this.src}
              anchor={this.anchor}
              anchorZoom={this.anchorZoom}
              hideAnchor={this.hideAnchor}
              language={this.language}></deckgo-highlight-code>
          </div>
          <slot name="code"></slot>
          <slot name="info"></slot>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
        </div>
      </Host>
    );
  }
}
