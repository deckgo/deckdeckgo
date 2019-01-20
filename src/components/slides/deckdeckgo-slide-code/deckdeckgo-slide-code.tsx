import {Component, Element, Event, EventEmitter, Method, Prop, State} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

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

  componentWillLoad() {
    this.mobile = DeckdeckgoSlideUtils.isMobile();
  }

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    await this.moveSlots();
  }

  private moveSlots(): Promise<void> {
    return new Promise<void>((resolve) => {
      const code: HTMLElement = this.el.querySelector('[slot=\'code\']');

      const codeComponent = this.el.shadowRoot.querySelector('deckgo-highlight-code');

      if (codeComponent && code) {
        codeComponent.appendChild(code);
      }

      resolve();
    });
  }

  @Method()
  beforeSwipe(_enter: boolean): Promise<boolean> {
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
    return DeckdeckgoSlideUtils.afterSwipe();
  }

  private scrollToNext(enter: boolean): Promise<boolean> {
    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-highlight-code');

    if (element && element.hasOwnProperty('findNextAnchor')) {
      return new Promise<boolean>(async (resolve) => {
        const nextAnchor: any = await (element as any).findNextAnchor(enter);

        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-code-container');

        if (nextAnchor && container) {
          const previousScrollTop: number = container.scrollTop;
          container.scrollTop = nextAnchor.offsetTop;

          if (element.hasOwnProperty('zoomCode')) {
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

      if (element && element.hasOwnProperty('zoomCode')) {
        await (element as any).zoomCode(zoom);
      }

      const title: HTMLElement = this.el.querySelector('[slot=\'title\']');
      if (title) {
        title.style.setProperty('opacity', zoom ? '0' : '1');
      }

      resolve();
    });
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoSlideUtils.lazyLoadContent(this.el);
  }

  private switchAction(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.mobile) {
        // Scrolling is allowed on not mobile devices
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

    return <div class="deckgo-slide"
                onClick={() => this.switchAction()}>
      <slot name="title"></slot>
      <div class={containerStyle}>
        <deckgo-highlight-code src={this.src} anchor={this.anchor} anchorZoom={this.anchorZoom} hideAnchor={this.hideAnchor} language={this.language}></deckgo-highlight-code>
      </div>
      <slot name="code"></slot>
      <slot name="notes"></slot>
    </div>;
  }

  // DeckDeckGoZoom
  hostData() {
    return {
      class: {
        'deckgo-slide-container': true
      }
    }
  }

}
