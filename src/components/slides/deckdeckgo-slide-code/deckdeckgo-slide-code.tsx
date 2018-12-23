import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

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

  @Event() scrolling: EventEmitter<void>;

  @Prop() src: string;

  @Prop() anchor: string = '// DeckDeckGo';
  @Prop() anchorZoom: string = '// DeckDeckGoZoom';
  @Prop() hideAnchor: boolean = true;

  @Prop() language: string = 'javascript';

  private startX: number = null;
  private action: DeckdeckgoSlideCodeAction = null;

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    await this.moveSlots();
  }

  private moveSlots(): Promise<void> {
    return new Promise<void>((resolve) => {
      const code: HTMLElement = this.el.querySelector('[slot=\'code\']');

      const codeComponent = this.el.shadowRoot.querySelector('deckgo-code');

      if (codeComponent && code) {
        codeComponent.appendChild(code);
      }

      resolve();
    });
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const couldSwipe: boolean = await this.scrollToNext(_swipeLeft);

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

  private scrollToNext(swipeLeft: boolean): Promise<boolean> {
    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-code');

    if (element) {
      return (element as any).scrollToNext(swipeLeft);
    } else {
      return new Promise<boolean>((resolve) => {
        resolve(true);
      });
    }
  }

  private zoomCode(zoom: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-code');

      if (element) {
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

  private touchScrollStart(event: TouchEvent) {
    this.startX = DeckdeckgoUtils.unifyEvent(event).clientX;
  }

  private touchScrollMove(event: TouchEvent) {
    if (this.action) {
      return;
    }

    const currentX: number = DeckdeckgoUtils.unifyEvent(event).clientX;

    const swipeLeft: boolean = this.startX > currentX;
    const swipeRight: boolean = this.startX < currentX;

    this.action = swipeLeft || swipeRight ? DeckdeckgoSlideCodeAction.SWIPE : DeckdeckgoSlideCodeAction.SCROLL;

    if (!swipeLeft && !swipeRight) {
      this.scrolling.emit();
      this.unlockScroll();
    } else {
      this.lockScroll();
    }
  }

  private touchScrollEnd() {
    this.action = null;

    this.unlockScroll();
  }

  private lockScroll() {
    const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-code-container');
    container.style.setProperty('overflow-y', 'hidden');
  }

  private unlockScroll() {
    const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-code-container');
    container.style.removeProperty('overflow-y');
  }

  private emitScrolling() {
    if (this.action === DeckdeckgoSlideCodeAction.SCROLL) {
      this.scrolling.emit();
    }
  }

  // DeckDeckGo
  render() {
    return <div class="deckgo-slide"
                onTouchStart={(event: TouchEvent) => this.touchScrollStart(event)}
                onTouchMove={(event: TouchEvent) => this.touchScrollMove(event)}
                onTouchEnd={() => this.touchScrollEnd()}>
      <slot name="title"></slot>
      <div class="deckgo-slide-code-container" onScroll={() => this.emitScrolling()}>
        <deckgo-code src={this.src} anchor={this.anchor} anchorZoom={this.anchorZoom} hideAnchor={this.hideAnchor} language={this.language}></deckgo-code>
      </div>
      <slot name="code"></slot>
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
