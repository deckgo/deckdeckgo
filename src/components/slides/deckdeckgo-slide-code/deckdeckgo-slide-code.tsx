import {Component, Element, Event, EventEmitter, Method, Prop, State} from '@stencil/core';

import {DeckdeckgoSlide, DeckDeckGoSlideUtils} from '../deckdeckgo-slide';

@Component({
  tag: 'deckgo-slide-code',
  styleUrl: 'deckdeckgo-slide-code.scss',
  shadow: true
})
export class DeckdeckgoSlideCode implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() srcFile: string;

  @Prop() anchor: string = '// DeckDeckGo';
  @Prop() anchorZoom: string = '// DeckDeckGoZoom';
  @Prop() hideAnchor: boolean = true;

  private anchorOffsetTop: number = 0;

  @State()
  private code: string[] = [];

  async componentDidLoad() {
    this.slideDidLoad.emit();

    await this.fetchCode();
  }

  @Method()
  beforeSwipe(swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const couldSwipe: boolean = await this.scrollToNext(swipeLeft);

      if (couldSwipe) {
        await this.zoomCode(false);
      }

      resolve(couldSwipe);
    });
  }

  // DeckDeckGo
  private scrollToNext(swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = this.el.shadowRoot.querySelectorAll('div.deckgo-code-anchor');

      let couldSwipe: boolean = true;

      if (elements) {
        const elementsArray: HTMLElement[] = swipeLeft ? Array.from(elements) : Array.from(elements).reverse();

        const anchor: HTMLElement = elementsArray.find((element: HTMLElement) => {
          return swipeLeft ? element.offsetTop > this.anchorOffsetTop : element.offsetTop < this.anchorOffsetTop;
        });

        if (anchor) {
          anchor.scrollIntoView({block: 'start', behavior: 'smooth'});
          couldSwipe = false;
          this.anchorOffsetTop = anchor.offsetTop;

          await this.zoomCode(this.hasLineZoom(anchor.textContent));
        } else if (!swipeLeft) {
          const elementCode: HTMLElement = this.el.shadowRoot.querySelector('code');

          if (elementCode && elementCode.firstElementChild) {
            elementCode.firstElementChild.scrollIntoView({block: 'center', behavior: 'smooth'});
            this.anchorOffsetTop = 0;
          }
        }
      } else {
        this.anchorOffsetTop = 0;
      }

      if (this.anchorOffsetTop === 0) {
        await this.zoomCode(false);
      }

      resolve(couldSwipe);
    });
  }

  private zoomCode(zoom: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-code-container');

      if (container) {
        container.style.setProperty('--zoom-code', zoom ? '2' : '1');
      }

      const title: HTMLElement = this.el.querySelector('[slot=\'title\']');
      if (title) {
        title.style.setProperty('opacity', zoom ? '0' : '1');
      }

      resolve();
    });
  }

  @Method()
  lazyLoadImages(): Promise<void> {
    return DeckDeckGoSlideUtils.lazyLoadImages(this.el);
  }

  // DeckDeckGoZoom
  async fetchCode() {
    if (!this.srcFile) {
      return;
    }

    try {
      const response: Response = await fetch(this.srcFile);
      const fetchedCode: string = await response.text();

      this.code = await this.splitCode(fetchedCode);

    } catch (e) {
      this.code = null;
    }
  }

  private splitCode(fetchedCode: string): Promise<string[]> {
    return new Promise<string[]>((resolve) => {
      if (!fetchedCode) {
        resolve([]);
      } else {
        resolve(fetchedCode.split('\n'));
      }
    });
  }

  // DeckDeckGo
  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-code-container"><code>{this.renderCode()}</code></div>
    </div>;
  }

  private renderCode() {
    return (
      this.code.map((line: string) => {
        return <div
          class={this.hasLineAnchor(line) ? 'deckgo-code-anchor' + (this.hideAnchor ? ' ' + 'deckgo-code-anchor-hidden' : '') : undefined}
        >{line}</div>
      })
    );
  }

  private hasLineAnchor(line: string): boolean {
    return line && this.anchor &&
      line.indexOf('@Prop') === -1 &&
      line.split(' ').join('').indexOf(this.anchor.split(' ').join('')) > -1;
  }

  private hasLineZoom(line: string): boolean {
    return line && this.anchorZoom &&
      line.indexOf('@Prop') === -1 &&
      line.split(' ').join('').indexOf(this.anchorZoom.split(' ').join('')) > -1;
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
