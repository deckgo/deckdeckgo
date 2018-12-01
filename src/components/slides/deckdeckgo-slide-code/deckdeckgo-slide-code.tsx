import {Component, Element, Event, EventEmitter, Method, Prop, Watch} from '@stencil/core';

import Prism from 'prismjs';

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

  private anchorOffsetTop: number = 0;

  private startX: number = null;
  private action: DeckdeckgoSlideCodeAction = null;

  async componentDidLoad() {
    await DeckdeckgoSlideUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    await this.loadLanguage();

    await this.fetchCode();

    await this.parseSlottedCode();
  }

  @Watch('language')
  loadLanguage(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document || this.language === 'javascript') {
        resolve();
        return;
      }

      const scripts = document.querySelector('[deckdeckgo-prims=\'' + this.language + '\']');

      if (scripts) {
        // Already loaded
        await this.parseSlottedCode();
        resolve();
        return;
      }

      const script = document.createElement('script');

      script.onload = async () => {
        await this.parseSlottedCode();
      };

      script.src = 'https://unpkg.com/prismjs@latest/components/prism-' + this.language + '.js';
      script.setAttribute('deckdeckgo-prims', this.language);
      script.defer = true;

      document.head.appendChild(script);

      resolve();
    });
  }

  private parseSlottedCode(): Promise<void> {
    const code: HTMLElement = this.el.querySelector('[slot=\'code\']');
    if (code) {
      return this.parseCode(code.innerHTML);
    } else {
      return new Promise<void>((resolve) => {
        resolve();
      })
    }
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

  // DeckDeckGo
  private scrollToNext(swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = this.el.shadowRoot.querySelectorAll('span.deckgo-code-anchor');

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
        container.style.setProperty('--zoom-code', zoom ? '1.3' : '1');
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
    return DeckdeckgoUtils.lazyLoadImages(this.el);
  }

  // DeckDeckGoZoom
  async fetchCode() {
    if (!this.src) {
      return;
    }

    let fetchedCode: string;
    try {
      const response: Response = await fetch(this.src);
      fetchedCode = await response.text();

      await this.parseCode(fetchedCode);
    } catch (e) {
      // Prism might not be able to parse the code for the selected language
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-code-container');

      if (container && fetchedCode) {
        container.children[0].innerHTML = fetchedCode;
      }
    }
  }

  private parseCode(code: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-code-container');

      if (container) {
        try {
          const highlightedCode: string = Prism.highlight(code, Prism.languages[this.language]);

          container.children[0].innerHTML = highlightedCode;

          await this.addAnchors();
        } catch (err) {
          // The highlighting might fail if the language is not yet defined
        }
      }

      resolve();
    });
  }

  private addAnchors(): Promise<void> {
    return new Promise<void>((resolve) => {
      const elements: NodeListOf<HTMLElement> = this.el.shadowRoot.querySelectorAll('span.comment');

      if (elements) {
        const elementsArray: HTMLElement[] = Array.from(elements);

        const anchors: HTMLElement[] = elementsArray.filter((element: HTMLElement) => {
          return this.hasLineAnchor(element.innerHTML);
        });

        if (anchors) {
          anchors.forEach((anchor: HTMLElement) => {
            anchor.classList.add('deckgo-code-anchor');

            if (this.hideAnchor) {
              anchor.classList.add('deckgo-code-anchor-hidden');
            }
          });
        }
      }

      resolve();
    });
  }

  private hasLineAnchor(line: string): boolean {
    return line && this.anchor &&
      line.indexOf('@Prop') === -1 &&
      line.split(' ').join('').indexOf(this.anchor.split(' ').join('')) > -1;
  }

  // DeckDeckGo
  render() {
    return <div class="deckgo-slide"
                onTouchStart={(event: TouchEvent) => this.touchScrollStart(event)}
                onTouchMove={(event: TouchEvent) => this.touchScrollMove(event)}
                onTouchEnd={() => this.touchScrollEnd()}>
      <slot name="title"></slot>
      <div class="deckgo-code-container" onScroll={() => this.emitScrolling()}>
        <code></code>
      </div>
      <slot name="code"></slot>
    </div>;
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
    const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-code-container');
    container.style.setProperty('overflow-y', 'hidden');
  }

  private unlockScroll() {
    const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-code-container');
    container.style.removeProperty('overflow-y');
  }

  private emitScrolling() {
    if (this.action === DeckdeckgoSlideCodeAction.SCROLL) {
      this.scrolling.emit();
    }
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
