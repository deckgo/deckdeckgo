import {Component, Element, Listen, Method, Prop, State} from '@stencil/core';

interface DeltaX {
  slider: HTMLElement
  swipeLeft: boolean;
  deltaX: number;
}

@Component({
  tag: 'deckgo-deck',
  styleUrl: 'deckdeckgo-deck.scss',
  shadow: true
})
export class DeckdeckgoDeck {

  @Element() el: HTMLElement;

  @Prop() keyboard: boolean = true;

  @Prop() pager: boolean = true;
  @Prop() pagerPercentage: boolean = true;

  private startX: number = null;
  private deckTranslateX: number = 0;
  private autoSwipeRatio: number = 10;

  private blockSlide: boolean = false;

  @State()
  private activeIndex: number = 0;

  @State()
  private length: number = 0;

  async componentDidLoad() {
    this.initWindowResize();
    this.initKeyboardAssist();

    const lazyLoadImagesFirstSlide = this.lazyLoadImages(0);
    const lazyLoadImagesSecondSlide = this.lazyLoadImages(1);

    await Promise.all([lazyLoadImagesFirstSlide, lazyLoadImagesSecondSlide]);
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', async () => {
        await this.slideTo(this.activeIndex);
      });
    }
  }

  private initKeyboardAssist() {
    if (this.keyboard) {
      document.addEventListener('keydown', async (e) => {
        if (e.defaultPrevented) {
          return;
        }

        if (e.key === 'ArrowLeft') {
          e.preventDefault();
          await this.slidePrev();
        } else if (e.key === 'ArrowRight') {
          e.preventDefault();
          await this.slideNext();
        }
      });
    }
  }

  /* BEGIN: Handle swipe */

  @Listen('mousedown', {passive: true})
  mousedown($event: MouseEvent) {
    this.start($event);
  }

  @Listen('touchstart', {passive: true})
  touchstart($event: TouchEvent) {
    this.start($event);
  }

  @Listen('mouseup', {passive: true})
  async mouseup($event: MouseEvent) {
    await this.stop($event);
  }

  @Listen('touchend', {passive: true})
  async touchend($event: TouchEvent) {
    await this.stop($event);
  }

  @Listen('mousemove', {passive: true})
  async mousemove($event: MouseEvent) {
    await this.move($event);
  }

  @Listen('touchmove', {passive: true})
  async touchmove($event: TouchEvent) {
    await this.move($event);
  }

  @Listen('dblclick', {passive: true})
  async dblclick() {
    this.startX = null;
  }

  @Listen('scrolling')
  scrolling() {
    this.blockSlide = true;
  }

  private unify(e) {
    return e.changedTouches ? e.changedTouches[0] : e;
  }

  private start(e: Event) {
    this.startX = this.unify(e).clientX;
  }

  private async move(e: Event) {

    if (this.blockSlide) {
      return;
    }

    const deltaX: DeltaX = await this.getDeltaX(e);

    if (!deltaX) {
      return;
    }

    let transformX: number = deltaX.swipeLeft ? this.deckTranslateX - deltaX.deltaX : this.deckTranslateX + deltaX.deltaX;

    deltaX.slider.style.setProperty('--transformX', transformX + 'px');
    deltaX.slider.style.setProperty('--transformXDuration', '0ms');
  }

  private async stop(e: Event) {
    if (this.blockSlide) {
      this.blockSlide = false;
      return;
    }

    const deltaX: DeltaX = await this.getDeltaX(e);

    await this.swipeSlide(deltaX);

    this.startX = null;
  }

  private swipeSlide(deltaX: DeltaX): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!deltaX || !window) {
        resolve();
        return;
      }


      const couldSwipeLeft: boolean = deltaX.swipeLeft && this.activeIndex < this.length - 1;
      const couldSwipeRight: boolean = !deltaX.swipeLeft && this.activeIndex > 0;

      if (couldSwipeLeft || couldSwipeRight) {
        const windowWidth: number = window.innerWidth;
        if (deltaX.deltaX > (windowWidth / this.autoSwipeRatio)) {
          this.deckTranslateX = deltaX.swipeLeft ? this.deckTranslateX - windowWidth : this.deckTranslateX + windowWidth;

          if (deltaX.swipeLeft) {
            this.activeIndex++;
          } else {
            this.activeIndex--;
          }
        }
      }

      await this.doSwipeSlide(deltaX.slider);

      // We want to lazy load the next slide images in the background
      await this.lazyLoadImages(this.activeIndex + 1);

      resolve();
    });
  }

  private doSwipeSlide(slider: HTMLElement, speed?: number | undefined): Promise<void> {
    return new Promise<void>((resolve) => {
      slider.style.setProperty('--transformX', this.deckTranslateX + 'px');
      slider.style.setProperty('--transformXDuration', '' + (!isNaN(speed) ? speed : 300) + 'ms');

      this.startX = null;

      resolve();
    });
  }

  private getDeltaX(e): Promise<DeltaX> {
    return new Promise<DeltaX>((resolve) => {
      if (!this.startX) {
        resolve(null);
        return;
      }

      const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

      if (!slider) {
        resolve(null);
        return;
      }

      const currentX: number = this.unify(e).clientX;

      if (this.startX === currentX) {
        resolve(null);
        return;
      }

      const swipeLeft: boolean = this.startX > currentX;

      resolve({
        slider: slider,
        swipeLeft: swipeLeft,
        deltaX: swipeLeft ? (this.startX - currentX) : (currentX - this.startX)
      })
    });
  }

  /* END: Handle swipe */

  /* BEGIN: Slide length and active index */

  @Listen('slideDidLoad')
  slideDidLoad() {
    this.length++;
  }

  @Method()
  isBeginning(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(this.activeIndex === 0);
    });
  }

  @Method()
  isEnd(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(this.activeIndex === this.length - 1);
    });
  }

  @Method()
  getActiveIndex(): Promise<number> {
    return new Promise<number>((resolve) => {
      resolve(this.activeIndex);
    });
  }

  @Method()
  getLength(): Promise<number> {
    return new Promise<number>((resolve) => {
      resolve(this.length);
    });
  }

  /* END: Slide length and active index */

  /* BEGIN: Manual sliding */

  @Method()
  async slideNext() {
    await this.slideNextPrev(true);
  }

  @Method()
  async slidePrev() {
    await this.slideNextPrev(false);
  }

  private async slideNextPrev(swipeLeft: boolean) {
    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider || !window) {
      return;
    }

    const couldSwipe: boolean = await this.couldSwipe(swipeLeft);

    // We might want first to show hide stuffs in the slide before swiping
    if (couldSwipe) {
      const deltaX: DeltaX = {
        slider: slider,
        swipeLeft: swipeLeft,
        deltaX: window.innerWidth
      };

      await this.swipeSlide(deltaX);
    }
  }

  private couldSwipe(swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');

      if (!slide) {
        // If we find no slide, we are cool something went wrong but the talk/show must go on
        resolve(true);
      } else {
        const result: boolean = await (slide as any).beforeSwipe(swipeLeft);
        resolve(result);
      }
    });
  }

  private lazyLoadImages(index: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

      if (slide) {
        await (slide as any).lazyLoadImages();
      }

      resolve();
    });
  }

  @Method()
  async slideTo(index: number, speed?: number | undefined) {
    if (index > this.length || index < 0) {
      return;
    }

    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider || !window) {
      return;
    }

    this.deckTranslateX = index * -1 * window.innerWidth;
    this.activeIndex = index;

    await this.lazyLoadImages(this.activeIndex);
    await this.doSwipeSlide(slider, speed);
  }

  /* END: Manual sliding */

  /* BEGIN: Full screen */

  @Method()
  toggleFullScreen(): Promise<void> {
    return new Promise<void>((resolve) => {
      const doc = window.document;
      const docEl = doc.documentElement;

      const requestFullScreen = docEl.requestFullscreen || (docEl as any).mozRequestFullScreen || docEl.webkitRequestFullScreen || (docEl as any).msRequestFullscreen;
      const cancelFullScreen = doc.exitFullscreen || (doc as any).mozCancelFullScreen || doc.webkitExitFullscreen || (doc as any).msExitFullscreen;

      if (!doc.fullscreenElement && !(doc as any).mozFullScreenElement && !doc.webkitFullscreenElement && !(doc as any).msFullscreenElement) {
        requestFullScreen.call(docEl);
      } else {
        cancelFullScreen.call(doc);
      }

      resolve();
    });
  }

  /* END: Full screen */

  /* BEGIN: Full screen */

  @Method()
  doPrint(): Promise<void> {
   return new Promise<void>(async (resolve) => {
     if (window) {
       await this.lazyLoadAllImages();

       window.print();
     }

     resolve();
   });
  }

  private lazyLoadAllImages(): Promise<any[]> {
    const promises = [];

    for (let i = 0; i < this.length; i++) {
      promises.push(this.lazyLoadImages(i));
    }

    return Promise.all(promises);
  }

  /* END: Full screen */

  render() {
    return [
      <div class="deckgo-deck"><slot/></div>,
      <div class="deckgo-pager">{this.pager ? <deckgo-pager active-index={this.activeIndex} length={this.length} percentage={this.pagerPercentage}></deckgo-pager> : ''}</div>
    ]
  }

}
