import {Component, Element, Listen, Method, Prop, State, Event, EventEmitter} from '@stencil/core';

import {DeckdeckgoSlideDefinition} from 'deckdeckgo-types';

import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';
import {DeckdeckgoDeckBackgroundUtils} from '../../utils/deckdeckgo-deck-background-utils';

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

  @Prop() pagerPercentage: boolean = true;

  @Prop() embedded: boolean = false;

  @Prop() cloneBackground: boolean = true;

  @State()
  private rtl: boolean = false;

  private startX: number = null;
  private deckTranslateX: number = 0;
  private autoSwipeRatio: number = 10;

  private block: boolean = false;

  @State()
  private activeIndex: number = 0;

  @State()
  private length: number = 0;

  @Event() slidesDidLoad: EventEmitter;
  @Event() slideNextDidChange: EventEmitter<number>;
  @Event() slidePrevDidChange: EventEmitter<number>;
  @Event() slideToChange: EventEmitter<number>;
  @Event() slideDrag: EventEmitter<number>;
  @Event() slideWillChange: EventEmitter<number>;

  private fullscreen: boolean = false;
  private cursorHidden: boolean = false;
  private idleMouseTimer: number;

  @Event() mouseInactivity: EventEmitter<boolean>;

  async componentWillLoad() {
    await this.initRtl();
  }

  async componentDidLoad() {
    await this.initSlideSize();

    this.initWindowResize();
    this.initKeyboardAssist();
  }

  private initRtl(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (document && document.documentElement) {
        const htmlDir: string = document.documentElement.getAttribute('dir');
        this.rtl = htmlDir && htmlDir === 'rtl';
      }

      resolve();
    });
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', DeckdeckgoUtils.debounce(async () => {
        await this.initSlideSize();
        await this.slideTo(this.activeIndex);

        const toggleFullscreen: boolean = DeckdeckgoUtils.isFullscreen();
        await this.hideOrClearMouseCursorTimer(toggleFullscreen);
        await this.showHideActionsSlot(toggleFullscreen);
      }, 100));
    }
  }

  @Method()
  initSlideSize(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

      if (!slider) {
        resolve();
        return;
      }

      if (!this.embedded) {
        await this.initSlideSizeStandard(slider);
      } else {
        await this.initSlideSizeEmbedded(slider);
      }

      resolve();
    });
  }

  private initSlideSizeStandard(slider: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!window || !screen) {
        resolve();
        return;
      }

      if (DeckdeckgoUtils.isIOS()) {
        slider.style.setProperty('--slide-width', '' + screen.width + 'px');
      } else {
        slider.style.setProperty('--slide-width', '' + window.innerWidth + 'px');
      }

      resolve();
    });
  }

  private initSlideSizeEmbedded(slider: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!slider.offsetParent) {
        resolve();
        return;
      }

      if (slider.offsetParent) {
        if (slider.offsetParent.clientWidth > 0) {
          slider.style.setProperty('--slide-width', '' + slider.offsetParent.clientWidth + 'px');
        }

        if (slider.offsetParent.clientHeight > 0) {
          slider.style.setProperty('--slide-height', '' + slider.offsetParent.clientHeight + 'px');
        }
      }

      resolve();
    });
  }

  private initKeyboardAssist() {
    if (document && this.keyboard) {
      document.addEventListener('keydown', this.keyboardAssist,{passive: true});
    }
  }

  @Method()
  toggleKeyboardAssist(state: boolean) {
    if (!document) {
      return;
    }

    if (!this.keyboard) {
      return;
    }

    if (state) {
      document.addEventListener('keydown', this.keyboardAssist,{passive: true});
    } else {
      // @ts-ignore
      document.removeEventListener('keydown', this.keyboardAssist, {passive: true});
    }
  }

  private keyboardAssist = async ($event: KeyboardEvent) => {
    if ($event.defaultPrevented) {
      return;
    }

    if ($event.key === 'ArrowLeft') {
      await this.slideNextPrev(false, true);
    } else if ($event.key === 'ArrowRight') {
      await this.slideNextPrev(true, true);
    }
  };

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
  scrolling($event: CustomEvent) {
    this.block = $event ? $event.detail : false;
  }

  @Listen('keypress')
  async keypress() {
    await this.clearMouseCursorTimer(true);
  }

  private start(e: Event) {
    this.startX = DeckdeckgoUtils.unifyEvent(e).clientX;
  }

  private async move(e: Event) {
    await this.clearMouseCursorTimer(true);

    if (this.block) {
      return;
    }

    const deltaX: DeltaX = await this.getDeltaX(e);

    if (!deltaX) {
      return;
    }

    const transformX: number = deltaX.swipeLeft ? this.deckTranslateX - deltaX.deltaX : this.deckTranslateX + deltaX.deltaX;

    deltaX.slider.style.setProperty('--transformX', transformX + 'px');
    deltaX.slider.style.setProperty('--transformXDuration', '0ms');

    this.slideDrag.emit(transformX);
  }

  private async stop(e: Event) {
    if (this.block) {
      return;
    }

    const deltaX: DeltaX = await this.getDeltaX(e);

    await this.swipeSlide(deltaX);

    this.startX = null;
  }

  private swipeSlide(deltaX: DeltaX, emitEvent: boolean = true): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!deltaX || !window) {
        resolve();
        return;
      }

      let couldSwipeLeft: boolean = deltaX.swipeLeft && this.activeIndex < this.length - 1;
      let couldSwipeRight: boolean = !deltaX.swipeLeft && this.activeIndex > 0;

      if (this.rtl) {
        couldSwipeLeft = deltaX.swipeLeft && this.activeIndex > 0;
        couldSwipeRight = !deltaX.swipeLeft && this.activeIndex < this.length - 1;
      }

      if (couldSwipeLeft || couldSwipeRight) {
        const sliderWidth: number = await this.getSliderWidth();

        if (deltaX.deltaX > (sliderWidth / this.autoSwipeRatio)) {
          this.deckTranslateX = deltaX.swipeLeft ? this.deckTranslateX - sliderWidth : this.deckTranslateX + sliderWidth;

          if (this.isNextChange(deltaX.swipeLeft)) {
            this.activeIndex++;

            if (emitEvent) {
              this.slideNextDidChange.emit(this.activeIndex);
            }
          } else {
            this.activeIndex--;

            if (emitEvent) {
              this.slidePrevDidChange.emit(this.activeIndex);
            }
          }
        }
      }

      await this.doSwipeSlide(deltaX.slider);

      // We want to lazy load the next slide images
      await this.lazyLoadContent(this.activeIndex + 1);

      resolve();
    });
  }

  private getSliderWidth(): Promise<number> {
    return new Promise<number>((resolve) => {
      if (!this.embedded) {
        resolve(window.innerWidth);
        return;
      }

      const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

      if (!slider || !slider.offsetParent || slider.offsetParent.clientWidth <= 0) {
        resolve(window.innerWidth);
        return;
      }

      resolve(slider.offsetParent.clientWidth);
    });
  }

  private isNextChange(swipeLeft: boolean): boolean {
    return (swipeLeft && !this.rtl) || (!swipeLeft && this.rtl);
  }

  private doSwipeSlide(slider: HTMLElement, speed?: number | undefined): Promise<void> {
    return new Promise<void>((resolve) => {
      slider.style.setProperty('--transformX', this.deckTranslateX + 'px');
      slider.style.setProperty('--transformXDuration', '' + (!isNaN(speed) ? speed : 300) + 'ms');

      this.slideWillChange.emit(this.deckTranslateX);

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

      const currentX: number = DeckdeckgoUtils.unifyEvent(e).clientX;

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
  async slideDidLoad() {
    await this.updateLength();

    await this.afterSlidesDidLoad();
  }

  private async updateLength() {
    const filteredSlides: HTMLElement[] = await this.getDefinedFilteredSlides();
    this.length = filteredSlides ? filteredSlides.length : 0;
  }

  private afterSlidesDidLoad(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const filteredSlides: HTMLElement[] = await this.getDefinedFilteredSlides();

      const loadedSlides: NodeListOf<HTMLElement> = this.el.querySelectorAll('.deckgo-slide-container');

      const definedSlidesLength: number = filteredSlides ? filteredSlides.length : 0;

      // Are all slides loaded?
      if (filteredSlides && loadedSlides && loadedSlides.length === definedSlidesLength && definedSlidesLength === this.length) {
        const orderedSlidesTagNames: DeckdeckgoSlideDefinition[] = [];

        Array.from(loadedSlides).forEach((slide: HTMLElement) => {
          const notes: HTMLElement = slide.querySelector('[slot=\'notes\']');
          const title: HTMLElement = slide.querySelector('[slot=\'title\']');

          orderedSlidesTagNames.push({
            name: slide.tagName,
            title: title ? title.innerHTML : null,
            notes: notes ? notes.innerHTML : null
          });
        });

        this.slidesDidLoad.emit(orderedSlidesTagNames);

        const promises: Promise<void>[] = [];
        promises.push(this.lazyLoadFirstSlides());
        promises.push(DeckdeckgoDeckBackgroundUtils.cloneSlots(this.el, filteredSlides, 'actions'));
        promises.push(DeckdeckgoDeckBackgroundUtils.cloneAndLoadBackground(this.el, filteredSlides, this.cloneBackground));

        await Promise.all(promises);
      }

      resolve();
    });
  }

  private getDefinedFilteredSlides(): Promise<HTMLElement[]> {
    return new Promise<HTMLElement[]>(async (resolve) => {
      const definedSlides: HTMLCollection = this.el.children;
      const filteredSlides: HTMLElement[] = await this.filterSlides(definedSlides);

      resolve(filteredSlides);
    });
  }

  private lazyLoadFirstSlides(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const lazyLoadContentFirstSlide = this.lazyLoadContent(0);
      const lazyLoadContentSecondSlide = this.lazyLoadContent(1);

      await Promise.all([lazyLoadContentFirstSlide, lazyLoadContentSecondSlide]);

      resolve();
    });
  }

  @Method()
  loadBackground(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const filteredSlides: HTMLElement[] = await this.getDefinedFilteredSlides();

      if (!filteredSlides || filteredSlides.length <= 0) {
        resolve();
        return;
      }

      await DeckdeckgoDeckBackgroundUtils.removeSlots(filteredSlides, 'background');

      await DeckdeckgoDeckBackgroundUtils.cloneAndLoadBackground(this.el, filteredSlides, this.cloneBackground);

      resolve();
    });
  }

  // The last children might be slots (background, note or action)
  private filterSlides(definedSlides: HTMLCollection): Promise<HTMLElement[]> {
    return new Promise<HTMLElement[]>((resolve) => {
      if (!definedSlides || definedSlides.length <= 0) {
        resolve(null);
        return;
      }

      const slides: Element[] = Array.from(definedSlides).filter((slide: Element) => {
        return slide.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1
      });

      resolve(slides as HTMLElement[]);
    });
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
  async slideNext(slideAnimation?: boolean, emitEvent?: boolean) {
    await this.slideNextPrev(!this.rtl, slideAnimation, emitEvent);
  }

  @Method()
  async slidePrev(slideAnimation?: boolean, emitEvent?: boolean) {
    await this.slideNextPrev(this.rtl, slideAnimation, emitEvent);
  }

  private async slideNextPrev(swipeLeft: boolean, slideAnimation: boolean = true, emitEvent?: boolean) {
    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider || !window) {
      return;
    }

    let couldSwipe: boolean;

    if (!slideAnimation) {
      couldSwipe = true;
    } else {
      couldSwipe = await this.beforeSwipe(this.isNextChange(swipeLeft));
    }

    // We might want first to show hide stuffs in the slide before swiping
    if (couldSwipe) {
      const deltaX: DeltaX = {
        slider: slider,
        swipeLeft: swipeLeft,
        deltaX: window.innerWidth
      };

      await this.swipeSlide(deltaX, emitEvent);

      await this.afterSwipe(swipeLeft);
    }
  }

  private beforeSwipe(enter: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');

      if (!slide) {
        // If we find no slide, we are cool something went wrong but the talk/show must go on
        resolve(true);
      } else {
        const result: boolean = await (slide as any).beforeSwipe(enter);
        resolve(result);
      }
    });
  }

  private afterSwipe(swipeLeft: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const indexPreviousSlide: number = swipeLeft ? this.activeIndex - 1 : this.activeIndex + 1;

      if (isNaN(indexPreviousSlide) || indexPreviousSlide < 0 || indexPreviousSlide > this.length) {
        resolve();
        return;
      }

      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (indexPreviousSlide + 1) + ')');

      if (!slide) {
        // Might be a swipe after the first or last slide
        resolve();
      } else {
        await (slide as any).afterSwipe();
        resolve();
      }
    });
  }

  private lazyLoadContent(index: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

      if (slide) {
        await (slide as any).lazyLoadContent();
      }

      resolve();
    });
  }

  @Method()
  async slideTo(index: number, speed?: number | undefined, emitEvent: boolean = true) {
    if (index > this.length || index < 0) {
      return;
    }

    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider || !window) {
      return;
    }

    const slideWidth: number = this.length > 0 && slider.offsetWidth > 0 ? (slider.offsetWidth / this.length) : window.innerWidth;

    this.deckTranslateX = index * slideWidth * (this.rtl ? 1 : -1);
    this.activeIndex = index;

    await this.lazyLoadContent(this.activeIndex);
    await this.doSwipeSlide(slider, speed);

    if (emitEvent) {
      this.slideToChange.emit(index);
    }
  }

  @Method()
  async deleteActiveSlide() {
    if (this.activeIndex > this.length || this.activeIndex < 0) {
      return;
    }

    const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');

    if (!slide) {
      return;
    }

    slide.parentElement.removeChild(slide);

    this.activeIndex = this.activeIndex > 0 ? this.activeIndex - 1 : 0;
    this.length = this.length > 0 ? this.length - 1 : 0;

    if (this.length > 0) {
      await this.slideTo(this.activeIndex, 0);
    }
  }

  @Method()
  async blockSlide(block: boolean) {
    this.block = block;
  }

  /* END: Manual sliding */

  /* BEGIN: Full screen */

  @Method()
  toggleFullScreen(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const doc = window.document;
      const docEl = doc.documentElement;

      // @ts-ignore
      const requestFullScreen = docEl.requestFullscreen || docEl.mozRequestFullScreen || docEl.webkitRequestFullScreen || docEl.msRequestFullscreen;
      // @ts-ignore
      const cancelFullScreen = doc.exitFullscreen || doc.mozCancelFullScreen || doc.webkitExitFullscreen || doc.msExitFullscreen;

      // @ts-ignore
      if (!doc.fullscreenElement && !doc.mozFullScreenElement && !doc.webkitFullscreenElement && !doc.msFullscreenElement) {
        requestFullScreen.call(docEl);
      } else {
        cancelFullScreen.call(doc);
      }

      resolve();
    });
  }

  private async hideOrClearMouseCursorTimer(toggleFullscreen: boolean) {
    if (toggleFullscreen) {
      this.fullscreen = true;
      this.hideMouseCursorWithDelay();
    } else {
      await this.clearMouseCursorTimer(false);
      this.fullscreen = false;
    }
  }

  private async clearMouseCursorTimer(hideWithDelay: boolean) {
    if (!this.fullscreen) {
      return;
    }

    if (this.idleMouseTimer > 0) {
      clearTimeout(this.idleMouseTimer);
    }

    await this.showHideMouseCursor(true);

    if (hideWithDelay) {
      this.hideMouseCursorWithDelay();
    }
  }

  private hideMouseCursorWithDelay() {
    if (!this.fullscreen) {
      return;
    }

    this.idleMouseTimer = setTimeout(async () => {
      await this.showHideMouseCursor(false);
    }, 2000);
  }

  private showHideMouseCursor(show: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.fullscreen) {
        resolve();
        return;
      }

      if (!this.cursorHidden && show) {
        // Cursor already displayed, we don't want to touch the style multiple times if not needed
        resolve();
        return;
      }

      const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

      if (!slider) {
        resolve();
        return;
      }

      slider.style.setProperty('cursor', show ? 'initial' : 'none');
      this.mouseInactivity.emit(show);

      this.cursorHidden = !show;

      resolve();
    });
  }

  private showHideActionsSlot(toggleFullscreen: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

      if (!slider) {
        resolve();
        return;
      }

      if (toggleFullscreen) {
        slider.style.setProperty('--slide-actions-display', 'none');
      } else {
        slider.style.removeProperty('--slide-actions-display');
      }
    });
  }

  /* END: Full screen */

  /* BEGIN: Utils */

  @Method()
  doPrint(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (window) {
        await this.lazyLoadAllContent();

        window.print();
      }

      resolve();
    });
  }

  private lazyLoadAllContent(): Promise<void[]> {
    const promises = [];

    for (let i = 0; i < this.length; i++) {
      promises.push(this.lazyLoadContent(i));
    }

    return Promise.all(promises);
  }

  @Method()
  isMobile(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(DeckdeckgoUtils.isMobile());
    });
  }

  /* END: Utils */

  render() {
    return [
      <div class="deckgo-deck">
        <slot/>
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>,
      <div class="deckgo-pager">
        <deckgo-pager active-index={this.activeIndex} length={this.length} percentage={this.pagerPercentage}></deckgo-pager>
      </div>
    ]
  }

}
