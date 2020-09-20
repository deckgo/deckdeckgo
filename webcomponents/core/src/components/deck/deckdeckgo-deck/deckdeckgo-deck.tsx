import {Component, Element, Listen, Method, Prop, State, Event, EventEmitter, h, Watch} from '@stencil/core';

import {isIOS, unifyEvent, isMobile, isFullscreen, debounce} from '@deckdeckgo/utils';
import {getSlideDefinition, getAttributesDefinition} from '@deckdeckgo/deck-utils';

import {DeckdeckgoDeckDefinition, DeckdeckgoSlideDefinition, DeckdeckgoAttributeDefinition} from '@deckdeckgo/types';

import {DeckdeckgoDeckBackgroundUtils} from '../../utils/deckdeckgo-deck-background-utils';

import {HideSlides, RevealSlide, TransitionSlide} from '../../utils/deckdeckgo-deck-transition';

interface Delta {
  slider: HTMLElement;
  swipeNext: boolean;
  deltaX: number;
  deltaY: number;
}

@Component({
  tag: 'deckgo-deck',
  styleUrl: 'deckdeckgo-deck.scss',
  shadow: true,
})
export class DeckdeckgoDeck {
  @Element() el: HTMLElement;

  @Prop() keyboard: boolean = true;

  @Prop() embedded: boolean = false;

  @Prop() cloneBackground: boolean = true;

  @State()
  private rtl: boolean = false;

  private startX: number = null;
  private startY: number = null;
  private deckMove: number = 0;
  private autoSwipeRatio: number = 10;

  private block: boolean = false;

  @State()
  private activeIndex: number = 0;

  @State()
  private length: number = 0;

  @Event() slidesDidLoad: EventEmitter;
  @Event() slideNextDidAnimate: EventEmitter<void>;
  @Event() slidePrevDidAnimate: EventEmitter<void>;
  @Event() slideNextDidChange: EventEmitter<number>;
  @Event() slidePrevDidChange: EventEmitter<number>;
  @Event() slideToChange: EventEmitter<number>;
  @Event() slideDrag: EventEmitter<number>;
  @Event() slideWillChange: EventEmitter<number>;

  @Event() deckDidLoad: EventEmitter<void>;

  private fullscreen: boolean = false;
  private cursorHidden: boolean = false;
  private idleMouseTimer: number;

  @Event() mouseInactivity: EventEmitter<boolean>;

  @Prop() reveal: boolean = true;
  @Prop() revealOnMobile: boolean = false;

  @Prop({reflect: true}) transition: 'slide' | 'fade' | 'none' = 'slide';

  @Prop({reflect: true}) direction: 'horizontal' | 'vertical' | 'papyrus' = 'horizontal';

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
      window.addEventListener(
        'resize',
        debounce(async () => {
          await this.initSlideSize();

          if (this.direction !== 'papyrus') {
            await this.slideTo(this.activeIndex);
          }

          const toggleFullscreen: boolean = isFullscreen();
          await this.hideOrClearMouseCursorTimer(toggleFullscreen);
          await this.showHideActionsSlot(toggleFullscreen);
        }, 100)
      );
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
    return new Promise<void>(async (resolve) => {
      if (!window || !screen) {
        resolve();
        return;
      }

      const sliderSize: {width: number; height: number} = await this.getSliderSize();

      slider.style.setProperty('--slide-width', `${sliderSize.width}px`);

      if (this.direction === 'papyrus') {
        slider.style.setProperty('--slide-min-height', `${sliderSize.height}px`);
        slider.style.setProperty('--slide-height', `auto`);
      } else {
        slider.style.removeProperty('--slide-min-height');
        slider.style.setProperty('--slide-height', `${sliderSize.height}px`);
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

        if (slider.offsetParent.clientHeight > 0 && this.direction === 'papyrus') {
          slider.style.setProperty('--slide-min-height', '' + slider.offsetParent.clientHeight + 'px');
          slider.style.setProperty('--slide-height', `auto`);
        }

        if (slider.offsetParent.clientHeight > 0 && this.direction !== 'papyrus') {
          slider.style.removeProperty('--slide-min-height');
          slider.style.setProperty('--slide-height', '' + slider.offsetParent.clientHeight + 'px');
        }
      }

      resolve();
    });
  }

  private initKeyboardAssist() {
    if (document && this.keyboard) {
      document.addEventListener('keydown', this.keyboardAssist, {passive: true});
    }
  }

  @Method()
  async toggleKeyboardAssist(state: boolean) {
    if (!document) {
      return;
    }

    if (!this.keyboard) {
      return;
    }

    if (state) {
      document.addEventListener('keydown', this.keyboardAssist, {passive: true});
    } else {
      // @ts-ignore
      document.removeEventListener('keydown', this.keyboardAssist, {passive: true});
    }
  }

  private keyboardAssist = async ($event: KeyboardEvent) => {
    if ($event.defaultPrevented) {
      return;
    }

    if (['ArrowLeft', 'k', 'PageUp'].indexOf($event.key) !== -1) {
      await this.slideNextPrev(false, true);
    } else if (['ArrowRight', 'j', 'PageDown'].indexOf($event.key) !== -1) {
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
    this.resetStart();
  }

  @Listen('contextmenu', {passive: true})
  async contextMenu() {
    this.resetStart();
  }

  @Listen('scrolling')
  scrolling($event: CustomEvent) {
    this.block = $event ? $event.detail : false;
  }

  @Listen('keypress')
  async keypress() {
    await this.clearMouseCursorTimer(true);
  }

  private start($event: Event) {
    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;
  }

  private async move($event: Event) {
    await this.clearMouseCursorTimer(true);

    if (this.block) {
      return;
    }

    if (this.transition !== 'slide') {
      return;
    }

    const delta: Delta = await this.getDelta($event);

    if (!delta) {
      return;
    }

    this.moveX(delta);
    this.moveY(delta);

    delta.slider.style.setProperty('--transformXDuration', '0ms');
  }

  private moveX(delta: Delta) {
    if (this.direction !== 'horizontal') {
      return;
    }

    const transformX: number = delta.swipeNext ? this.deckMove - delta.deltaX : this.deckMove + delta.deltaX;

    delta.slider.style.setProperty('--transformX', transformX + 'px');

    this.slideDrag.emit(transformX);
  }

  private moveY(delta: Delta) {
    if (this.direction !== 'vertical') {
      return;
    }

    const transformY: number = delta.swipeNext ? this.deckMove - delta.deltaY : this.deckMove + delta.deltaY;

    delta.slider.style.setProperty('--transformY', transformY + 'px');

    this.slideDrag.emit(transformY);
  }

  private async stop($event: Event) {
    if (this.block) {
      return;
    }

    const delta: Delta = await this.getDelta($event);

    await this.swipeSlide(delta);

    this.resetStart();
  }

  private swipeSlide(delta: Delta, emitEvent: boolean = true): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!delta || !window) {
        resolve();
        return;
      }

      let couldSwipeNext: boolean = delta.swipeNext && this.activeIndex < this.length - 1;
      let couldSwipePrevious: boolean = !delta.swipeNext && this.activeIndex > 0;

      if (this.rtl) {
        couldSwipeNext = delta.swipeNext && this.activeIndex > 0;
        couldSwipePrevious = !delta.swipeNext && this.activeIndex < this.length - 1;
      }

      if (couldSwipeNext || couldSwipePrevious) {
        const sliderSize: {width: number; height: number} = await this.getSliderSize();

        const autoSwipeHorizontal: boolean = delta.deltaX > sliderSize.width / this.autoSwipeRatio;
        const autoSwipeVertical: boolean = delta.deltaY > sliderSize.height / this.autoSwipeRatio;

        if (autoSwipeHorizontal || autoSwipeVertical) {
          this.deckMove =
            this.direction !== 'horizontal'
              ? delta.swipeNext
                ? this.deckMove - sliderSize.height
                : this.deckMove + sliderSize.height
              : delta.swipeNext
              ? this.deckMove - sliderSize.width
              : this.deckMove + sliderSize.width;

          if (this.isNextChange(delta.swipeNext)) {
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

      await this.doSwipeSlide(delta.slider);

      // We want to lazy load the next slide images
      await this.lazyLoadContent(this.activeIndex + 1);

      resolve();
    });
  }

  private async getSliderSize(): Promise<{width: number; height: number}> {
    if (!this.embedded) {
      return this.getSliderSizeNotEmbededd();
    }

    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider || !slider.offsetParent || slider.offsetParent.clientWidth <= 0) {
      return this.getSliderSizeNotEmbededd();
    }

    return {
      width: slider.offsetParent.clientWidth,
      height: slider.offsetParent.clientHeight,
    };
  }

  private async getSliderSizeNotEmbededd(): Promise<{width: number; height: number}> {
    if (isIOS()) {
      return {
        width: screen.width > window.innerWidth ? screen.width : window.innerWidth,
        height: screen.height > window.innerHeight ? screen.height : window.innerHeight,
      };
    } else {
      return {
        width: window.innerWidth,
        height: window.innerHeight,
      };
    }
  }

  private isNextChange(swipeLeft: boolean): boolean {
    return (swipeLeft && !this.rtl) || (!swipeLeft && this.rtl);
  }

  private doSwipeSlide(slider: HTMLElement, speed?: number | undefined): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.direction === 'horizontal') {
        slider.style.setProperty('--transformX', this.deckMove + 'px');
      }

      if (this.direction === 'vertical') {
        slider.style.setProperty('--transformY', this.deckMove + 'px');
      }

      if (this.transition === 'slide') {
        slider.style.setProperty('--transformXDuration', '' + (!isNaN(speed) ? speed : 300) + 'ms');
      } else {
        slider.style.setProperty('--transformXDuration', '0ms');
      }

      if (this.direction === 'papyrus') {
        const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');
        slide.scrollIntoView(this.transition === 'none' ? null : {behavior: 'smooth'});
      }

      this.slideWillChange.emit(this.deckMove);

      this.resetStart();

      resolve();
    });
  }

  private resetStart() {
    this.startX = null;
    this.startY = null;
  }

  private async getDelta($event): Promise<Delta> {
    if (!this.startX && this.direction === 'horizontal') {
      return null;
    }

    if (!this.startY && this.direction === 'vertical') {
      return null;
    }

    if (this.direction === 'papyrus') {
      return null;
    }

    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider) {
      return null;
    }

    const currentX: number = unifyEvent($event).clientX;
    const currentY: number = unifyEvent($event).clientY;

    if (this.startX === currentX && this.direction === 'horizontal') {
      return null;
    } else if (this.startY === currentY && this.direction === 'vertical') {
      return null;
    }

    const swipeLeft: boolean = this.startX > currentX && this.direction === 'horizontal';
    const swipeTop: boolean = this.startY > currentY && this.direction === 'vertical';

    return {
      slider,
      swipeNext: swipeLeft || swipeTop,
      deltaX: swipeLeft ? this.startX - currentX : currentX - this.startX,
      deltaY: swipeTop ? this.startY - currentY : currentY - this.startY,
    };
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
      const deckDefinition: DeckdeckgoDeckDefinition = await this.getDeckDefinition();

      if (deckDefinition && deckDefinition.slides && deckDefinition.slides.length > 0) {
        this.slidesDidLoad.emit(deckDefinition);

        await this.onAllSlidesDidLoad();

        this.deckDidLoad.emit();
      }

      resolve();
    });
  }

  private onAllSlidesDidLoad(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const filteredSlides: HTMLElement[] = await this.getDefinedFilteredSlides();

      const promises: Promise<void>[] = [];
      promises.push(this.lazyLoadFirstSlides());
      promises.push(DeckdeckgoDeckBackgroundUtils.loadSlots(this.el, filteredSlides, 'background', this.cloneBackground));

      if (this.direction !== 'papyrus') {
        promises.push(DeckdeckgoDeckBackgroundUtils.cloneSlots(this.el, filteredSlides, 'actions'));
        promises.push(DeckdeckgoDeckBackgroundUtils.loadSlots(this.el, filteredSlides, 'header'));
        promises.push(DeckdeckgoDeckBackgroundUtils.loadSlots(this.el, filteredSlides, 'footer'));
      } else if (filteredSlides && filteredSlides.length > 0) {
        promises.push(DeckdeckgoDeckBackgroundUtils.cloneSlot(this.el, filteredSlides[0], 'actions'));
        promises.push(DeckdeckgoDeckBackgroundUtils.loadSlot(this.el, filteredSlides[0], 'header'));
        promises.push(DeckdeckgoDeckBackgroundUtils.loadSlot(this.el, filteredSlides[filteredSlides.length - 1], 'footer'));
      }

      // In standard case, we want to be able to reveal elements or not, as we wish but if we set reveal to false, we want to display everything straight at the begin.
      // Or we display also all reveal elements on mobile devices as there is no keyboard on mobile device to reveal elements
      // Also, no reveal for papyrus as we can scroll
      if (!this.reveal || (!this.revealOnMobile && isMobile()) || this.direction === 'papyrus') {
        promises.push(this.revealAllContent());
      }

      await Promise.all(promises);

      resolve();
    });
  }

  @Method()
  getDeckDefinition(): Promise<DeckdeckgoDeckDefinition | null> {
    return new Promise<DeckdeckgoDeckDefinition | null>(async (resolve) => {
      const loadedSlides: NodeListOf<HTMLElement> = this.el.querySelectorAll('.deckgo-slide-container');

      if (!loadedSlides || loadedSlides.length <= 0) {
        resolve(null);
        return;
      }

      const filteredSlides: HTMLElement[] = await this.getDefinedFilteredSlides();

      const definedSlidesLength: number = filteredSlides ? filteredSlides.length : 0;

      // Are all slides loaded?
      if (loadedSlides.length !== definedSlidesLength) {
        resolve(null);
        return;
      }

      const orderedSlidesTagNames: DeckdeckgoSlideDefinition[] = [];

      for (const slide of Array.from(loadedSlides)) {
        const attributes: DeckdeckgoAttributeDefinition[] = await getAttributesDefinition(slide.attributes);

        orderedSlidesTagNames.push({
          template: slide.tagName ? slide.tagName.toLowerCase() : undefined,
          content: slide.innerHTML,
          attributes: attributes,
        });
      }

      const attributes: DeckdeckgoAttributeDefinition[] = await getAttributesDefinition(this.el.attributes);
      const background: HTMLElement = this.el.querySelector(":scope > [slot='background']");

      const deck: DeckdeckgoDeckDefinition = {
        slides: orderedSlidesTagNames,
        attributes: attributes,
        background: background ? background.innerHTML : null,
        reveal: this.reveal,
        revealOnMobile: this.revealOnMobile,
      };

      resolve(deck);
    });
  }

  @Method()
  getSlideDefinition(index: number): Promise<DeckdeckgoSlideDefinition | null> {
    return new Promise<DeckdeckgoSlideDefinition | null>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

      const definition: DeckdeckgoSlideDefinition | null = await getSlideDefinition(slide);

      resolve(definition);
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
    return this.loadSlots('background', this.cloneBackground);
  }

  @Method()
  loadHeader(): Promise<void> {
    return this.loadSlots('header');
  }

  @Method()
  loadFooter(): Promise<void> {
    return this.loadSlots('footer');
  }

  private async loadSlots(slotName: string, clone: boolean = true) {
    const filteredSlides: HTMLElement[] = await this.getDefinedFilteredSlides();

    if (!filteredSlides || filteredSlides.length <= 0) {
      return;
    }

    await DeckdeckgoDeckBackgroundUtils.removeSlots(filteredSlides, slotName);

    await DeckdeckgoDeckBackgroundUtils.loadSlots(this.el, filteredSlides, slotName, clone);
  }

  // The last children might be slots (background, note or action)
  private filterSlides(definedSlides: HTMLCollection): Promise<HTMLElement[]> {
    return new Promise<HTMLElement[]>((resolve) => {
      if (!definedSlides || definedSlides.length <= 0) {
        resolve(null);
        return;
      }

      const slides: Element[] = Array.from(definedSlides).filter((slide: Element) => {
        return slide.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1;
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

  private async slideNextPrev(swipeNext: boolean, slideAnimation: boolean = true, emitEvent?: boolean) {
    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider || !window) {
      return;
    }

    let couldSwipe: boolean;

    if (!slideAnimation) {
      couldSwipe = true;
    } else {
      couldSwipe = await this.beforeSwipe(this.isNextChange(swipeNext));
    }

    // We might want first to show hide stuffs in the slide before swiping
    if (couldSwipe) {
      const sliderSize: {width: number; height: number} = await this.getSliderSize();

      const deltaX: Delta = {
        slider,
        swipeNext,
        deltaX: sliderSize.width,
        deltaY: sliderSize.height,
      };

      await this.swipeSlide(deltaX, emitEvent);

      await this.afterSwipe(swipeNext);
    } else if (emitEvent) {
      if (swipeNext) {
        this.slideNextDidAnimate.emit();
      } else {
        this.slidePrevDidAnimate.emit();
      }
    }
  }

  private beforeSwipe(enter: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');

      if (!slide) {
        // If we find no slide, we are cool something went wrong but the talk/show must go on
        resolve(true);
      } else {
        const result: boolean = await (slide as any).beforeSwipe(enter, this.reveal);
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

    const sliderSize: {width: number; height: number} = await this.getSliderSize();

    this.deckMove = index * (this.direction === 'horizontal' ? sliderSize.width : sliderSize.height) * (this.rtl ? 1 : -1);

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

    // If we want to block, then we reset then previous start position as we don't want to start the slide to scroll when the blocking will be resolved
    if (this.block) {
      this.resetStart();
    }
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

  @Method()
  lazyLoadAllContent(): Promise<void[]> {
    const promises = [];

    for (let i = 0; i < this.length; i++) {
      promises.push(this.lazyLoadContent(i));
    }

    return Promise.all(promises);
  }

  @Method()
  isMobile(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(isMobile());
    });
  }

  /* END: Utils */

  /* BEGIN: Reveal */

  @Watch('reveal')
  async onRevealChange() {
    if (!this.reveal) {
      await this.revealAllContent();
    } else {
      await this.redoRevealContent();
    }
  }

  private revealAllContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const promises = [];

      for (let i = 0; i < this.length; i++) {
        promises.push(this.revealContent(i));
      }

      await Promise.all(promises);

      resolve();
    });
  }

  private redoRevealContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // If we switch back to standard mode, reveal previous slide and hide the "reveal" content of this and next slides
      const promises = [];

      for (let i = 0; i < this.length; i++) {
        if (i < this.activeIndex) {
          promises.push(this.revealContent(i));
        } else {
          promises.push(this.hideContent(i));
        }
      }

      await Promise.all(promises);

      resolve();
    });
  }

  private revealContent(index: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

      if (slide) {
        await (slide as any).revealContent();
      }

      resolve();
    });
  }

  private hideContent(index: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slide: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

      if (slide) {
        await (slide as any).hideContent();
      }

      resolve();
    });
  }

  /* END: Reveal */

  render() {
    return (
      <main>
        {this.renderTransition()}
        <div class="deckgo-deck">
          <slot />
          <slot name="actions"></slot>
          <slot name="background"></slot>
        </div>
        <div class="deckgo-pager">{this.renderPager()}</div>
      </main>
    );
  }

  private renderPager() {
    return <deckgo-pager active-index={this.activeIndex} length={this.length}></deckgo-pager>;
  }

  private renderTransition() {
    if (this.transition !== 'fade' || this.direction === 'papyrus') {
      return <TransitionSlide />;
    }

    return [<HideSlides />, <RevealSlide index={this.activeIndex} />];
  }
}
