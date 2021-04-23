import {Component, Element, Listen, Method, Prop, State, Event, EventEmitter, h, Watch, Host} from '@stencil/core';

import {isIOS, unifyEvent, isMobile, isFullscreen, debounce} from '@deckdeckgo/utils';
import {getSlideDefinition, getAttributesDefinition} from '@deckdeckgo/deck-utils';

import {DeckdeckgoDeckDefinition, DeckdeckgoSlideDefinition, DeckdeckgoAttributeDefinition} from '@deckdeckgo/types';

import {DeckdeckgoDeckBackgroundUtils} from '../../utils/deckdeckgo-deck-background-utils';

import {HideSlides, RevealSlide, AnimationSlide} from '../../utils/deckdeckgo-deck-animation';

interface Delta {
  slider: HTMLElement;
  swipeNext: boolean;
  deltaX: number;
  deltaY: number;
}

// Duplicate DeckdeckgoSlide definition of @deckdeckgo/slide-utils without dependency but extending HTMLElement.
interface DeckdeckgoSlideHTMLElement extends HTMLElement {
  beforeSwipe(enter: boolean, _reveal: boolean): Promise<boolean>;

  afterSwipe(): Promise<void>;

  lazyLoadContent(): Promise<void>;

  revealContent(): Promise<void>;

  hideContent(): Promise<void>;
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
  private readonly idleMouseTimeout: number = 2000;

  @Event() mouseInactivity: EventEmitter<boolean>;

  @Prop() reveal: boolean = true;
  @Prop() revealOnMobile: boolean = false;

  @Prop({reflect: true}) animation: 'slide' | 'fade' | 'none' = 'slide';

  @Prop({reflect: true}) direction: 'horizontal' | 'vertical' | 'papyrus' = 'horizontal';
  @Prop({reflect: true}) directionMobile: 'horizontal' | 'vertical' | 'papyrus' = 'papyrus';

  @Prop() autoSlide: 'true' | 'false' = 'false';

  @Prop() autoSlideInterval: number = 5000;

  @State()
  private dir: 'horizontal' | 'vertical' | 'papyrus';

  private observer: IntersectionObserver;

  private slideLoopInterval: number;
  private idleSlideLoopTimer: number;

  async componentWillLoad() {
    await this.initRtl();
    await this.initDirection();
  }

  async componentDidLoad() {
    await this.initSlideSize();
    await this.initAutoSlide();

    this.initWindowResize();
    this.initKeyboardAssist();
  }

  disconnectedCallback() {
    if (this.observer) {
      this.observer.disconnect();
    }

    if (this.idleSlideLoopTimer > 0) {
      clearTimeout(this.idleSlideLoopTimer);
    }

    if (this.slideLoopInterval > 0) {
      clearInterval(this.slideLoopInterval);
    }

    if (this.idleMouseTimer > 0) {
      clearTimeout(this.idleMouseTimer);
    }
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

          if (this.dir !== 'papyrus') {
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

      if (this.dir === 'papyrus') {
        slider.style.setProperty('--slide-min-height', `${sliderSize.height}px`);
        slider.style.removeProperty('--slide-height');
      } else {
        slider.style.removeProperty('--slide-min-height');
        slider.style.setProperty('--slide-height', `${sliderSize.height}px`);
      }

      await this.initFontSize(slider, {height: sliderSize.height, width: sliderSize.width});

      resolve();
    });
  }

  private initSlideSizeEmbedded(slider: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!slider.offsetParent) {
        resolve();
        return;
      }

      if (slider.offsetParent) {
        if (slider.offsetParent.clientWidth > 0) {
          slider.style.setProperty('--slide-width', '' + slider.offsetParent.clientWidth + 'px');
        }

        if (slider.offsetParent.clientHeight > 0 && this.dir === 'papyrus') {
          slider.style.setProperty('--slide-min-height', '' + slider.offsetParent.clientHeight + 'px');
          slider.style.removeProperty('--slide-height');
        }

        if (slider.offsetParent.clientHeight > 0 && this.dir !== 'papyrus') {
          slider.style.removeProperty('--slide-min-height');
          slider.style.setProperty('--slide-height', '' + slider.offsetParent.clientHeight + 'px');
        }

        await this.initFontSize(slider, {height: slider.offsetParent.clientHeight, width: slider.offsetParent.clientWidth});
      }

      resolve();
    });
  }

  private async initFontSize(slider: HTMLElement, {height, width}: {height: number; width: number}) {
    // 576px height = font-size 16px or 1em (relative to the font-size of its direct or nearest parent)
    const fontSize: number = height / 576;
    const ratioFontSize: number = width / 16 * 9 / 576;

    slider.style.setProperty('--slide-auto-font-size', `${fontSize}em`);
    slider.style.setProperty('--slide-auto-ratio-font-size', `${ratioFontSize}em`);
  }

  private initKeyboardAssist() {
    if (document && this.keyboard) {
      document.addEventListener('keydown', this.keyboardAssist, {passive: true});
    }
  }

  private async initLazyLoadContent() {
    if (window && 'IntersectionObserver' in window) {
      await this.deferIntersectionObserverLoad();
    } else {
      await this.lazyLoadAllContent();
    }
  }

  private async deferIntersectionObserverLoad() {
    const slides: NodeListOf<HTMLElement> = this.el.querySelectorAll('.deckgo-slide-container');

    if (!slides || slides.length <= 0) {
      return;
    }

    if (this.observer) {
      this.observer.disconnect();
    }

    this.observer = new IntersectionObserver(this.onIntersection, {
      rootMargin: '300px',
      threshold: 0,
    });

    slides.forEach((slide: HTMLElement) => {
      this.observer.observe(slide);
    });
  }

  private onIntersection = async (entries: IntersectionObserverEntry[]) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    const intersectingElements: Element[] = entries
      .filter((entry: IntersectionObserverEntry) => entry.isIntersecting)
      .map((entry: IntersectionObserverEntry) => entry.target);

    if (!intersectingElements || intersectingElements.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = intersectingElements.map((element: Element) => {
      return new Promise<void>(async (resolve) => {
        this.observer.unobserve(element);
        await (element as DeckdeckgoSlideHTMLElement).lazyLoadContent();

        resolve();
      });
    });

    await Promise.all(promises);
  };

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

  @Watch('directionMobile')
  async onDirectionMobileChange() {
    await this.initDirection();
  }

  @Watch('direction')
  async onDirectionChange() {
    await this.initDirection();
  }

  private async initDirection() {
    this.dir = isMobile() ? this.directionMobile : this.direction;
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
    const css: CSSStyleDeclaration = window.getComputedStyle($event.target as HTMLElement);
    this.block =
      css?.userSelect === 'text' ||
      css?.getPropertyValue('-webkit-user-select') === 'text' ||
      css?.getPropertyValue('-moz-user-select') === 'text' ||
      css?.getPropertyValue('-ms-user-select') === 'text';

    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;
  }

  private async move($event: Event) {
    await this.clearMouseCursorTimer(true);

    if (this.block) {
      return;
    }

    if (this.animation !== 'slide') {
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
    if (this.dir !== 'horizontal') {
      return;
    }

    const transformX: number = delta.swipeNext ? this.deckMove - delta.deltaX : this.deckMove + delta.deltaX;

    delta.slider.style.setProperty('--transformX', transformX + 'px');

    this.slideDrag.emit(transformX);
  }

  private moveY(delta: Delta) {
    if (this.dir !== 'vertical') {
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
            this.dir !== 'horizontal'
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
      if (this.dir === 'horizontal') {
        slider.style.setProperty('--transformX', this.deckMove + 'px');
      }

      if (this.dir === 'vertical') {
        slider.style.setProperty('--transformY', this.deckMove + 'px');
      }

      if (this.animation === 'slide') {
        slider.style.setProperty('--transformXDuration', '' + (!isNaN(speed) ? speed : 300) + 'ms');
      } else {
        slider.style.setProperty('--transformXDuration', '0ms');
      }

      if (this.dir === 'papyrus') {
        const slide: HTMLElement | null = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');

        // HACK: Chrome does not perform any scroll if the behavior 'smooth' is set and the event is triggered programmatically or with the keyboard aka only works if triggered with a click event
        setTimeout(() => {
          slide?.scrollIntoView(this.animation === 'none' ? null : {behavior: 'smooth', block: 'nearest'});
        }, 0);
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
    if (!this.startX && this.dir === 'horizontal') {
      return null;
    }

    if (!this.startY && this.dir === 'vertical') {
      return null;
    }

    if (this.dir === 'papyrus') {
      return null;
    }

    const slider: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-deck');

    if (!slider) {
      return null;
    }

    const currentX: number = unifyEvent($event).clientX;
    const currentY: number = unifyEvent($event).clientY;

    if (this.startX === currentX && this.dir === 'horizontal') {
      return null;
    } else if (this.startY === currentY && this.dir === 'vertical') {
      return null;
    }

    const swipeLeft: boolean = this.startX > currentX && this.dir === 'horizontal';
    const swipeTop: boolean = this.startY > currentY && this.dir === 'vertical';

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
      promises.push(this.initLazyLoadContent());
      promises.push(DeckdeckgoDeckBackgroundUtils.loadSlots(this.el, filteredSlides, 'background', this.cloneBackground));

      if (this.dir !== 'papyrus') {
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
      if (!this.reveal || (!this.revealOnMobile && isMobile()) || this.dir === 'papyrus') {
        promises.push(this.revealAllContent());
      }

      await Promise.all(promises);

      resolve();
    });
  }

  @Method()
  getDeckDefinition(): Promise<DeckdeckgoDeckDefinition | null> {
    return new Promise<DeckdeckgoDeckDefinition | null>(async (resolve) => {
      const loadedSlides: NodeListOf<HTMLElement> = this.el.querySelectorAll('.deckgo-slide-container.hydrated');

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
        return !slide.hasAttribute('slot') || slide.getAttribute('slot') === '';
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
      const slide: DeckdeckgoSlideHTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.activeIndex + 1) + ')');

      if (!slide) {
        // If we find no slide, we are cool something went wrong but the talk/show must go on
        resolve(true);
      } else {
        const result: boolean = await slide.beforeSwipe(enter, this.reveal);
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

      const slide: DeckdeckgoSlideHTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (indexPreviousSlide + 1) + ')');

      if (!slide) {
        // Might be a swipe after the first or last slide
        resolve();
      } else {
        await slide.afterSwipe();
        resolve();
      }
    });
  }

  private async lazyLoadContent(index: number) {
    const slide: DeckdeckgoSlideHTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    if (slide) {
      await slide.lazyLoadContent();
    }
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

    this.deckMove = index * (this.dir === 'horizontal' ? sliderSize.width : sliderSize.height) * (this.rtl ? 1 : -1);

    this.activeIndex = index;

    await this.doSwipeSlide(slider, speed);

    // In case we are sliding to a slide which received focus before being displayed (previously outside viewport)
    // a scroll on the parent element would have been applied by the browser. Therefore we set it back to the origin, as we takes care of the positioning of the slider.
    slider.parentElement.scrollTo(0, 0);

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

      // We are entering fullscreen mode now, therefore we don't want to want two seconds to hide the mouse
      setTimeout(async () => {
        await this.showHideMouseCursor(false);
      }, 500);

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
    }, this.idleMouseTimeout);
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

      slider.style.setProperty('cursor', show ? 'inherit' : 'none');
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

  private async revealContent(index: number) {
    const slide: DeckdeckgoSlideHTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    if (slide) {
      await slide.revealContent();
    }
  }

  private async hideContent(index: number) {
    const slide: DeckdeckgoSlideHTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    if (slide) {
      await slide.hideContent();
    }
  }

  /* END: Reveal */

  /* BEGIN: AutoSlide */

  private async initAutoSlide() {
    if (this.autoSlide === 'true') {
      await this.onAutoSlide();
    }
  }

  @Watch('autoSlide')
  async onAutoSlide() {
    let idleMouseTimer;

    this.idleSlideLoopTimer = setTimeout(() => (idleMouseTimer = this.idleMouseTimer), this.idleMouseTimeout);

    if (this.autoSlide === 'true') {
      this.slideLoopInterval = setInterval(async () => {
        if (idleMouseTimer === this.idleMouseTimer) {
          const end: boolean = await this.isEnd();

          if (end) {
            await this.slideTo(0, 0);
          } else {
            await this.slideNext(true);
          }
        } else {
          idleMouseTimer = this.idleMouseTimer;
        }
      }, this.autoSlideInterval);
    } else {
      clearInterval(this.slideLoopInterval);
    }
  }

  /* END: AutoSlide */

  render() {
    return (
      <Host class={`${this.dir}`}>
        <main>
          {this.renderAnimation()}
          <div class="deckgo-deck">
            <slot />
            <slot name="actions"></slot>
            <slot name="background"></slot>
          </div>
          <slot name="pager"></slot>
        </main>
      </Host>
    );
  }

  private renderAnimation() {
    if (this.animation !== 'fade' || this.dir === 'papyrus') {
      return <AnimationSlide />;
    }

    return [<HideSlides />, <RevealSlide index={this.activeIndex} />];
  }
}
