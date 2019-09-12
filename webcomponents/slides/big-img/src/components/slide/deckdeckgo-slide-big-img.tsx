import { Component, Element, Event, EventEmitter, Method, Prop, h, Host } from '@stencil/core';

import {
  DeckdeckgoSlide,
  hideLazyLoadImages,
  afterSwipe,
  beforeSwipe,
  lazyLoadContent,
  hideAllRevealElements,
  showAllRevealElements
} from '@deckdeckgo/slide-utils';

const capitalize = str => str.charAt(0).toUpperCase() + str.slice(1);

@Component({
  tag: 'deckgo-slide-big-img',
  styleUrl: 'deckdeckgo-slide-big-img.scss',
  shadow: true
})
export class DeckdeckgoSlideBigImg implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({ reflectToAttr: true }) customActions: boolean = false;
  @Prop({ reflectToAttr: true }) customBackground: boolean = false;
  @Prop() imgSrc: string = '';
  @Prop() imgDivisions: string = '';
  @Prop() axis: 'x' | 'y' = 'x';

  private crop: HTMLElement;
  private bigImg: HTMLElement;
  private currentStep: number = -1;

  private get divisions(): number[] {
    return this.imgDivisions.split(';').map(str => {
      const num = parseInt(str);
      if (isNaN(num)) {
        return 0;
      }
      return num;
    });
  }

  async componentDidLoad() {
    this.crop = this.el.shadowRoot.querySelector('.crop');
    this.bigImg = this.el.shadowRoot.querySelector('.big-image');

    this.slideDidLoad.emit();
  }

  private next() {
    this.prevNext(true);
  }

  private prev() {
    this.prevNext(false);
  }

  private prevNext(next: boolean) {
    if (next && this.currentStep === -1) {
      this.bigImg.style[this.axis == 'x' ? 'height' : 'width'] = '100%';
      this.bigImg.classList.add('cropped');
    }
    if (!next && this.currentStep === 0) {
      this.bigImg.classList.remove('cropped');
      this.bigImg.style[this.axis == 'x' ? 'height' : 'width'] = '';
      this.crop.style.height = '';
      this.crop.style.width = '';
    }
    this.currentStep = this.currentStep + (next ? 1 : -1);
    const previousSize = this.currentStep === 0 ? 0 : this.divisions[this.currentStep - 1];
    const heightOrWidth = this.axis === 'x' ? 'width' : 'height';
    const clientHeightOrWidth = this.bigImg[`client${capitalize(heightOrWidth)}`];
    const naturalHeightOrWidht = this.bigImg[`natural${capitalize(heightOrWidth)}`];
    const sizeFactor = clientHeightOrWidth / naturalHeightOrWidht;
    this.crop.style[heightOrWidth] = (this.divisions[this.currentStep] - previousSize) * sizeFactor + 'px';
    this.bigImg.style[`margin${this.axis === 'x' ? 'Left' : 'Top'}`] = -(previousSize * sizeFactor) + 'px';
  }

  private isEnd(): boolean {
    return this.currentStep === this.divisions.length;
  }

  private isBeginning(): boolean {
    return this.currentStep === -1;
  }

  @Method()
  beforeSwipe(enter: boolean, reveal: boolean): Promise<boolean> {
    return new Promise<boolean>(async resolve => {
      const couldSwipe: boolean = !this.divisions[0] || (enter ? this.isEnd() : this.isBeginning());

      if (couldSwipe) {
        resolve(true);
        return;
      }

      if (enter) {
        await this.next();
      } else {
        await this.prev();
      }

      resolve(false);
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return lazyLoadContent(this.el);
  }

  @Method()
  revealContent(): Promise<void> {
    return showAllRevealElements(this.el);
  }

  @Method()
  hideContent(): Promise<void> {
    return hideAllRevealElements(this.el);
  }

  render() {
    return (
      <Host class={{ 'deckgo-slide-container': true }}>
        <div class="deckgo-slide">
          <div class="crop">
            <img class="big-image" data-src={this.imgSrc} alt={} />
          </div>
        </div>
      </Host>
    );
  }
}
