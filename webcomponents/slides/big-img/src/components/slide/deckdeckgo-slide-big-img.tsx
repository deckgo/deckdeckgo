import { Component, Element, Event, EventEmitter, Method, Prop, h, Host } from '@stencil/core';

import {
  DeckdeckgoSlide,
  hideLazyLoadImages,
  afterSwipe,
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
  @Prop() reverse: boolean = false;

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
    await hideLazyLoadImages(this.el);

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
    const axisDimension = this.axis === 'x' ? 'width' : 'height';
    const perpendicularAxisDimension = this.axis === 'y' ? 'width' : 'height';
    const axisMarginStart = `margin${this.axis === 'x' ? 'Left' : 'Top'}`;

    if (this.currentStep === -1 && next) {
      this.currentStep = this.reverse ? this.divisions.length : 0;
    } else if (this.currentStep === this.divisions.length && !next && this.reverse) {
      this.currentStep = -1;
    } else {
      this.currentStep = this.currentStep + (this.reverse ? -1 : 1) * (next ? 1 : -1);
    }

    if (this.currentStep === -1) {
      this.bigImg.classList.remove('cropped');
      this.bigImg.style[axisMarginStart] = '';
      this.bigImg.style[perpendicularAxisDimension] = '';
      this.crop.style[axisDimension] = '';
      this.crop.style[perpendicularAxisDimension] = '';
    } else {
      this.crop.style[perpendicularAxisDimension] = '100%';
      this.bigImg.style[perpendicularAxisDimension] = '100%';
      this.bigImg.classList.add('cropped');

      const previousNaturalDivision = this.currentStep === 0 ? 0 : this.divisions[this.currentStep - 1];

      const calcCrop = () => {
        const imgClientLength = this.bigImg[`client${capitalize(axisDimension)}`];
        const imgNaturalLength = this.bigImg[`natural${capitalize(axisDimension)}`];
        const lengthFactor = imgClientLength / imgNaturalLength;
        const currentNaturalDivision = this.currentStep === this.divisions.length ? imgNaturalLength : this.divisions[this.currentStep];
        return {
          length: (currentNaturalDivision - previousNaturalDivision) * lengthFactor,
          shift: -(previousNaturalDivision * lengthFactor)
        };
      };

      let crop = calcCrop();

      if (crop.length > this.el.shadowRoot.querySelector('.deckgo-slide').clientHeight) {
        this.crop.style[perpendicularAxisDimension] = '';
        crop = calcCrop();
      }

      this.crop.style[axisDimension] = crop.length + 'px';
      this.bigImg.style[axisMarginStart] = crop.shift + 'px';
    }
  }

  private isEnd(): boolean {
    return this.reverse ? this.currentStep === 0 : this.currentStep === this.divisions.length;
  }

  private isBeginning(): boolean {
    return this.currentStep === -1;
  }

  @Method()
  beforeSwipe(enter: boolean): Promise<boolean> {
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
            <img class="big-image" data-src={this.imgSrc} />
          </div>
        </div>
      </Host>
    );
  }
}
