import {Component, Listen, h, Host, State, Prop} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import {isSlide} from '../../../../../../../utils/deck/src';
import {deckSelector} from '../../../../utils/editor/deck.utils';

@Component({
  tag: 'app-slides-aside',
  styleUrl: 'app-slides-aside.scss'
})
export class AppSlidesAside {
  @State()
  private slides: HTMLElement[] = [];

  @Prop()
  deckRef!: HTMLDeckgoDeckElement;

  private readonly debounceUpdateAllSlides: () => void;

  private readonly debounceUpdateSlide: (updateSlide: HTMLElement) => void;

  constructor() {
    this.debounceUpdateAllSlides = debounce(async () => {
      await this.updateAllSlides();
    }, 750);

    this.debounceUpdateSlide = debounce(async (updateSlide: HTMLElement) => {
      await this.updateSlide(updateSlide);
    }, 750);
  }

  componentDidLoad() {
    this.debounceUpdateAllSlides();
  }

  @Listen('deckDidLoad', {target: 'document'})
  onDeckDidLoad() {
    this.debounceUpdateAllSlides();
  }

  @Listen('deckDidChange', {target: 'document'})
  onDeckDidChange() {
    this.debounceUpdateAllSlides();
  }

  @Listen('slideDidUpdate', {target: 'document'})
  onSlideDidUpdate({detail: updatedSlide}: CustomEvent<HTMLElement>) {
    this.debounceUpdateSlide(updatedSlide);
  }

  private async updateSlide(updatedSlide: HTMLElement) {
    const slideIndex: number = Array.from(updatedSlide.parentNode.children).indexOf(updatedSlide);

    this.slides = [...this.slides.map((slide: HTMLElement, index: number) => (slideIndex === index ? (updatedSlide.cloneNode(true) as HTMLElement) : slide))];
  }

  private async updateAllSlides() {
    const slides: NodeListOf<HTMLElement> = document.querySelectorAll(`${deckSelector} > *`);

    if (!slides) {
      return;
    }

    this.slides = Array.from(slides)
      .filter((slide: HTMLElement) => isSlide(slide))
      .map((slide: HTMLElement) => slide.cloneNode(true) as HTMLElement);
  }

  render() {
    return (
      <Host>
        {this.slides.map((slide: HTMLElement) => (
          <app-slide-thumbnail key={slide.getAttribute('slide_id')} slide={slide} deck={this.deckRef}></app-slide-thumbnail>
        ))}
      </Host>
    );
  }
}
