import {Component, Listen, h, Host, State, Prop, Event, EventEmitter} from '@stencil/core';

import {ItemReorderEventDetail} from '@ionic/core';

import {debounce} from '@deckdeckgo/utils';

import {isSlide} from '../../../../../../../utils/deck/src';
import {deckSelector, slideTo} from '../../../../utils/editor/deck.utils';

@Component({
  tag: 'app-slides-aside',
  styleUrl: 'app-slides-aside.scss'
})
export class AppSlidesAside {
  @State()
  private slides: HTMLElement[] = [];

  @Prop()
  deckRef!: HTMLDeckgoDeckElement;

  @Event()
  private reorder: EventEmitter<ItemReorderEventDetail>;

  @State()
  private reorderDetail: ItemReorderEventDetail | undefined = undefined;

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

  @Listen('slideDelete', {target: 'document'})
  async onSlideDelete({detail: deletedSlide}: CustomEvent<HTMLElement>) {
    await this.deleteSlide(deletedSlide);
  }

  private async updateSlide(updatedSlide: HTMLElement) {
    const slideIndex: number = this.slideIndex(updatedSlide);

    this.slides = [...this.slides.map((slide: HTMLElement, index: number) => (slideIndex === index ? (updatedSlide.cloneNode(true) as HTMLElement) : slide))];
  }

  private async deleteSlide(deletedSlide: HTMLElement) {
    const slideIndex: number = this.slideIndex(deletedSlide);

    this.slides = [...this.slides.filter((_slide: HTMLElement, index: number) => slideIndex !== index)];
  }

  private slideIndex(slide: HTMLElement): number {
    return Array.from(slide.parentNode.children).indexOf(slide);
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

  private onDragStart(from: number) {
    this.reorderDetail = {
      from,
      to: undefined,
      complete: () => {}
    };
  }

  private onDragHover(to: number) {
    if (!this.reorderDetail) {
      return;
    }

    this.reorderDetail = {
      ...this.reorderDetail,
      to
    };
  }

  private onDragLeave() {
    if (!this.reorderDetail) {
      return;
    }

    if (this.reorderDetail.to !== 0) {
      return;
    }

    this.reorderDetail = {
      ...this.reorderDetail,
      to: -1
    };
  }

  private onDrop() {
    if (!this.reorderDetail || this.reorderDetail.to === undefined) {
      return;
    }

    const {from, to, complete} = this.reorderDetail;
    const detail = {
      from,
      to: from > to ? to + 1 : to,
      complete
    };

    this.reorder.emit(detail);

    this.slides.splice(detail.to, 0, ...this.slides.splice(detail.from, 1));
    this.slides = [...this.slides];

    this.reorderDetail = undefined;
  }

  render() {
    return (
      <Host>
        {this.renderSlides()}

        {this.renderActions()}
      </Host>
    );
  }

  private renderSlides() {
    return (
      <aside
        onDrop={() => this.onDrop()}
        onDragOver={($event: DragEvent) => $event.preventDefault()}
        onDragLeave={() => this.onDragLeave()}
        class={this.reorderDetail !== undefined ? 'drag' : ''}>
        {this.slides.map((slide: HTMLElement, index: number) => (
          <app-slide-thumbnail
            custom-tappable
            onClick={async () => await slideTo(index)}
            key={slide.getAttribute('slide_id')}
            slide={slide}
            deck={this.deckRef}
            class={
              index === this.reorderDetail?.to && this.reorderDetail?.from !== this.reorderDetail?.to
                ? 'hover'
                : index === 0 && this.reorderDetail?.to === -1
                ? 'hover-top'
                : index === this.reorderDetail?.from
                ? index === this.reorderDetail?.to
                  ? 'drag-start'
                  : 'drag-hover'
                : ''
            }
            draggable={true}
            onDragStart={() => this.onDragStart(index)}
            onDragOver={() => this.onDragHover(index)}></app-slide-thumbnail>
        ))}
      </aside>
    );
  }

  private renderActions() {
    return (
      <div class="actions">
        <app-action-add-slide></app-action-add-slide>
      </div>
    );
  }
}
