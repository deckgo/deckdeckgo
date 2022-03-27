import {isSlide} from '@deckdeckgo/deck-utils';
import {deckSelector, elementIndex, selectDeckSlide, slideTo} from '@deckdeckgo/editor';
import {debounce} from '@deckdeckgo/utils';
import type {ItemReorderEventDetail} from '@ionic/core';
import {Component, Element, Event, EventEmitter, h, Host, Listen, Prop, State, Watch} from '@stencil/core';

@Component({
  tag: 'app-slides-aside',
  styleUrl: 'app-slides-aside.scss'
})
export class AppSlidesAside {
  @Element() el: HTMLElement;

  @State()
  private slides: HTMLElement[] = [];

  @Prop()
  activeIndex: number;

  @Prop()
  deckRef!: HTMLDeckgoDeckElement;

  private hostRef!: HTMLAppSlidesAsideElement;
  private asideRef!: HTMLElement;

  @Event()
  private reorder: EventEmitter<ItemReorderEventDetail>;

  @Event()
  private stepTo: EventEmitter<HTMLElement | undefined>;

  @State()
  private reorderDetail: ItemReorderEventDetail | undefined = undefined;

  private readonly debounceUpdateAllSlides: () => void;

  private readonly debounceUpdateSlide: (updateSlide: HTMLElement) => void;

  private canDragLeave: boolean = true;
  private canDragHover: boolean = true;

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

  componentDidUpdate() {
    setTimeout(() => {
      this.canDragLeave = true;
      this.canDragHover = true;
    }, 250);
  }

  @Watch('activeIndex')
  onActiveIndexChange() {
    const nextThumbnail: HTMLElement | null = this.el.querySelector(`app-slide-thumbnail:nth-child(${this.activeIndex + 1})`);

    const rect: DOMRect | undefined = nextThumbnail?.getBoundingClientRect();

    if (rect?.top > this.hostRef?.scrollHeight || rect?.top < this.hostRef?.scrollTop) {
      this.asideRef?.scrollTo({
        top: rect.top,
        behavior: 'smooth'
      });
    }
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
    const slideIndex: number = elementIndex(updatedSlide);

    this.slides = [
      ...this.slides.map((slide: HTMLElement, index: number) =>
        slideIndex === index ? (updatedSlide.cloneNode(true) as HTMLElement) : slide
      )
    ];
  }

  private async deleteSlide(deletedSlide: HTMLElement) {
    const slideIndex: number = elementIndex(deletedSlide);

    this.slides = [...this.slides.filter((_slide: HTMLElement, index: number) => slideIndex !== index)];
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
    if (!this.canDragHover) {
      return;
    }

    if (!this.reorderDetail || this.reorderDetail.to === to) {
      return;
    }

    if (this.reorderDetail.to === -1 && to === 0) {
      this.canDragLeave = false;
    }

    this.reorderDetail = {
      ...this.reorderDetail,
      to
    };
  }

  private onDragLeave() {
    if (!this.canDragLeave) {
      return;
    }

    if (!this.reorderDetail) {
      return;
    }

    if (this.reorderDetail.to !== 0) {
      return;
    }

    this.canDragHover = false;

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

  private async slideTo(index: number) {
    const slide: HTMLElement | null = selectDeckSlide(index);

    this.stepTo.emit(slide);

    await slideTo(index);
  }

  render() {
    return (
      <Host ref={(el) => (this.hostRef = el as HTMLAppSlidesAsideElement)}>
        {this.renderSlides()}

        {this.renderActions()}
      </Host>
    );
  }

  private renderSlides() {
    return (
      <aside
        ref={(el) => (this.asideRef = el as HTMLElement)}
        onDrop={() => this.onDrop()}
        onDragOver={($event: DragEvent) => $event.preventDefault()}
        onDragLeave={() => this.onDragLeave()}
        class={this.reorderDetail !== undefined ? 'drag' : ''}>
        {this.slides.map((slide: HTMLElement, index: number) => this.renderThumbnail(slide, index))}
      </aside>
    );
  }

  private renderThumbnail(slide: HTMLElement, index: number) {
    const dragClass: string =
      index === this.reorderDetail?.to && this.reorderDetail?.from !== this.reorderDetail?.to
        ? 'hover'
        : index === 0 && this.reorderDetail?.to === -1
        ? 'hover-top'
        : index === this.reorderDetail?.from
        ? index === this.reorderDetail?.to
          ? 'drag-start'
          : 'drag-hover'
        : '';

    return (
      <app-slide-thumbnail
        custom-tappable
        onClick={async () => await this.slideTo(index)}
        key={slide.getAttribute('slide_id')}
        slide={slide}
        deck={this.deckRef}
        class={`${dragClass} ${this.activeIndex === index ? 'highlight' : ''}`}
        draggable={true}
        onDragStart={() => this.onDragStart(index)}
        onDragOver={() => this.onDragHover(index)}></app-slide-thumbnail>
    );
  }

  private renderActions() {
    return (
      <div class="actions">
        <app-action-add-slide slidesLength={this.slides.length} popoverCssClass="popover-menu-wide-start"></app-action-add-slide>
      </div>
    );
  }
}
