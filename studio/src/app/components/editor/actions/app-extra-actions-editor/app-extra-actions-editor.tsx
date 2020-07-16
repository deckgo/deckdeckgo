import {Component, Element, Event, EventEmitter, h, Host, JSX, Prop} from '@stencil/core';

import store from '../../../../stores/busy.store';

import {SlidesHelper} from '../../../../helpers/editor/slides.helper';

import {AnonymousService} from '../../../../services/editor/anonymous/anonymous.service';

@Component({
  tag: 'app-extra-actions-editor',
  styleUrl: 'app-extra-actions-editor.scss',
  shadow: false,
})
export class AppExtraActionsEditor {
  @Element() el: HTMLElement;

  @Prop()
  fullscreen: boolean = false;

  @Prop()
  slides: JSX.IntrinsicElements[] = [];

  @Prop()
  deckIsBeginning: boolean = true;

  @Prop()
  deckIsEnd: boolean = false;

  @Event() private addSlide: EventEmitter<JSX.IntrinsicElements>;

  @Event() private animatePrevNextSlide: EventEmitter<boolean>;

  @Event() private signIn: EventEmitter<void>;

  @Event() private blockSlide: EventEmitter<boolean>;

  private anonymousService: AnonymousService;

  constructor() {
    this.anonymousService = AnonymousService.getInstance();
  }

  private async onActionOpenSlideAdd($event: UIEvent) {
    const couldAddSlide: boolean = await this.anonymousService.couldAddSlide(this.slides);

    if (!couldAddSlide) {
      this.signIn.emit();
      return;
    }

    const helper: SlidesHelper = new SlidesHelper(this.addSlide, this.blockSlide);
    await helper.openSlideAdd($event);
  }

  render() {
    return (
      <Host
        class={{
          fullscreen: this.fullscreen,
        }}>
        {this.renderPrevSlide()}
        {this.renderNextSlide()}

        {this.renderAddSlide()}
      </Host>
    );
  }

  private renderPrevSlide() {
    if (this.deckIsBeginning) {
      return undefined;
    }

    return (
      <button class="action prev" onClick={() => this.animatePrevNextSlide.emit(false)} aria-label="Previous slide" disabled={store.state.deckBusy}>
        <ion-icon name="chevron-back-outline"></ion-icon>
      </button>
    );
  }

  private renderNextSlide() {
    if (this.deckIsEnd) {
      return undefined;
    }

    return (
      <button class="action next" onClick={() => this.animatePrevNextSlide.emit(true)} aria-label="Next slide" disabled={store.state.deckBusy}>
        <ion-icon name="chevron-forward-outline"></ion-icon>
      </button>
    );
  }

  private renderAddSlide() {
    if (!this.deckIsEnd) {
      return undefined;
    }

    return (
      <button
        class="action next add"
        onClick={($event: UIEvent) => this.onActionOpenSlideAdd($event)}
        aria-label="Add a new slide"
        disabled={store.state.deckBusy}>
        <ion-icon src="/assets/icons/ionicons/add.svg"></ion-icon>
      </button>
    );
  }
}
