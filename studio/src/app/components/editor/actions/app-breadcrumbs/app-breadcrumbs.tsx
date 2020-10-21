import {Component, Prop, h, Host, EventEmitter, Event, Fragment} from '@stencil/core';

import store from '../../../../stores/busy.store';

import {BreadcrumbsStep} from '../../../../utils/editor/breadcrumbs-type';

@Component({
  tag: 'app-breadcrumbs',
  styleUrl: 'app-breadcrumbs.scss',
  shadow: false,
})
export class AppBreadcrumbs {
  @Prop()
  step: BreadcrumbsStep = BreadcrumbsStep.DECK;

  @Prop()
  slideNumber: number;

  @Event()
  private stepTo: EventEmitter<HTMLElement | undefined>;

  private async selectStep(step: BreadcrumbsStep) {
    if (!document) {
      return;
    }

    if (step === BreadcrumbsStep.DECK) {
      this.stepTo.emit(undefined);
    } else {
      const deck = document.querySelector('main > deckgo-deck');

      if (!deck) {
        return;
      }

      const index = await (deck as any).getActiveIndex();

      const slideElement: HTMLElement = deck.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

      if (!slideElement) {
        return;
      }

      if (step === BreadcrumbsStep.SLIDE) {
        this.stepTo.emit(slideElement);
      } else {
        const elementEditable: HTMLElement = slideElement.querySelector('[contenteditable], [editable]');

        if (elementEditable) {
          this.stepTo.emit(elementEditable);
        }
      }
    }
  }

  render() {
    return (
      <Host>
        {this.renderStep(BreadcrumbsStep.DECK)}
        {this.renderSeparator()}
        {this.renderStep(BreadcrumbsStep.SLIDE)}
        {this.renderSeparator()}
        {this.renderStep(BreadcrumbsStep.ELEMENT)}
      </Host>
    );
  }

  private renderStep(step: BreadcrumbsStep) {
    return (
      <button
        onMouseDown={($event) => $event.stopPropagation()}
        onTouchStart={($event) => $event.stopPropagation()}
        class={this.step === step ? 'selected' : undefined}
        onClick={() => this.selectStep(step)}
        disabled={store.state.deckBusy}>
        <ion-label>
          {step}{' '}
          {step === 'slide' ? (
            <Fragment>
              <small>#</small>
              {this.slideNumber + 1}
            </Fragment>
          ) : null}
        </ion-label>
      </button>
    );
  }

  private renderSeparator() {
    return <ion-label class={`separator ${store.state.deckBusy ? 'busy' : ''}`}>&#62;</ion-label>;
  }
}
