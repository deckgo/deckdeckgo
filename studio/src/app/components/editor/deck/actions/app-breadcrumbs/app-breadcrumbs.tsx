import {deckSelector, selectSlide} from '@deckdeckgo/editor';
import {Component, Event, EventEmitter, Fragment, h, Host, Prop} from '@stencil/core';
import busyStore from '../../../../../stores/busy.store';
import deckEditorStore from '../../../../../stores/deck-editor.store';
import {BreadcrumbsStep} from '../../../../../types/editor/breadcrumbs-step';

@Component({
  tag: 'app-breadcrumbs',
  styleUrl: 'app-breadcrumbs.scss',
  shadow: false
})
export class AppBreadcrumbs {
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
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      const index = await deck?.getActiveIndex();

      const slideElement: HTMLElement | null = selectSlide({deck, index});

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
        class={deckEditorStore.state.step === step ? 'selected' : undefined}
        onClick={() => this.selectStep(step)}
        disabled={busyStore.state.busy}>
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
    return <ion-label class={`separator ${busyStore.state.busy ? 'busy' : ''}`}>&#62;</ion-label>;
  }
}
