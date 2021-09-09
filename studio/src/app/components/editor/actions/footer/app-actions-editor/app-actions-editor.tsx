import {Component, Element, Event, Watch, EventEmitter, Fragment, h, Host, JSX, Method, Prop, State, Listen} from '@stencil/core';

import {isSlide} from '@deckdeckgo/deck-utils';

import editorStore from '../../../../../stores/editor.store';
import undoRedoStore from '../../../../../stores/undo-redo.store';

import {BreadcrumbsStep} from '../../../../../types/editor/breadcrumbs-step';

@Component({
  tag: 'app-actions-editor',
  styleUrl: 'app-actions-editor.scss',
  shadow: false
})
export class AppActionsEditor {
  @Element() el: HTMLElement;

  @Prop()
  hideActions: boolean = false;

  @Prop()
  fullscreen: boolean = false;

  @Prop()
  slides: JSX.IntrinsicElements[] = [];

  @Prop()
  slideNumber: number;

  @Event({bubbles: true})
  private blockSlide: EventEmitter<boolean>;

  @Event() private animatePrevNextSlide: EventEmitter<boolean>;

  @Event() private toggleFullScreen: EventEmitter<void>;

  @Event() private actionPublish: EventEmitter<void>;

  @Event() private deckDidChange: EventEmitter<HTMLElement>;

  @Event() private slideCopy: EventEmitter<HTMLElement>;

  @Event() private slideTransform: EventEmitter<JSX.IntrinsicElements>;

  @Event() private elementFocus: EventEmitter<HTMLElement>;

  @Event() private presenting: EventEmitter<boolean>;

  @State()
  private hideBottomSheet: boolean = true;

  private bottomSheetState: 'open' | 'close' = 'close';

  private actionsElementRef!: HTMLAppActionsElementElement;

  private destroyUndoRedoListener;

  componentDidLoad() {
    this.destroyUndoRedoListener = undoRedoStore.onChange('elementInnerHTML', (elementInnerHTML: string | undefined) => {
      if (elementInnerHTML === undefined) {
        this.el.removeEventListener('click', this.resetElementInnerHTML, false);
        return;
      }

      this.el.addEventListener('click', this.resetElementInnerHTML, {once: true});
    });
  }

  disconnectedCallback() {
    this.destroyUndoRedoListener?.();
  }

  private resetElementInnerHTML = () => {
    undoRedoStore.state.elementInnerHTML = undefined;
  };

  @Watch('fullscreen')
  onFullscreenChange() {
    this.hideBottomSheet = true;
  }

  @Method()
  async touch(element: HTMLElement, autoOpen: boolean = true) {
    if (!this.actionsElementRef) {
      return;
    }

    await this.actionsElementRef.touch(element, autoOpen);

    editorStore.state.step = isSlide(element) ? BreadcrumbsStep.SLIDE : BreadcrumbsStep.ELEMENT;
  }

  @Method()
  async selectDeck() {
    if (this.actionsElementRef) {
      await this.actionsElementRef.blurSelectedElement();
      await this.actionsElementRef.unSelect();
    }

    this.blockSlide.emit(false);

    this.selectStepDeck();
  }

  @Listen('mouseInactivity', {target: 'document'})
  async inactivity($event: CustomEvent) {
    if (!this.fullscreen) {
      return;
    }

    const mouseDisplayed: boolean = $event?.detail;

    if (mouseDisplayed) {
      this.hideBottomSheet = false;
    } else if (this.bottomSheetState === 'close') {
      // On mouse inactivity we hide the bottom sheet only if already closed, otherwise the user should close it (he/she might still use it)
      this.hideBottomSheet = true;
    }
  }

  @Watch('hideActions')
  async reset() {
    if (!this.hideActions) {
      return;
    }

    if (this.actionsElementRef) {
      await this.actionsElementRef.reset();
    }
  }

  private selectStepDeck() {
    editorStore.state.step = BreadcrumbsStep.DECK;
  }

  @Method()
  async selectStep(element: HTMLElement | undefined) {
    if (!element || element === undefined) {
      await this.selectDeck();
    } else {
      await this.touch(element, false);
      element.focus();
    }
  }

  private sheetChanged($event: CustomEvent<'open' | 'close'>) {
    this.bottomSheetState = $event?.detail;
    this.presenting.emit($event?.detail === 'close');

    if ($event?.detail === 'close') {
      setTimeout(async () => {
        this.hideBottomSheet = true;
        await this.reset();
        await this.selectDeck();
      }, 400);
    }
  }

  private async selectDeckOnClick($event: MouseEvent | TouchEvent) {
    const src: HTMLElement = $event.composedPath()[0] as HTMLElement;

    if (!this.actionsElementRef.isEqualNode(src)) {
      return;
    }

    await this.selectDeck();
  }

  render() {
    return (
      <Host
        class={{
          fullscreen: this.fullscreen,
          hidden: this.hideActions,
          'hidden-bottom-sheet': this.hideBottomSheet
        }}>
        {this.fullscreen ? this.renderFullscreen() : this.renderActions()}
      </Host>
    );
  }

  private renderFullscreen() {
    return (
      <app-bottom-sheet onSheetChanged={($event: CustomEvent<'open' | 'close'>) => this.sheetChanged($event)}>
        <app-breadcrumbs
          slideNumber={this.slideNumber}
          onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event?.detail)}></app-breadcrumbs>

        {this.renderActions()}
      </app-bottom-sheet>
    );
  }

  private renderActions() {
    return (
      <Fragment>
        {this.renderDeckActions()}

        {this.renderEditActions()}
      </Fragment>
    );
  }

  private renderDeckActions() {
    return (
      <app-actions-deck
        class={editorStore.state.step != BreadcrumbsStep.DECK ? 'hidden' : undefined}
        fullscreen={this.fullscreen}
        slides={this.slides}
        animatePrevNextSlide={this.animatePrevNextSlide}
        toggleFullScreen={this.toggleFullScreen}
        actionPublish={this.actionPublish}
        deckDidChange={this.deckDidChange}
        onSelectDeck={() => this.selectDeck()}
        onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event?.detail)}></app-actions-deck>
    );
  }

  private renderEditActions() {
    return (
      <app-actions-element
        onClick={($event: MouseEvent | TouchEvent) => this.selectDeckOnClick($event)}
        ref={(el) => (this.actionsElementRef = el as HTMLAppActionsElementElement)}
        class={editorStore.state.step === BreadcrumbsStep.DECK ? 'hidden' : undefined}
        slideCopy={this.slideCopy}
        slideTransform={this.slideTransform}
        elementFocus={this.elementFocus}
        onResetted={() => this.selectStepDeck()}></app-actions-element>
    );
  }
}
