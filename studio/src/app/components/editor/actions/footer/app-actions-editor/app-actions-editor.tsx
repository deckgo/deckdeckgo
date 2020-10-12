import {Component, Element, Event, Watch, EventEmitter, Fragment, h, Host, JSX, Method, Prop, State} from '@stencil/core';

import {BreadcrumbsStep} from '../../../../../utils/editor/breadcrumbs-type';

@Component({
  tag: 'app-actions-editor',
  styleUrl: 'app-actions-editor.scss',
  shadow: false,
})
export class AppActionsEditor {
  @Element() el: HTMLElement;

  @Prop()
  hideActions: boolean = false;

  @Prop()
  fullscreen: boolean = false;

  @Prop()
  slides: JSX.IntrinsicElements[] = [];

  @Event() private blockSlide: EventEmitter<boolean>;

  @Event() private signIn: EventEmitter<void>;

  @Event() private addSlide: EventEmitter<JSX.IntrinsicElements>;

  @Event() private animatePrevNextSlide: EventEmitter<boolean>;

  @Event() private slideTo: EventEmitter<number>;

  @Event() private toggleFullScreen: EventEmitter<void>;

  @Event() private actionPublish: EventEmitter<void>;

  @Event() private deckDidChange: EventEmitter<HTMLElement>;

  @Event() private slideCopy: EventEmitter<HTMLElement>;

  @Event() private elementFocus: EventEmitter<HTMLElement>;

  @Event() private presenting: EventEmitter<boolean>;

  @State()
  private step: BreadcrumbsStep = BreadcrumbsStep.DECK;

  @Method()
  async touch(element: HTMLElement, autoOpen: boolean = true) {
    const actionsElement: HTMLAppActionsElementElement = this.el.querySelector('app-actions-element');

    if (!actionsElement) {
      return;
    }

    await actionsElement.touch(element, autoOpen);

    this.step = element && element.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1 ? BreadcrumbsStep.SLIDE : BreadcrumbsStep.ELEMENT;
  }

  @Method()
  async selectDeck() {
    const actionsElement: HTMLAppActionsElementElement = this.el.querySelector('app-actions-element');

    if (actionsElement) {
      await actionsElement.blurSelectedElement();
      await actionsElement.unSelect();
    }

    this.blockSlide.emit(false);

    this.selectStepDeck();
  }

  @Watch('hideActions')
  async hide() {
    if (!this.hideActions) {
      return;
    }

    const actionsElement: HTMLAppActionsElementElement = this.el.querySelector('app-actions-element');

    if (actionsElement) {
      await actionsElement.reset();
    }
  }

  private selectStepDeck() {
    this.step = BreadcrumbsStep.DECK;
  }

  private async selectStep($event: CustomEvent<HTMLElement>) {
    if (!$event) {
      return;
    }

    if (!$event.detail || $event.detail === undefined) {
      await this.selectDeck();
    } else {
      await this.touch($event.detail, false);
      $event.detail.focus();
    }
  }

  private sheetChanged($event: CustomEvent<'open' | 'close'>) {
    this.presenting.emit($event?.detail === 'close');
  }

  render() {
    return (
      <Host
        class={{
          fullscreen: this.fullscreen,
          hidden: this.hideActions,
        }}>
        {this.fullscreen ? this.renderFullscreen() : this.renderActions()}
      </Host>
    );
  }

  private renderFullscreen() {
    return <app-bottom-sheet onSheetChanged={($event: CustomEvent<'open' | 'close'>) => this.sheetChanged($event)}>{this.renderActions()}</app-bottom-sheet>;
  }

  private renderActions() {
    return (
      <Fragment>
        {this.renderSelectedIndicator()}

        {this.renderDeckActions()}

        {this.renderEditActions()}
      </Fragment>
    );
  }

  private renderSelectedIndicator() {
    return (
      <div class="indicator">
        <app-breadcrumbs step={this.step} onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event)}></app-breadcrumbs>
      </div>
    );
  }

  private renderDeckActions() {
    return (
      <app-actions-deck
        class={this.step != BreadcrumbsStep.DECK ? 'hidden' : undefined}
        fullscreen={this.fullscreen}
        slides={this.slides}
        blockSlide={this.blockSlide}
        signIn={this.signIn}
        addSlide={this.addSlide}
        animatePrevNextSlide={this.animatePrevNextSlide}
        slideTo={this.slideTo}
        toggleFullScreen={this.toggleFullScreen}
        actionPublish={this.actionPublish}
        deckDidChange={this.deckDidChange}
        onSelectDeck={() => this.selectDeck()}></app-actions-deck>
    );
  }

  private renderEditActions() {
    return (
      <app-actions-element
        class={this.step === BreadcrumbsStep.DECK ? 'hidden' : undefined}
        slideCopy={this.slideCopy}
        elementFocus={this.elementFocus}
        onResetted={() => this.selectStepDeck()}></app-actions-element>
    );
  }
}
