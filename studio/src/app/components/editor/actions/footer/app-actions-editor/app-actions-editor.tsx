import {Component, Element, Event, Watch, EventEmitter, Fragment, h, Host, JSX, Method, Prop, State, Listen} from '@stencil/core';

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

  @State()
  private hideBottomSheet: boolean = true;

  private bottomSheetState: 'open' | 'close' = 'close';

  private actionsElementRef!: HTMLAppActionsElementElement;

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

    this.step = element && element.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1 ? BreadcrumbsStep.SLIDE : BreadcrumbsStep.ELEMENT;
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
  async onHideActions() {
    if (!this.hideActions) {
      return;
    }

    await this.reset();
  }

  @Method()
  async reset() {
    if (this.actionsElementRef) {
      await this.actionsElementRef.reset();
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

  render() {
    return (
      <Host
        class={{
          fullscreen: this.fullscreen,
          hidden: this.hideActions,
          'hidden-bottom-sheet': this.hideBottomSheet,
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
        ref={(el) => (this.actionsElementRef = el as HTMLAppActionsElementElement)}
        class={this.step === BreadcrumbsStep.DECK ? 'hidden' : undefined}
        slideCopy={this.slideCopy}
        elementFocus={this.elementFocus}
        onResetted={() => this.selectStepDeck()}></app-actions-element>
    );
  }
}
