import {Component, Element, Event, EventEmitter, h, Host, JSX, Method, Prop, State} from '@stencil/core';

import {BreadcrumbsStep} from '../../../../utils/editor/breadcrumbs-type';

@Component({
  tag: 'app-editor-actions',
  styleUrl: 'app-editor-actions.scss',
  shadow: false
})
export class AppEditorActions {
  @Element() el: HTMLElement;

  @Prop()
  hideFooterActions: boolean = true;

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

  @Event() private openShare: EventEmitter<void>;

  @Event() private deckDidChange: EventEmitter<HTMLElement>;

  @Event() private slideCopy: EventEmitter<HTMLElement>;

  @Event() private elementFocus: EventEmitter<HTMLElement>;

  @State()
  private deck: boolean = true;

  @State()
  private slide: boolean = true;

  @Method()
  async touch(element: HTMLElement) {
    const toolbar: HTMLAppElementActionsElement = this.el.querySelector('app-element-actions');

    if (!toolbar) {
      return;
    }

    await toolbar.touch(element);

    this.deck = false;
    this.slide = element && element.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1;
  }

  @Method()
  async selectDeck() {
    const toolbar: HTMLAppElementActionsElement = this.el.querySelector('app-element-actions');

    if (toolbar) {
      await toolbar.blurSelectedElement();
      await toolbar.unSelect();
    }

    this.blockSlide.emit(false);

    this.deck = true;
    this.slide = false;
  }

  @Method()
  async hideToolbar() {
    const toolbar: HTMLAppElementActionsElement = this.el.querySelector('app-element-actions');

    if (toolbar) {
      await toolbar.hideToolbar();
    }

    this.deck = true;
    this.slide = false;
  }

  private async selectStep($event: CustomEvent<HTMLElement>) {
    if (!$event) {
      return;
    }

    if (!$event.detail || $event.detail === undefined) {
      await this.selectDeck();
    } else {
      await this.touch($event.detail);
      $event.detail.focus();
    }
  }

  render() {
    return (
      <Host class={this.hideFooterActions ? 'hidden' : undefined}>
        {this.renderSelectedIndicator()}

        {this.renderDeckActions()}

        {this.renderEditActions()}
      </Host>
    );
  }

  private renderSelectedIndicator() {
    return (
      <div class="indicator">
        <app-breadcrumbs
          step={this.deck ? BreadcrumbsStep.DECK : this.slide ? BreadcrumbsStep.SLIDE : BreadcrumbsStep.ELEMENT}
          onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event)}></app-breadcrumbs>
      </div>
    );
  }

  private renderDeckActions() {
    return (
      <app-deck-actions
        class={!this.deck ? 'hidden' : undefined}
        fullscreen={this.fullscreen}
        slides={this.slides}
        blockSlide={this.blockSlide}
        signIn={this.signIn}
        addSlide={this.addSlide}
        animatePrevNextSlide={this.animatePrevNextSlide}
        slideTo={this.slideTo}
        toggleFullScreen={this.toggleFullScreen}
        actionPublish={this.actionPublish}
        openShare={this.openShare}
        deckDidChange={this.deckDidChange}
        onSelectDeck={() => this.selectDeck()}></app-deck-actions>
    );
  }

  private renderEditActions() {
    return <app-element-actions class={this.deck ? 'hidden' : undefined} slideCopy={this.slideCopy} elementFocus={this.elementFocus}></app-element-actions>;
  }
}
