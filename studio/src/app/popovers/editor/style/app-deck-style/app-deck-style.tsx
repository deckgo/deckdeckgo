import {Component, Element, Event, EventEmitter, h, Host, Prop, State} from '@stencil/core';
import {ImageHelper} from '../../../../helpers/editor/image.helper';
import i18n from '../../../../stores/i18n.store';
import settingsStore from '../../../../stores/settings.store';
import {ImageAction} from '../../../../types/editor/image-action';
import {TargetElement} from '../../../../types/editor/target-element';

@Component({
  tag: 'app-deck-style',
  styleUrl: 'app-deck-style.scss'
})
export class AppDeck {
  @Element() el: HTMLElement;

  @Prop()
  deckDidChange: EventEmitter<HTMLElement>;

  @State()
  private applyToTargetElement: TargetElement = TargetElement.TEXT;

  @State()
  private deckElement: HTMLDeckgoDeckElement;

  @Event({bubbles: true})
  private signIn: EventEmitter<void>;

  @Event({bubbles: true})
  private blockSlide: EventEmitter<boolean>;

  private imageHelper: ImageHelper;

  async componentWillLoad() {
    this.deckElement = document?.querySelector('app-editor main deckgo-deck');

    this.imageHelper = new ImageHelper(this.deckDidChange, this.blockSlide, this.signIn);
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async selectApplyToTargetElement($event: CustomEvent<TargetElement>) {
    if ($event && $event.detail) {
      this.applyToTargetElement = $event.detail;
    }
  }

  private onDeckChange() {
    this.deckDidChange.emit(this.deckElement);
  }

  private async onImageAction($event: CustomEvent<ImageAction>) {
    if ($event && $event.detail) {
      const popover = this.el.closest('ion-popover') as HTMLIonPopoverElement;

      popover.onWillDismiss().then(async () => {
        await this.imageHelper.imageAction(this.deckElement, false, true, $event.detail);
      });

      await popover.dismiss();
    }
  }

  private async closePopoverAndSignIn() {
    this.signIn.emit();

    await this.closePopover();
  }

  render() {
    return (
      <Host edit-mode={settingsStore.state.editMode}>
        <ion-toolbar>
          <h2>{i18n.state.editor.deck_style}</h2>
          <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
        </ion-toolbar>
        <app-select-target-element
          textTarget={true}
          transition={true}
          header-footer={true}
          onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}
        ></app-select-target-element>

        {this.renderOptions()}

        {this.renderEditMode()}
      </Host>
    );
  }

  private renderEditMode() {
    if ([TargetElement.TEXT, TargetElement.BACKGROUND].includes(this.applyToTargetElement)) {
      return <app-edit-mode></app-edit-mode>;
    }

    return undefined;
  }

  private renderOptions() {
    if (this.applyToTargetElement === TargetElement.TEXT) {
      return <app-deck-fonts deckElement={this.deckElement} onFontsChange={() => this.onDeckChange()}></app-deck-fonts>;
    } else if (this.applyToTargetElement === TargetElement.BACKGROUND) {
      return [
        <app-color-text-background
          colorType={'background'}
          selectedTarget={this.deckElement}
          deck={true}
          onColorChange={() => this.onDeckChange()}
        ></app-color-text-background>,
        <app-image-choice
          selectedTarget={this.deckElement}
          deck={true}
          onAction={($event: CustomEvent<ImageAction>) => this.onImageAction($event)}
        ></app-image-choice>
      ];
    } else if (this.applyToTargetElement === TargetElement.TRANSITION) {
      return <app-deck-transition deckElement={this.deckElement} onTransitionChange={() => this.onDeckChange()}></app-deck-transition>;
    } else if (this.applyToTargetElement === TargetElement.HEADER_FOOTER) {
      return (
        <app-deck-header-footer
          deckElement={this.deckElement}
          deckDidChange={this.deckDidChange}
          onNavigateSettings={() => this.closePopover()}
          onNavigateSignIn={() => this.closePopoverAndSignIn()}
        ></app-deck-header-footer>
      );
    } else {
      return undefined;
    }
  }
}
