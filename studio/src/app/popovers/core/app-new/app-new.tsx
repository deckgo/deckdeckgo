import {ComponentInterface, h, Component, Element, Host, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-new',
  styleUrl: 'app-new.scss'
})
export class AppNew implements ComponentInterface {
  @Element() el: HTMLElement;

  @State()
  private type: 'deck' | 'doc' | undefined;

  async closePopover(confirm: boolean) {
    if (!confirm) {
      await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
      return;
    }

    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(this.type);
  }

  render() {
    return (
      <Host>
        <div class="type">
          <div>{this.type === 'doc' && <mark>{i18n.state.tools.new_experimental}</mark>}</div>

          <article>
            <button onClick={() => (this.type = 'deck')} class={`ion-activatable ${this.type === 'deck' ? 'selected' : ''}`}>
              <ion-ripple-effect></ion-ripple-effect>
              <AppIcon name="deck" path="icons" ariaLabel="" ariaHidden={true} lazy={false} slot="start"></AppIcon>
              <ion-label>{i18n.state.tools.new_deck}</ion-label>
            </button>
          </article>

          <article>
            <button onClick={() => (this.type = 'doc')} class={`ion-activatable ${this.type === 'doc' ? 'selected' : ''}`}>
              <ion-ripple-effect></ion-ripple-effect>
              <AppIcon name="doc" path="icons" ariaLabel="" ariaHidden={true} lazy={false} slot="start"></AppIcon>
              <ion-label>{i18n.state.tools.new_doc}</ion-label>
            </button>
          </article>
        </div>

        <p>{i18n.state.tools.new_warning_text}</p>

        <app-popover-confirm onConfirm={({detail}: CustomEvent<boolean>) => this.closePopover(detail)}></app-popover-confirm>
      </Host>
    );
  }
}
