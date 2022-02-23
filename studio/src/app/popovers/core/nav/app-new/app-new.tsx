import {Component, ComponentInterface, Element, h, Host, State} from '@stencil/core';
import {AppIcon} from '../../../../components/core/app-icon/app-icon';
import {EnvironmentAppConfig, EnvironmentAppConfigFeature} from '../../../../config/environment-config';
import {EnvironmentConfigService} from '../../../../services/environment/environment-config.service';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-new',
  styleUrl: 'app-new.scss'
})
export class AppNew implements ComponentInterface {
  @Element() el: HTMLElement;

  @State()
  private type: 'deck' | 'doc' | undefined;

  private features: EnvironmentAppConfigFeature[] = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app').features;

  async closePopover(confirm: boolean) {
    if (!confirm) {
      await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
      return;
    }

    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(this.type || this.features[0]);
  }

  render() {
    return (
      <Host>
        {this.renderTypes()}

        <p>{i18n.state.tools.new_warning_text}</p>

        <app-popover-confirm onConfirm={({detail}: CustomEvent<boolean>) => this.closePopover(detail)}></app-popover-confirm>
      </Host>
    );
  }

  private renderTypes() {
    if (this.features.length === 1) {
      return undefined;
    }

    return (
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
    );
  }
}
