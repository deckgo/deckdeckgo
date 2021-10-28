import {Component, EventEmitter, h, Host, Event} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';
import {AppIcon} from '../../../core/app-icon/app-icon';

@Component({
  tag: 'app-close-menu',
  styleUrl: 'app-close-menu.scss'
})
export class AppCloseMenu {
  @Event() private close: EventEmitter<void>;

  render() {
    return (
      <Host tabindex={0}>
        <button class="ion-activatable" onClick={() => this.close.emit()}>
          <ion-ripple-effect></ion-ripple-effect>
          <slot>
            <AppIcon name="close" ariaLabel={i18n.state.core.close}></AppIcon>
          </slot>
        </button>
      </Host>
    );
  }
}
