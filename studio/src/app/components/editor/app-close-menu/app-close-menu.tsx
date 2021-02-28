import {Component, EventEmitter, h, Host, Event} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-close-menu',
  styleUrl: 'app-close-menu.scss',
})
export class AppCloseMenu {
  @Event() private close: EventEmitter<void>;

  render() {
    return (
      <Host tabindex={0}>
        <button class="ion-activatable" onClick={() => this.close.emit()}>
          <ion-ripple-effect></ion-ripple-effect>
          <slot>
            <ion-icon aria-label={i18n.state.core.close} src="/assets/icons/ionicons/close.svg"></ion-icon>
          </slot>
        </button>
      </Host>
    );
  }
}
