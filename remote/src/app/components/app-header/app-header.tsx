import {Component, h} from '@stencil/core';

import themeStore from '../../stores/theme.store';

@Component({
  tag: 'app-header',
  styleUrl: 'app-header.scss'
})
export class AppHeader {
  render() {
    return (
      <ion-header>
        <ion-toolbar color={themeStore.state.darkTheme ? 'dark' : 'primary'}>
          <slot name="start">
            <ion-buttons slot="start">
              <ion-menu-toggle>
                <ion-button>
                  <ion-icon slot="icon-only" name="menu"></ion-icon>
                </ion-button>
              </ion-menu-toggle>
            </ion-buttons>
          </slot>

          <slot name="end"></slot>
        </ion-toolbar>
      </ion-header>
    );
  }
}
