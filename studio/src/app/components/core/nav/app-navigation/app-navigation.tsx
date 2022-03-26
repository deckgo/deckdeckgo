import {isIOS} from '@deckdeckgo/utils';
import {Component, h, Host, Prop, State} from '@stencil/core';
import i18n from '../../../../stores/i18n.store';
import {AppIcon} from '../../app-icon/app-icon';

@Component({
  tag: 'app-navigation',
  styleUrl: 'app-navigation.scss',
  shadow: false
})
export class AppNavigation {
  @Prop() actions: 'all' | 'none' | 'editor-less' = 'editor-less';

  @State()
  private hideIC: boolean = localStorage.getItem('deckgo-hide-announcement-ic') !== null;

  render() {
    return (
      <Host>
        {this.renderICP()}
        <ion-header>
          <ion-toolbar>
            {this.renderStart()}
            {this.renderEnd()}
          </ion-toolbar>
        </ion-header>
      </Host>
    );
  }

  private renderStart() {
    return (
      <ion-buttons slot="start">
        <ion-menu-toggle>
          <ion-button aria-label={i18n.state.nav.menu}>
            <AppIcon name="menu" ariaHidden={true} ariaLabel="" slot="icon-only"></AppIcon>
          </ion-button>
        </ion-menu-toggle>

        {this.actions === 'all' ? <app-navigation-start></app-navigation-start> : undefined}
      </ion-buttons>
    );
  }

  private renderEnd() {
    if (this.actions === 'none') {
      return undefined;
    }

    return (
      <ion-buttons slot="end">
        <app-navigation-end></app-navigation-end>
      </ion-buttons>
    );
  }

  private renderICP() {
    if (this.hideIC) {
      return undefined;
    }

    return (
      <header class={`ic ${isIOS() ? 'ios' : 'md'}`}>
        <ion-button
          fill="clear"
          color="light"
          onClick={() => {
            this.hideIC = true;
            localStorage.setItem('deckgo-hide-announcement-ic', 'true');
          }}>
          <AppIcon name="close" ariaLabel={i18n.state.core.close}></AppIcon>
        </ion-button>

        <p>
          <a
            href="https://medium.com/geekculture/bye-bye-amazon-google-hello-web-3-0-b01bfe8f8783"
            rel="noopener norefferer"
            target="_blank">
            We are porting DeckDeckGo to DFINITY’s Internet Computer.
          </a>
        </p>
      </header>
    );
  }
}
