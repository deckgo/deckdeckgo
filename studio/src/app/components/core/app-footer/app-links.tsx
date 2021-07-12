import {Component, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import { AppIcon } from '../app-icon/app-icon';

@Component({
  tag: 'app-links',
  styleUrl: 'app-links.scss',
  shadow: false
})
export class AppLinks {
  render() {
    return (
      <div class={`ion-padding-start ion-padding-end ion-padding-bottom links-container links-menu`}>
        <section>
          <ion-label class="ion-padding-top">DeckDeckGo</ion-label>

          <a href="https://deckdeckgo.com/en/" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.home}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/discover" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.discover}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/enterprise" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.enterprise}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/about" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.about}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/team" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.team}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/newsletter" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.newsletter}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/contact" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.contact}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/press" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.press}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/faq" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.faq}</ion-label>
          </a>
        </section>

        <section>
          <ion-label class="ion-padding-top">{i18n.state.links.developers}</ion-label>

          <a href="https://deckdeckgo.com/en/opensource" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.open_source}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/services" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.services}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/en/developer" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.developer}</ion-label>
          </a>
        </section>

        <section>
          <ion-label class="ion-padding-top">{i18n.state.links.terms}</ion-label>

          <a href="https://deckdeckgo.com/terms" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.terms_of_use}</ion-label>
          </a>

          <a href="https://deckdeckgo.com/privacy" rel="noopener norefferer" target="_blank">
            <ion-label>{i18n.state.links.privacy_policy}</ion-label>
          </a>
        </section>

        <div class="social ion-padding-top ion-margin-top">
          <a href="https://twitter.com/deckdeckgo" rel="noopener norefferer" target="_blank" aria-label="Twitter">
            <AppIcon name="twitter" ariaLabel="" ariaHidden={true}></AppIcon>
          </a>
          <a href="https://github.com/deckgo" rel="noopener noreferrer" aria-label="GitHub">
            <AppIcon name="github" ariaLabel="" ariaHidden={true}></AppIcon>
          </a>
          <a
            aria-label="Slack"
            href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
            rel="noopener noreferrer">
            <AppIcon name="slack" ariaLabel="" ariaHidden={true}></AppIcon>
          </a>
        </div>
      </div>
    );
  }
}
