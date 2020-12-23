import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-links',
  styleUrl: 'app-links.scss',
  shadow: false,
})
export class AppLinks {
  render() {
    return (
      <div class={`ion-padding-start ion-padding-end ion-padding-bottom links-container links-menu`}>
        <section>
          <ion-label class="ion-padding-top">DeckDeckGo</ion-label>

          <a href="https://deckdeckgo.com/discover" rel="noopener norefferer" target="_blank">
            <ion-label>Discover</ion-label>
          </a>

          <a href="https://deckdeckgo.com/enterprise" rel="noopener norefferer" target="_blank">
            <ion-label>Enterprise</ion-label>
          </a>

          <a href="https://deckdeckgo.com/about" rel="noopener norefferer" target="_blank">
            <ion-label>About</ion-label>
          </a>

          <a href="https://deckdeckgo.com/team" rel="noopener norefferer" target="_blank">
            <ion-label>Team</ion-label>
          </a>

          <a href="https://deckdeckgo.com/newsletter" rel="noopener norefferer" target="_blank">
            <ion-label>Newsletter</ion-label>
          </a>

          <a href="https://deckdeckgo.com/contact" rel="noopener norefferer" target="_blank">
            <ion-label>Contact</ion-label>
          </a>

          <a href="https://deckdeckgo.com/press" rel="noopener norefferer" target="_blank">
            <ion-label>Press</ion-label>
          </a>

          <a href="https://deckdeckgo.com/faq" rel="noopener norefferer" target="_blank">
            <ion-label>FAQ</ion-label>
          </a>
        </section>

        <section>
          <ion-label class="ion-padding-top">Developers</ion-label>

          <a href="https://deckdeckgo.com/opensource" rel="noopener norefferer" target="_blank">
            <ion-label>Open source</ion-label>
          </a>

          <a href="https://deckdeckgo.com/services" rel="noopener norefferer" target="_blank">
            <ion-label>Services</ion-label>
          </a>

          <a href="https://deckdeckgo.com/developer" rel="noopener norefferer" target="_blank">
            <ion-label>Developer</ion-label>
          </a>
        </section>

        <section>
          <ion-label class="ion-padding-top">Terms</ion-label>

          <a href="https://deckdeckgo.com/terms" rel="noopener norefferer" target="_blank">
            <ion-label>Terms of use</ion-label>
          </a>

          <a href="https://deckdeckgo.com/privacy" rel="noopener norefferer" target="_blank">
            <ion-label>Privacy Policy</ion-label>
          </a>
        </section>

        <div class="social ion-padding-top ion-margin-top">
          <a href="https://twitter.com/deckdeckgo" rel="noopener norefferer" target="_blank">
            <ion-icon name="logo-twitter"></ion-icon>
          </a>
          <a href="https://github.com/deckgo" rel="noopener noreferrer">
            <ion-icon name="logo-github"></ion-icon>
          </a>
          <a
            href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
            rel="noopener noreferrer">
            <ion-icon name="logo-slack"></ion-icon>
          </a>
        </div>
      </div>
    );
  }
}
