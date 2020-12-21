import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-links',
  styleUrl: 'app-links.scss',
  shadow: false,
})
export class AppLinks {
  render() {
    return (
      <div class={`ion-padding-start ion-padding-end ion-margin-bottom footer-container footer-menu`}>
        <section>
          <ion-label class="ion-padding-top">DeckDeckGo</ion-label>

          <ion-router-link href="/about" routerDirection="forward">
            <ion-label>About</ion-label>
          </ion-router-link>

          <ion-router-link href="/team" routerDirection="forward">
            <ion-label>Team</ion-label>
          </ion-router-link>

          <ion-router-link href="/newsletter" routerDirection="forward">
            <ion-label>Newsletter</ion-label>
          </ion-router-link>

          <ion-router-link href="/contact" routerDirection="forward">
            <ion-label>Contact</ion-label>
          </ion-router-link>

          <ion-router-link href="/press" routerDirection="forward">
            <ion-label>Press</ion-label>
          </ion-router-link>

          <ion-router-link href="/faq" routerDirection="forward">
            <ion-label>FAQ</ion-label>
          </ion-router-link>
        </section>

        <section>
          <ion-label class="ion-padding-top">Interact</ion-label>

          <ion-router-link href="/poll" routerDirection="forward">
            <ion-label>Poll</ion-label>
          </ion-router-link>

          <ion-router-link href="/remote" routerDirection="forward">
            <ion-label>Remote control</ion-label>
          </ion-router-link>
        </section>

        <section>
          <ion-label class="ion-padding-top">Developers</ion-label>

          <ion-router-link href="/opensource" routerDirection="forward">
            <ion-label>Open source</ion-label>
          </ion-router-link>

          <ion-router-link href="/services" routerDirection="forward">
            <ion-label>Services</ion-label>
          </ion-router-link>

          <ion-router-link href="/developer" routerDirection="forward">
            <ion-label>Developer</ion-label>
          </ion-router-link>
        </section>

        <section>
          <ion-label class="ion-padding-top">Terms</ion-label>

          <ion-router-link href="/terms" routerDirection="forward">
            <ion-label>Terms of use</ion-label>
          </ion-router-link>

          <ion-router-link href="/privacy" routerDirection="forward">
            <ion-label>Privacy Policy</ion-label>
          </ion-router-link>
        </section>

        <div class="social ion-padding-top ion-margin-top">
          <a href="https://twitter.com/deckdeckgo" rel="noopener norefferer">
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
