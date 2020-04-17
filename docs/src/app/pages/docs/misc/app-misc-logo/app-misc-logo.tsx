import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-misc-logo',
})
export class AppMiscLogo {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-misc-logo-logo">Logo</h1>
          <p>
            The <a href="https://deckdeckgo.com">DeckDeckGo</a> logo was designed and offered by <a href="mailto:hello@skinque.com">Anita</a> from{' '}
            <a href="http://skinque.com">Skinque.com</a>, a great online marketplace for tattoos 🤘
          </p>
          <p>Reach her out if you are looking for a cool custom tattoo or a nice logo 😃</p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
