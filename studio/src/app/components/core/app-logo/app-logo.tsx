import {Component} from '@stencil/core';

@Component({
  tag: 'app-logo',
  styleUrl: 'app-logo.scss',
  shadow: true
})
export class AppLogo {

  render() {
    return <div>
      <ion-icon src="/assets/img/deckdeckgo-logo-round.svg"></ion-icon>
    </div>;
  }
}
