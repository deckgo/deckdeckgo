import {Component} from '@stencil/core';

@Component({
  tag: 'app-home',
  styleUrl: 'app-home.scss'
})
export class AppHome {

  render() {
    return [
      <app-navigation></app-navigation>,
      <ion-content padding>

        <h1>Hello World, I'm DeckDeckGo Studio</h1>

      </ion-content>
    ];
  }
}
