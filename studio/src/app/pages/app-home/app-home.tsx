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

        <main>
          <app-feed></app-feed>

          <app-popular></app-popular>
        </main>

      </ion-content>
    ];
  }
}
