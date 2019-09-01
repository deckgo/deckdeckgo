import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-misc-open-source',
  styleUrl: 'app-misc-open-source.scss'
})
export class AppMiscOpenSource {

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-misc-open-source-open-source">Open source</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> is <strong>open source</strong> and its source code could be found on <a href="https://github.com/deckgo/deckdeckgo">Github</a>.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
