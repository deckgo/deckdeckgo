import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-default',
})
export class AppEditDefault {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-edit-default-html">HTML</h1>
          <p>
            A <a href="https://deckdeckgo.com">DeckDeckGo</a> presentation shall be edited in <strong>HTML</strong>.
          </p>
          <p>
            Slides and content have to written in the main <code>index.html</code> file of your project.
          </p>
          <p>
            Checkout the chapter <a href="/slides/concept">Concept</a> for more information about the structure.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
