import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-auto-slide',
})
export class AppDeckAutoSlide {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-auto-slide-auto-slide">Auto Slide</h1>
          <p>Set a presentation to swipe slides automatically after X seconds.</p>
          <p>Per default, the auto slide interval is set to 5000 ms</p>
          <h2 id="app-deck-auto-slide-properties">Properties</h2>
          <p>
            This new options comes with two new properties which can be optionally set on the deck (<code>&lt;deckgo-deck/&gt;</code>) level.
          </p>
          <table>
            <thead>
              <tr>
                <th>Property</th>
                <th>Attribute</th>
                <th>Description</th>
                <th>Type</th>
                <th>Default</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <code>autoSlide</code>
                </td>
                <td>
                  <code>auto-slide</code>
                </td>
                <td>Eneable auto slide</td>
                <td>boolean</td>
                <td>false</td>
              </tr>
              <tr>
                <td>
                  <code>autoSlideInterval</code>
                </td>
                <td>
                  <code>auto-slide-interval</code>
                </td>
                <td>The interval (in milliseconds) on how often to swipe the slides</td>
                <td>number</td>
                <td>5000</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
