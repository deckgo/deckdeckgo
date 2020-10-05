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
          <p>The presentations can be configured to swipe slides automatically, without any user inputs. </p>
          <p>Per default, if activated, the auto slide interval is set to 5000 ms</p>
          <h2 id="app-deck-auto-slide-properties">Properties</h2>
          <p>
            Two options are available to make the decks step through slides automatically. The properties have to be set on the root element{' '}
            <code>&lt;deckgo-deck/&gt;</code>.
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
                <td>Enable auto slide</td>
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
                <td>The interval provided in milliseconds)</td>
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
