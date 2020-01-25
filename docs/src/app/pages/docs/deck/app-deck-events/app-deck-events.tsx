import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-events'
})
export class AppDeckEvents {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-events-events">Events</h1>
          <p>
            The <a href="https://deckdeckgo.com">DeckDeckGo</a> deck triggers the following events:
          </p>
          <table>
            <thead>
              <tr>
                <th>Event</th>
                <th>Emitted value</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>slidesDidLoad</td>
                <td>string[]</td>
                <td>Emitted when the deck and all slides have loaded. Emit the an ordered list of all the tag names of the slides.</td>
              </tr>
              <tr>
                <td>slideNextDidAnimate</td>
                <td>void</td>
                <td>Emitted when the slide didn&#39;t change but an animation occurred towards the next slide.</td>
              </tr>
              <tr>
                <td>slidePrevDidAnimate</td>
                <td>void</td>
                <td>Emitted when the slide didn&#39;t change but an animation occurred towards the previous slide.</td>
              </tr>
              <tr>
                <td>slideNextDidChange</td>
                <td>number</td>
                <td>Emitted when the next slide has started. Emit the index of the new active slide.</td>
              </tr>
              <tr>
                <td>slidePrevDidChange</td>
                <td>number</td>
                <td>Emitted when the previous slide has ended. Emit the index of the new active slide.</td>
              </tr>
              <tr>
                <td>slideToChange</td>
                <td>number</td>
                <td>Emitted when a specific slide has been selected. Emit the index of the new selected slide.</td>
              </tr>
              <tr>
                <td>slideDrag</td>
                <td>number</td>
                <td>Emitted when the slider is actively being moved. Emit the transformX value of the deck.</td>
              </tr>
              <tr>
                <td>slideWillChange</td>
                <td>number</td>
                <td>Emitted before the active slide has changed. Emit the transformX value of the deck.</td>
              </tr>
              <tr>
                <td>deckDidLoad</td>
                <td></td>
                <td>
                  Emitted after <code>slidesDidLoad</code> when all slides have been processed by the core (lazy loading, clone actions, etc.)
                </td>
              </tr>
              <tr>
                <td>mouseInactivity</td>
                <td>boolean</td>
                <td>Emitted when the mouse is idle and will be hidden.</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
