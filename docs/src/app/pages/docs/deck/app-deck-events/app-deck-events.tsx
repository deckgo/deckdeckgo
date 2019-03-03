import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-events'
})
export class AppDeckEvents {

  @Element() el: HTMLElement;

  constructor(private menuService: MenuService) {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content padding>
        <main><h1 id="app-deck-events-events">Events</h1>
<p>The <a href="https://deckdeckgo.com">DeckDeckGo</a> deck triggers the following events:</p>
<table>
<thead>
<tr>
<th>Event</th>
<th>Emitted value</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>slidesDidLoad</td>
<td>string[]</td>
<td>Emitted when the deck and all slides have loaded. Emit the an ordered list of all the tag names of the slides.</td>
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
</tbody></table>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
