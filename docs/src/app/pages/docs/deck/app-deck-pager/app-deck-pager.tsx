import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-pager'
})
export class AppDeckPager {

  @Element() el: HTMLElement;

  private menuService: MenuService;

  constructor() {
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

      <ion-content class="ion-padding">
        <main><h1 id="app-deck-pager-pager">Pager</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> will per default display a pager in form of a progress circle bar. It&#39;s possible to hide it or to customize the following various style options.</p>
<p>Also worth to notice that the pager inherits per default the document and deck direction (LTR or RTL for example).</p>
<ul>
<li><a href="#app-deck-pager-deck">Deck</a></li>
<li><a href="#app-deck-pager-customization">Customization</a></li>
<li><a href="#app-deck-pager-events">Events</a> </li>
</ul>
<h2 id="app-deck-pager-show-or-hide">Show or hide</h2>
<p>To show or hide the pager, a CSS4 variable has to be set on the <code>&lt;deckgo-deck&gt;</code> element. Two more options are also available when set on this element.</p>
<table>
<thead>
<tr>
<th>Attribute</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>--pager-display</td>
<td></td>
<td>The display property of the pager. Set to <code>none</code> to hide it.</td>
</tr>
<tr>
<td>--pager-position-left</td>
<td></td>
<td>The left attribute of the absolute positioning of the pager over the deck</td>
</tr>
<tr>
<td>--pager-position-right</td>
<td></td>
<td>The right attribute of the absolute positioning of the pager over the deck</td>
</tr>
</tbody></table>
<h2 id="app-deck-pager-customization">Customization</h2>
<p>The following style options are available to style the pager:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>--pager-size</td>
<td>56px</td>
<td></td>
</tr>
<tr>
<td>--pager-margin-top</td>
<td>8px</td>
<td></td>
</tr>
<tr>
<td>--pager-margin-end</td>
<td>8px</td>
<td></td>
</tr>
<tr>
<td>--pager-margin-bottom</td>
<td>8px</td>
<td></td>
</tr>
<tr>
<td>--pager-margin-start</td>
<td>8px</td>
<td></td>
</tr>
<tr>
<td>--pager-background</td>
<td>#eee</td>
<td></td>
</tr>
<tr>
<td>--pager-text-color</td>
<td>#4c8dff</td>
<td></td>
</tr>
<tr>
<td>--pager-text-size</td>
<td>0.5em</td>
<td></td>
</tr>
<tr>
<td>--pager-stroke-outer-width</td>
<td>2.8</td>
<td></td>
</tr>
<tr>
<td>--pager-stroke-inner-width</td>
<td>1.8</td>
<td></td>
</tr>
<tr>
<td>--pager-text-percentage-display</td>
<td>hidden</td>
<td>Set to <code>block</code> to display a progression with percentage (for example: 35%)</td>
</tr>
<tr>
<td>--pager-text-slides-display</td>
<td>hidden</td>
<td>Set to <code>block</code> to display a progression as slides&#39; count (for example: 2/15)</td>
</tr>
</tbody></table>
<p>Note: of course if you would display both <code>--pager-text-percentage-display</code> and <code>--pager-text-slides-display</code> it would be a weird display, use just one at once.</p>
<h1 id="app-deck-pager-events">Events</h1>
<p>In case you would like to hook on the pager click, it triggers the following event:</p>
<table>
<thead>
<tr>
<th>Event</th>
<th>Emitted value</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>pagerClick</td>
<td></td>
<td>Emitted when the user click on the pager.</td>
</tr>
</tbody></table>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
