import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-pager'
})
export class AppDeckPager {

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
        <main><h1 id="app-deck-pager-pager">Pager</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> will per default display a pager in form of a progress circle bar. It&#39;s possible to hide it or to customize the following various style options.</p>
<p>Also worth to notice that the pager inherits per default the document and deck direction (LTR or RTL for example).</p>
<ul>
<li><a href="#app-deck-pager-show-or-hide">Show or hide</a></li>
<li><a href="#app-deck-pager-customization">Customization</a></li>
</ul>
<h2 id="app-deck-pager-show-or-hide">Show or hide</h2>
<p>The show or hide options of the pager are available on the <code>&lt;deckgo-deck&gt;</code> element.</p>
<table>
<thead>
<tr>
<th>Attribute</th>
<th>Type</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>pager</td>
<td>boolean</td>
<td>true</td>
<td>Show or hide the pager</td>
</tr>
<tr>
<td>pagerPercentage</td>
<td>boolean</td>
<td>true</td>
<td>Show or hide the progression in percentage inside the pager</td>
</tr>
</tbody></table>
<h2 id="app-deck-pager-customization">Customization</h2>
<p>The following style options are available to style the pager:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
</tr>
</thead>
<tbody><tr>
<td>--pager-size</td>
<td>56px</td>
</tr>
<tr>
<td>--pager-margin-top</td>
<td>8px</td>
</tr>
<tr>
<td>--pager-margin-end</td>
<td>8px</td>
</tr>
<tr>
<td>--pager-margin-bottom</td>
<td>8px</td>
</tr>
<tr>
<td>--pager-margin-start</td>
<td>8px</td>
</tr>
<tr>
<td>--pager-background</td>
<td>#eee</td>
</tr>
<tr>
<td>--pager-text-color</td>
<td>#4c8dff</td>
</tr>
<tr>
<td>--pager-text-size</td>
<td>0.5em</td>
</tr>
<tr>
<td>--pager-stroke-outer-width</td>
<td>2.8</td>
</tr>
<tr>
<td>--pager-stroke-inner-width</td>
<td>1.8</td>
</tr>
<tr>
<td>--pager-position-left</td>
<td></td>
</tr>
<tr>
<td>--pager-position-right</td>
<td></td>
</tr>
</tbody></table>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
