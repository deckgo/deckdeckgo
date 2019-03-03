import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-extra-features'
})
export class AppDeckExtraFeatures {

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
        <main><h1 id="app-deck-extra-features-extra-features">Extra Features</h1>
<p>The <a href="https://deckdeckgo.com">DeckDeckGo</a> deck exposes a couple of extra features which could be added to your presentation too if you don&#39;t already use the starter kit.</p>
<h2 id="app-deck-extra-features-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-deck-extra-features-toggle-onoff-the-full-screen-mode">Toggle on/off the full screen mode</a></li>
<li><a href="#app-deck-extra-features-print-the-presentation">Print the presentation</a></li>
<li><a href="#app-deck-extra-features-mobile">Mobile</a></li>
</ul>
<h2 id="app-deck-extra-features-toggle-onoff-the-full-screen-mode">Toggle on/off the full screen mode</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.toggleFullScreen();</code>
    </deckgo-highlight-code><h2 id="app-deck-extra-features-print-the-presentation">Print the presentation</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.print();</code>
    </deckgo-highlight-code><h2 id="app-deck-extra-features-mobile">Mobile</h2>
<p>A util method to know if the current presentation is browsed on a mobile device or not.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.isMobile(); &#47;&#47; resolve a boolean</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
