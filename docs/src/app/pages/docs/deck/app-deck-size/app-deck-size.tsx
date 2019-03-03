import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-size'
})
export class AppDeckSize {

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
        <main><h1 id="app-deck-size-size">Size</h1>
<p>Per default, the <a href="https://deckdeckgo.com">DeckDeckGo</a> deck will use all the browser <code>window</code> size respectively width and height.</p>
<p>However, it is possible to include or use <a href="https://deckdeckgo.com">DeckDeckGo</a> in any container, for that purpose you would only need to set the attribute <code>embedded</code> to <code>true</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;div style=&quot;width: 500px; height: 400px;&quot;&gt;{'\n'}  &lt;deckgo-deck embedded=&quot;true&quot;&gt;{'\n'}    &lt;deckgo-slide-title&gt;{'\n'}      &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}      &lt;p slot=&quot;content&quot;&gt;{'\n'}        Hello World 🚀{'\n'}      &lt;&#47;p&gt;{'\n'}    &lt;&#47;deckgo-slide-title&gt;{'\n'}  &lt;&#47;deckgo-deck&gt;{'\n'}&lt;&#47;div&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
