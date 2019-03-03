import {Component, Element} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-edit-lazy-loading'
})
export class AppEditLazyLoading {

  @Element() el: HTMLElement;

  constructor(private menuService: MenuService) {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content padding>
        <main><h1 id="app-edit-lazy-loading-lazy-loading">Lazy loading</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> is build for performance and try to lazy load as much as possible the components of your presentation, have a look to the Lighthouse score of the <a href="https://deckdeckgo.com">DeckDeckGo</a> website for reference.</p>
<p>Therefore, in order to lazy load the images of your presentation, please provide their url using the attribute <code>data-src</code> instead of <code>src</code>. <a href="https://deckdeckgo.com">DeckDeckGo</a> will then take care of loading them only when needed.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;img data-src=&quot;https:&#47;&#47;deckdeckgo.com&#47;assets&#47;img&#47;deckdeckgo.png&quot;&#47;&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
