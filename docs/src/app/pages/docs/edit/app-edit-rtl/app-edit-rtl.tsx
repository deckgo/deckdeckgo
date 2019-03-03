import {Component, Element} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-edit-rtl'
})
export class AppEditRtl {

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
        <main><h1 id="app-edit-rtl-rtl-support">RTL Support</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> offers full LTR and RTL support. The deck inherits its parent text direction.</p>
<p>Commonly, if you wish to use RTL for your all page respectively presentation, you could set the attribute <code>dir</code> of the root <code>html</code> tag to <code>rtl</code>.    </p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;!DOCTYPE html&gt;{'\n'}&lt;html dir=&quot;rtl&quot;&gt;{'\n'}&lt;body&gt;{'\n'}  &lt;deckgo-deck&gt;{'\n'}  &lt;&#47;deckgo-deck&gt;{'\n'}&lt;&#47;body&gt;{'\n'}&lt;&#47;html&gt;  </code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
