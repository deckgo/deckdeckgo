import {Component, Element} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-edit-default'
})
export class AppEditDefault {

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
        <main><h1 id="app-edit-default-html">HTML</h1>
<p>To edit your <a href="https://deckdeckgo.com">DeckDeckGo</a> presentation your could either use <strong>HTML</strong> or <strong>Markdown</strong>.</p>
<p>The default language is <strong>HTML</strong>. It&#39;s also the recommended language as it will offers you more flexibility specially regarding styling.</p>
<p>If you use the starter kit and use HTML as language for the edition, you could begin to edit the <code>index.html</code> of your project as displayed in the chapter <a href="/slides/concept">Concept</a>.</p>
<p>If you wish to edit your talk using <strong>Markdown</strong> have a look to <a href="/edit/markdown">the next chapter</a>.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
