import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-installation',
  styleUrl: 'app-installation.scss'
})
export class AppInstallation {

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
        <main><h1 id="app-installation-installation">Installation</h1>
<p>To create easily your PWA presentation and to enjoy all the options, I suggest you to create your slides using the CLI and the starter kit as described in the <a href="/docs/introduction">previous chapter</a>.</p>
<p>However, the <a href="https://deckdeckgo.com">DeckDeckGo</a> core component, could be installed in any project too.</p>
<p>If you wish to do so, use it directly in your project from a CDN, using a simple script include, or install it from <a href="https://www.npmjs.com/package/deckdeckgo">npm</a>.</p>
<h2 id="app-installation-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-installation-using-deckdeckgo-from-a-cdn">Using DeckDeckGo from a CDN</a></li>
<li><a href="#app-installation-install-deckdeckgo-from-npm">Install DeckDeckGo from npm</a></li>
<li><a href="#app-installation-framework-integration">Framework integration</a></li>
</ul>
<h2 id="app-installation-using-deckdeckgo-from-a-cdn">Using DeckDeckGo from a CDN</h2>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script src=&quot;https:&#47;&#47;unpkg.com&#47;deckdeckgo@latest&#47;dist&#47;deckdeckgo.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h2 id="app-installation-install-deckdeckgo-from-npm">Install DeckDeckGo from NPM</h2>
<p>Install <a href="https://deckdeckgo.com">DeckDeckGo</a> in your project from <a href="https://www.npmjs.com/package/deckdeckgo">npm</a> using the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install deckdeckgo</code>
    </deckgo-highlight-code><h2 id="app-installation-framework-integration">Framework integration</h2>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
