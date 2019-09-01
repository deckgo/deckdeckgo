import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-installation',
  styleUrl: 'app-installation.scss'
})
export class AppInstallation {

  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-installation-installation">Installation</h1>
<p>To create easily your PWA presentation and to enjoy all the options, I suggest you to create your slides using the CLI and the starter kit as described in the <a href="/docs/introduction">previous chapter</a>.</p>
<p>However, the <a href="https://deckdeckgo.com">DeckDeckGo</a> core component, could be installed in any project too.</p>
<p>If you wish to do so, use it directly in your project from a CDN, using a simple script include, or install it from <a href="https://www.npmjs.com/package/@deckdeckgo/core">npm</a>.</p>
<blockquote>
<p>Installing the core component as displayed below will &quot;only&quot; install the &quot;engine&quot; of <a href="https://deckdeckgo.com">DeckDeckGo</a> respectively its core doesn&#39;t contains any slides.</p>
<p>Splitting the core and the templates has for goal to reduce as much as possible the dependencies and amount of external code needed in your project. Therefore, it has for ultimate goal to unleash the best performances for your presentation.</p>
</blockquote>
<h2 id="app-installation-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-installation-using-deckdeckgo-from-a-cdn">Using DeckDeckGo from a CDN</a></li>
<li><a href="#app-installation-install-deckdeckgo-from-npm">Install DeckDeckGo from npm</a></li>
<li><a href="#app-installation-framework-integration">Framework integration</a></li>
</ul>
<h2 id="app-installation-using-deckdeckgo-from-a-cdn">Using DeckDeckGo from a CDN</h2>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> core from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script type=&quot;module&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;core@latest&#47;dist&#47;deckdeckgo&#47;deckdeckgo.esm.js&quot;&gt;&lt;&#47;script&gt;{'\n'}&lt;script nomodule=&quot;&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;core@latest&#47;dist&#47;deckdeckgo&#47;deckdeckgo.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h2 id="app-installation-install-deckdeckgo-from-npm">Install DeckDeckGo from NPM</h2>
<p>Install <a href="https://deckdeckgo.com">DeckDeckGo</a> in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/core">npm</a> using the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install @deckdeckgo&#47;core</code>
    </deckgo-highlight-code><h2 id="app-installation-framework-integration">Framework integration</h2>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
<p>That being said, commonly, you might either <code>import</code> or <code>load</code> the component:</p>
<h3 id="app-installation-import">Import</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#039;@deckdeckgo&#47;core&#039;;</code>
    </deckgo-highlight-code><h3 id="app-installation-loader">Loader</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#123; defineCustomElements as deckDeckGoElements &#125; from &#039;@deckdeckgo&#47;core&#47;dist&#47;loader&#039;;{'\n'}deckDeckGoElements(window);</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
