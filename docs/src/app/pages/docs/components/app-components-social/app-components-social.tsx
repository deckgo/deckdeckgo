import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-components-social'
})
export class AppComponentsSocial {

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
        <main><h1 id="app-components-social-social">Social</h1>
<p>The &quot;Social&quot; component allows you to easily add a social link to your presentation.</p>
<h2 id="app-components-social-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#showcase">Showcase</a></li>
<li><a href="#usage">Usage</a><ul>
<li><a href="#slots">Slots</a></li>
<li><a href="#attributes">Attributes</a></li>
<li><a href="#examples">Examples</a></li>
</ul>
</li>
</ul>
<h2 id="app-components-social-showcase">Showcase</h2>
<p>
  <deckgo-social twitter="daviddalbusco"><ion-icon slot="icon" name="logo-twitter"></ion-icon> Twitter</deckgo-social>
</p>

<p>
  <deckgo-social github="fluster/deckdeckgo"><ion-icon slot="icon" name="logo-github"></ion-icon> DeckDeckGo on Github</deckgo-social>
</p>

<h2 id="app-components-social-usage">Usage</h2>
<p>The &quot;Social&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-social/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'}  &lt;img data-src=&quot;&#47;assets&#47;twitter.svg&quot; slot=&quot;icon&quot;&#47;&gt;{'\n'}  Twitter{'\n'}&lt;&#47;deckgo-social&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-social-slots">Slots</h3>
<p>The slot <code>icon</code> and the text are both optional. Of course, if you provide nothing, nothing will be rendered.</p>
<h3 id="app-components-social-attributes">Attributes</h3>
<p>This component offers the following options which could be set using attributes:</p>
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
<td>twitter</td>
<td>string</td>
<td></td>
<td>Your Twitter username. It will be concatenated automatically with <code>https://twitter.com/</code></td>
</tr>
<tr>
<td>linkedin</td>
<td>string</td>
<td></td>
<td>Your Linkedin username. It will be concatenated automatically with <code>https://www.linkedin.com/in/</code></td>
</tr>
<tr>
<td>medium</td>
<td>string</td>
<td></td>
<td>Your Medium username. It will be concatenated automatically with <code>https://medium.com/@</code></td>
</tr>
<tr>
<td>github</td>
<td>string</td>
<td></td>
<td>Your Github username. It will be concatenated automatically with <code>https://github.com/</code></td>
</tr>
<tr>
<td>fullUrl</td>
<td>string</td>
<td></td>
<td>In case you would like to provide the URI of your choice</td>
</tr>
</tbody></table>
<h3 id="app-components-social-examples">Examples</h3>
<p>Without any icons:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;Twitter&lt;&#47;deckgo-social&gt;{'\n'}&lt;deckgo-social linkedin=&quot;david-dal-busco&#47;&quot;&gt;Linkedin&lt;&#47;deckgo-social&gt;{'\n'}&lt;deckgo-social medium=&quot;david.dalbusco&quot;&gt;Medium&lt;&#47;deckgo-social&gt;{'\n'}&lt;deckgo-social full-url=&quot;https:&#47;&#47;stackoverflow.com&#47;users&#47;5404186&#47;peter-parker&quot;&gt;Stackoverflow&lt;&#47;deckgo-social&gt;</code>
    </deckgo-highlight-code><p>With for example <code>ion-icon</code>:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'}  &lt;ion-icon slot=&quot;icon&quot; name=&quot;logo-twitter&quot;&gt;&lt;&#47;ion-icon&gt;{'\n'}  Twitter{'\n'}&lt;&#47;deckgo-social&gt;{'\n'}{'\n'}&lt;deckgo-social github=&quot;fluster&#47;deckdeckgo&quot;&gt;{'\n'}  &lt;ion-icon slot=&quot;icon&quot; name=&quot;logo-github&quot;&gt;&lt;&#47;ion-icon&gt;{'\n'}  DeckDeckGo on Github{'\n'}&lt;&#47;deckgo-social&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
