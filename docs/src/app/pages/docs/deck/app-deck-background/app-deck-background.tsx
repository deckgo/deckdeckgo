import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-background'
})
export class AppDeckBackground {

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
        <main><h1 id="app-deck-background-background">Background</h1>
<p>Beside slides and templates, a <a href="https://deckdeckgo.com">DeckDeckGo</a> deck could also contains a customized element <code>background</code> which could be injected using a dedicated <code>slot</code>.</p>
<h2 id="app-deck-background-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-deck-background-introduction">Introduction</a></li>
<li><a href="#app-deck-background-attributes">Attributes</a></li>
<li><a href="#app-deck-background-theming">Theming</a></li>
<li><a href="#app-deck-background-examples">Examples</a></li>
</ul>
<h2 id="app-deck-background-introduction">Introduction</h2>
<p>The slot <code>background</code> provided for the deck will be cloned into each slides of your presentation.</p>
<p>This slot is particularly useful if you wish to display your brand or company logo on each slides.</p>
<p>Also worth to notice, this slot will be <code>hidden</code> when the presentation will be displayed full screen.</p>
<p>Optionally, if you wish, this slot could also not be cloned, could be useful in case you rather would like to display a background which follows your entire presentation.</p>
<h2 id="app-deck-background-attributes">Attributes</h2>
<p>The following attribute could be applied to the <code>deckgo-deck</code> element:</p>
<table>
<thead>
<tr>
<th>Property</th>
<th>Attribute</th>
<th>Mandatory</th>
<th>Description</th>
<th>Type</th>
<th>Default</th>
</tr>
</thead>
<tbody><tr>
<td><code>cloneBackground</code></td>
<td><code>clone-background</code></td>
<td></td>
<td>Set to false in case you don&#39;t want to clone the background in each slides</td>
<td><code>boolean</code></td>
<td>true</td>
</tr>
</tbody></table>
<h2 id="app-deck-background-theming">Theming</h2>
<p>The following theming options will affect the slot <code>background</code> if set on the <code>deckgo-deck</code> or any slides.</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--slide-background-position</td>
<td>absolute</td>
<td>The position of the background</td>
</tr>
<tr>
<td>--slide-background-top</td>
<td>0</td>
<td>Top value</td>
</tr>
<tr>
<td>--slide-background-end</td>
<td></td>
<td>In LTR, right value</td>
</tr>
<tr>
<td>--slide-background-start</td>
<td>0</td>
<td>In LTR, left value</td>
</tr>
<tr>
<td>--slide-background-width</td>
<td></td>
<td>A background width, default without being set all width</td>
</tr>
<tr>
<td>--slide-background-height</td>
<td></td>
<td>A background height, default without being set all height</td>
</tr>
<tr>
<td>--slide-background-print-display</td>
<td>none</td>
<td>Don&#39;t print per default the background</td>
</tr>
</tbody></table>
<h2 id="app-deck-background-examples">Examples</h2>
<p>An example with an image cloned as background for each slides:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;p slot=&quot;content&quot;&gt;{'\n'}      Hello World 🚀{'\n'}    &lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}{'\n'}  &lt;img slot=&quot;background&quot; data-src=&quot;https:&#47;&#47;deckdeckgo.com&#47;assets&#47;img&#47;deckdeckgo.png&quot;&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><p>An example with a more complex html and css element which is set as background for the all presentation:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck clone-background=&quot;false&quot;&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;p slot=&quot;content&quot;&gt;{'\n'}      Hello World 🚀{'\n'}    &lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}{'\n'}  &lt;div class=&quot;circle&quot; slot=&quot;background&quot;&gt;&lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><p>where for example the related <code>circle</code> css code could be:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">div.circle &#123;{'\n'}  position: absolute;{'\n'}  left: 50%;{'\n'}  transform: translate(-50%, 0);{'\n'}{'\n'}  bottom: -10vh;{'\n'}  width: 800vw;{'\n'}  height: 100vh;{'\n'}{'\n'}  border-radius: 50%;{'\n'}  background: yellow;{'\n'}  opacity: 0.3;{'\n'}&#125;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
