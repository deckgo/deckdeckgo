import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-components-highlight-code'
})
export class AppComponentsHighlightCode {

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
        <main><h1 id="app-components-highlight-code-highlight-code">Highlight Code</h1>
<p>The &quot;Highlight Code&quot; component is an extra component which let you highlight code easily.</p>
<p>To highlight your code, this component is using <a href="https://prismjs.com">Prism.js</a> from <a href="https://twitter.com/jamesdigioia">James DiGioia</a>.</p>
<h2 id="app-components-highlight-code-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-components-highlight-code-showcase">Showcase</a></li>
<li><a href="#app-components-highlight-code-getting-started">Getting started</a><ul>
<li><a href="#app-components-highlight-code-using-deckdeckgo-highlight-code-from-a-cdn">Using DeckDeckGo Highlight Code from a CDN</a></li>
<li><a href="#app-components-highlight-code-install-deckdeckgo-highlight-code-from-npm">Install DeckDeckGo Highlight Code from NPM</a></li>
<li><a href="#app-components-highlight-code-framework-integration">Framework integration</a></li>
</ul>
</li>
<li><a href="#app-components-highlight-code-usage">Usage</a><ul>
<li><a href="#app-components-highlight-code-properties">Properties</a></li>
<li><a href="#app-components-highlight-code-styling">Styling</a></li>
<li><a href="#app-components-highlight-code-methods">Methods</a><ul>
<li><a href="#app-components-highlight-code-find-the-next-anchor">Find the next anchor</a></li>
<li><a href="#app-components-highlight-code-zoom-into-code">Zoom into code</a></li>
<li><a href="#app-components-highlight-code-load-or-reload-the-component">Load or reload the component</a></li>
</ul>
</li>
<li><a href="#app-components-highlight-code-examples">Examples</a></li>
</ul>
</li>
</ul>
<h2 id="app-components-highlight-code-showcase">Showcase</h2>
<deckgo-highlight-code language="java">
      <code slot="code">public static void main(String args[]) &#123; {'\n'}  System.out.println(&quot;Hello World&quot;);{'\n'}&#125;</code>
    </deckgo-highlight-code><h2 id="app-components-highlight-code-getting-started">Getting started</h2>
<p>To create easily your PWA presentation and to enjoy all the options, I suggest you to create your slides using the CLI as described in the <a href="/docs/introduction">Getting started chapter</a>.</p>
<p>Doing so you will use the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit which already includes this Web Component.</p>
<p>However, if you are looking to use this Web Component as a standalone component, to add a code highlighter feature to your web applications, it could be use directly in your project from a CDN, using a simple script include, or could be installed from <a href="https://www.npmjs.com/package/deckdeckgo-highlight-code">npm</a>.</p>
<h3 id="app-components-highlight-code-using-deckdeckgo-highlight-code-from-a-cdn">Using DeckDeckGo Highlight Code from a CDN</h3>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> Code from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script src=&quot;https:&#47;&#47;unpkg.com&#47;deckdeckgo-highlight-code@latest&#47;dist&#47;deckdeckgo-highlight-code.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-highlight-code-install-deckdeckgo-highlight-code-from-npm">Install DeckDeckGo Highlight Code from NPM</h3>
<p>Install <a href="https://deckdeckgo.com">DeckDeckGo</a> - Highlight Code in your project from <a href="https://www.npmjs.com/package/deckdeckgo-highlight-code">npm</a> using the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install deckdeckgo-highlight-code</code>
    </deckgo-highlight-code><h3 id="app-components-highlight-code-framework-integration">Framework integration</h3>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
<h2 id="app-components-highlight-code-usage">Usage</h2>
<p>The <code>&lt;deckgo-highlight-code/&gt;</code> Web Component will highlight your code using <a href="https://prismjs.com">Prism.js</a>. You could inject a <code>&lt;code/&gt;</code> tag using slot or provide an URI to the file containing your code.</p>
<h3 id="app-components-highlight-code-properties">Properties</h3>
<p>The <code>&lt;deckgo-highlight-code/&gt;</code> expose the following properties:</p>
<table>
<thead>
<tr>
<th>Property</th>
<th>Attribute</th>
<th>Description</th>
<th>Type</th>
<th>Default</th>
</tr>
</thead>
<tbody><tr>
<td><code>src</code></td>
<td><code>src</code></td>
<td>The web url to the source code you would like to showcase</td>
<td><code>string</code></td>
<td></td>
</tr>
<tr>
<td><code>anchor</code></td>
<td><code>anchor</code></td>
<td>The anchor identifier which will be use to find the next anchor to scroll too using <code>findNextAnchor()</code></td>
<td><code>string</code></td>
<td><code>&#39;// DeckDeckGo&#39;</code></td>
</tr>
<tr>
<td><code>anchorZoom</code></td>
<td><code>anchor-zoom</code></td>
<td>The anchor identifier which will be use to find the next anchor to zoom inside your code using <code>findNextAnchor()</code></td>
<td><code>string</code></td>
<td><code>&#39;// DeckDeckGoZoom&#39;</code></td>
</tr>
<tr>
<td><code>hideAnchor</code></td>
<td><code>hide-anchor</code></td>
<td>Set this attribute to <code>false</code> in case you would like to actually display the anchor value too</td>
<td><code>boolean</code></td>
<td><code>true</code></td>
</tr>
<tr>
<td><code>language</code></td>
<td><code>language</code></td>
<td>Define the language to be used for the syntax highlighting. The list of <a href="https://prismjs.com/#languages-list">supported languages</a> is defined by <a href="https://prismjs.com/#languages-list">Prism.js</a></td>
<td><code>string</code></td>
<td><code>&#39;javascript&#39;</code></td>
</tr>
<tr>
<td><code>highlightLines</code></td>
<td><code>highlight-lines</code></td>
<td>If you wish to highlight some lines of your code. The lines number should be provided as number separated with coma and group separated with space. For example: &quot;3,5 8,9 13,13 14,17&quot;</td>
<td><code>string</code></td>
<td></td>
</tr>
</tbody></table>
<h3 id="app-components-highlight-code-styling">Styling</h3>
<p>The <code>&lt;deckgo-highlight-code/&gt;</code> could be styled using the following CSS4 variables:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-highlight-code-color</td>
<td>inherit</td>
<td>The color of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-background</td>
<td></td>
<td>The background of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-padding</td>
<td></td>
<td>The padding of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-border-radius</td>
<td></td>
<td>The border radius of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-margin-bottom</td>
<td>64px</td>
<td>Margin bottom of the code scroller</td>
</tr>
<tr>
<td>--deckgo-highlight-code-zoom</td>
<td>1</td>
<td>If you wish to manually zoom the code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-font-size</td>
<td></td>
<td>The size of the font for the code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-font-family</td>
<td>monospace</td>
<td>The family of the font for the code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-line-background</td>
<td></td>
<td>The background of the lines you wish to highlight</td>
</tr>
<tr>
<td>--deckgo-highlight-code-line-padding</td>
<td></td>
<td>A padding for each lines you wish to highlight</td>
</tr>
<tr>
<td>--deckgo-highlight-code-line-border-top</td>
<td></td>
<td>The border-top property of the lines you wish to highlight</td>
</tr>
<tr>
<td>--deckgo-highlight-code-direction</td>
<td>ltr</td>
<td>The direction of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-text-align</td>
<td>start</td>
<td>The text alignment of your code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-comment</td>
<td></td>
<td>Highlighted code tokens comment, prolog, doctype and cdata</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-punctuation</td>
<td></td>
<td>Highlighted code token punctuation</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-property</td>
<td></td>
<td>Highlighted code tokens property, tag, boolean, number, constant, symbol, deleted</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-selector</td>
<td></td>
<td>Highlighted code tokens selector, attr-name, string, char, builtin, inserted</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-operator</td>
<td></td>
<td>Highlighted code tokens operator, entity, url, string</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-atrule</td>
<td></td>
<td>Highlighted code tokens atrule, attr-value, keyword</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-function</td>
<td></td>
<td>Highlighted code function, class-name</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-regex</td>
<td></td>
<td>Highlighted code tokens regex, important, variable</td>
</tr>
</tbody></table>
<h3 id="app-components-highlight-code-methods">Methods</h3>
<p>The <code>&lt;deckgo-highlight-code/&gt;</code> expose the following methods:</p>
<h4 id="app-components-highlight-code-find-the-next-anchor">Find the next anchor</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">findNextAnchor(enter: boolean) =&gt; Promise&lt;boolean&gt;</code>
    </deckgo-highlight-code><h4 id="app-components-highlight-code-zoom-into-code">Zoom into code</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">zoomCode(zoom: boolean) =&gt; Promise&lt;void&gt;</code>
    </deckgo-highlight-code><h4 id="app-components-highlight-code-load-or-reload-the-component">Load or reload the component</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">load() =&gt; Promise&lt;void&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-highlight-code-examples">Examples</h3>
<p>You could find the examples in the <a href="https://github.com/deckgo/deckdeckgo-qrcode/blob/master/src/index.html">src/index.html</a> of the project.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-highlight-code src=&quot;https:&#47;&#47;domain.com&#47;yourfile.js&quot;&gt;{'\n'}&lt;&#47;deckgo-highlight-code&gt;{'\n'}{'\n'}&lt;deckgo-highlight-code language=&quot;java&quot; highlight-lines=&quot;2,2&quot;&gt;{'\n'}  &lt;code slot=&quot;code&quot;&gt;public static void main(String args[]) &#123;{'\n'}{'\n'}  System.out.println(&quot;Hello World&quot;);{'\n'}&#125;&lt;&#47;code&gt;{'\n'}&lt;&#47;deckgo-highlight-code&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
