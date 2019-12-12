import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-highlight-code'
})
export class AppComponentsHighlightCode {

  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-components-highlight-code-highlight-code">Highlight Code</h1>
<p>The &quot;Highlight Code&quot; component is an extra component which let you highlight code easily.</p>
<p>To highlight your code, this component is using <a href="https://prismjs.com">Prism.js</a> from <a href="http://lea.verou.me">Lea Verou</a> and <a href="https://twitter.com/jamesdigioia">James DiGioia</a>.</p>
<p>Moreover, per default, your code will be displayed in form of a stylish &quot;windowed&quot; card as the amazing <a href="https://carbon.now.sh">carbon</a>, the tool to create and share beautiful images of your source code, would do.</p>
<h2 id="app-components-highlight-code-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-components-highlight-code-showcase">Showcase</a></li>
<li><a href="#app-components-highlight-code-installation">Installation</a><ul>
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
<li><a href="#app-components-highlight-code-events">Events</a></li>
<li><a href="#app-components-highlight-code-examples">Examples</a></li>
</ul>
</li>
</ul>
<h2 id="app-components-highlight-code-showcase">Showcase</h2>
<deckgo-highlight-code language="java">
      <code slot="code">public static void main(String args[]) &#123; {'\n'}  System.out.println(&quot;Hello World&quot;);{'\n'}&#125;</code>
    </deckgo-highlight-code><h2 id="app-components-highlight-code-installation">Installation</h2>
<p>This component could be added to your web application using the following methods.</p>
<blockquote>
<p>If you are using our Starter Kit this template is included. You don&#39;t need to install it so therefore you should skip the &quot;Installation&quot; chapter.</p>
</blockquote>
<h3 id="app-components-highlight-code-using-deckdeckgo-highlight-code-from-a-cdn">Using DeckDeckGo Highlight Code from a CDN</h3>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> Code from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script type=&quot;module&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;highlight-code@latest&#47;dist&#47;deckdeckgo-highlight-code&#47;deckdeckgo-highlight-code.esm.js&quot;&gt;&lt;&#47;script&gt;{'\n'}&lt;script nomodule=&quot;&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;highlight-code@latest&#47;dist&#47;deckdeckgo-highlight-code&#47;deckdeckgo-highlight-code.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-highlight-code-install-deckdeckgo-highlight-code-from-npm">Install DeckDeckGo Highlight Code from NPM</h3>
<p>Install <a href="https://deckdeckgo.com">DeckDeckGo</a> - Highlight Code in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/highlight-code">npm</a> using the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install @deckdeckgo&#47;highlight-code</code>
    </deckgo-highlight-code><h3 id="app-components-highlight-code-framework-integration">Framework integration</h3>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
<p>That being said, commonly, you might either <code>import</code> or <code>load</code> it:</p>
<h4 id="app-components-highlight-code-import">Import</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#039;@deckdeckgo&#47;highlight-code&#039;;</code>
    </deckgo-highlight-code><h4 id="app-components-highlight-code-loader">Loader</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;highlight-code&#47;dist&#47;loader&#039;;{'\n'}deckDeckGoElement(window);</code>
    </deckgo-highlight-code><h2 id="app-components-highlight-code-usage">Usage</h2>
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
<tr>
<td><code>lineNumbers</code></td>
<td><code>line-numbers</code></td>
<td>Display the number of the lines of code</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>editable</code></td>
<td><code>editable</code></td>
<td>In case you would like to set the code component as being editable.</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>carbon</code></td>
<td><code>carbon</code></td>
<td>Present the code in a stylish &quot;windowed&quot; card.</td>
<td><code>boolean</code></td>
<td><code>true</code></td>
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
<td>--deckgo-highlight-code-display</td>
<td>block</td>
<td>The display property of the code</td>
</tr>
<tr>
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
<td>0 16px</td>
<td>The padding of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-border-radius</td>
<td></td>
<td>The border radius of the displayed code</td>
</tr>
<tr>
<td>--deckgo-highlight-code-margin-bottom</td>
<td>0 16px</td>
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
<td>#3E4564</td>
<td>The background of the lines you wish to highlight</td>
</tr>
<tr>
<td>--deckgo-highlight-code-line-numbers</td>
<td>#999999</td>
<td>The color of the line numbers and divider</td>
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
<td>#6272a4</td>
<td>Highlighted code tokens comment, prolog, doctype and cdata</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-punctuation</td>
<td>inherit</td>
<td>Highlighted code token punctuation</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-property</td>
<td>#bd93f9</td>
<td>Highlighted code tokens property, tag, boolean, number, constant, symbol, deleted</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-selector</td>
<td>#50fa7b</td>
<td>Highlighted code tokens selector, attr-name, string, char, builtin, inserted</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-operator</td>
<td>#ff79c6</td>
<td>Highlighted code tokens operator, entity, url, string</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-atrule</td>
<td>#ff79c6</td>
<td>Highlighted code tokens atrule, attr-value, keyword</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-function</td>
<td>#ffb86c</td>
<td>Highlighted code function, class-name</td>
</tr>
<tr>
<td>--deckgo-highlight-code-token-regex</td>
<td>#f1fa8c</td>
<td>Highlighted code tokens regex, important, variable</td>
</tr>
<tr>
<td>--deckgo-highlight-code-empty-text</td>
<td>&quot;Click to add your code&quot;</td>
<td>Place holder in case the <code>editable</code> is set to <code>true</code></td>
</tr>
<tr>
<td>--deckgo-highlight-code-scroll</td>
<td>scroll</td>
<td>In case you would like to change the scroll property of the shadowed code block</td>
</tr>
<tr>
<td>--deckgo-highlight-code-container-width</td>
<td></td>
<td>The attribute width of the code&#39;s container</td>
</tr>
<tr>
<td>--deckgo-highlight-code-container-height</td>
<td></td>
<td>The attribute height of the code&#39;s container</td>
</tr>
<tr>
<td>--deckgo-highlight-code-container-display</td>
<td></td>
<td>The attribute display of the code&#39;s container</td>
</tr>
<tr>
<td>--deckgo-highlight-code-container-justify-content</td>
<td></td>
<td>The attribute justify-content of the code&#39;s container</td>
</tr>
<tr>
<td>--deckgo-highlight-code-container-flex-direction</td>
<td></td>
<td>The attribute flex-direction of the code&#39;s container</td>
</tr>
<tr>
<td>--deckgo-highlight-code-container-align-items</td>
<td></td>
<td>The attribute align-items of the code&#39;s container</td>
</tr>
</tbody></table>
<p>Furthermore the following styles apply if the code is displayed as a &quot;windowed&quot; card (<code>carbon</code> property equals to <code>true</code>):</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-highlight-code-carbon-display</td>
<td>block</td>
<td>The display property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-overflow</td>
<td>auto</td>
<td>The overflow property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-border</td>
<td></td>
<td>The border property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-border-radius</td>
<td>4px</td>
<td>The border-radius property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-background</td>
<td>#282a36</td>
<td>The background property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-color</td>
<td>white</td>
<td>The color property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-box-shadow</td>
<td>rgba(0, 0, 0, 0.55) 0 8px 16px</td>
<td>The box-shadow property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-margin</td>
<td>16px 0</td>
<td>The margin property of the host container.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-padding</td>
<td>16px</td>
<td>The padding property of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-width</td>
<td>12px</td>
<td>The width of a button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-heeght</td>
<td>12px</td>
<td>The height of a button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-border-radius</td>
<td>50%</td>
<td>The border-radius of a button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-margin</td>
<td>0 6px 0 0</td>
<td>The margin of a button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-red-background</td>
<td>#FF5F56</td>
<td>The background of the first button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-red-border</td>
<td>0.5px solid #E0443E</td>
<td>The border of the first button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-yellow-background</td>
<td>#FFBD2E</td>
<td>The background of the second button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-yellow-border</td>
<td>0.5px solid #DEA123</td>
<td>The border of the second button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-green-background</td>
<td>#27C93F</td>
<td>The background of the third button of the card header.</td>
</tr>
<tr>
<td>--deckgo-highlight-code-carbon-header-button-green-border</td>
<td>0.5px solid #1AAB29</td>
<td>The color of the third button of the card header.</td>
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
    </deckgo-highlight-code><h3 id="app-components-highlight-code-events">Events</h3>
<p>The <code>&lt;deckgo-highlight-code/&gt;</code> will bubble the following events:</p>
<h4 id="app-components-highlight-code-code-did-change">Code did change</h4>
<p>Emitted when the code was edited (see attribute <code>editable</code>). Propagate the root component itself.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">codeDidChange(HTMLElement);</code>
    </deckgo-highlight-code><h3 id="app-components-highlight-code-examples">Examples</h3>
<p>You could find the examples in the <a href="https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/highlight-code/src/index.html">src/index.html</a> of the project.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-highlight-code src=&quot;https:&#47;&#47;domain.com&#47;yourfile.js&quot;&gt;{'\n'}&lt;&#47;deckgo-highlight-code&gt;{'\n'}{'\n'}&lt;deckgo-highlight-code language=&quot;java&quot; highlight-lines=&quot;2,2&quot;&gt;{'\n'}  &lt;code slot=&quot;code&quot;&gt;public static void main(String args[]) &#123;{'\n'}{'\n'}  System.out.println(&quot;Hello World&quot;);{'\n'}&#125;&lt;&#47;code&gt;{'\n'}&lt;&#47;deckgo-highlight-code&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
