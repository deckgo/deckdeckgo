import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-components-inline-editor'
})
export class AppComponentsInlineEditor {

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
        <main><h1 id="app-components-inline-editor-wysiwyg-inline-editor">WYSIWYG inline editor</h1>
<p>The &quot;WYSIWYG inline editor&quot; component is an extra component which will be use in the upcoming <a href="https://deckdeckgo.com">DeckDeckGo</a> Studio.</p>
<h2 id="app-components-inline-editor-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-components-qrcode-showcase">Showcase</a></li>
<li><a href="#app-components-qrcode-getting-started">Getting started</a><ul>
<li><a href="#app-components-from-a-cdn">Using from a CDN</a></li>
<li><a href="#app-components-from-npm">Install from NPM</a></li>
<li><a href="#app-components-qrcode-framework-integration">Framework integration</a></li>
</ul>
</li>
<li><a href="#app-components-qrcode-usage">Usage</a><ul>
<li><a href="#app-components-qrcode-properties">Properties</a></li>
<li><a href="#app-components-qrcode-styling">Styling</a></li>
<li><a href="#app-components-qrcode-examples">Examples</a></li>
</ul>
</li>
</ul>
<h2 id="app-components-inline-editor-showcase">Showcase</h2>
<div>
  <h1 style={{color: '#3880ff'}} contenteditable>DeckDeckGo (editable title)</h1>

  <h2 style={{color: '#3880ff'}} contenteditable>The Progressive Web App alternative for simple presentations 🚀 (editable subtitle)</h2>

  <p style={{color: '#3880ff'}} contenteditable>Edit anywhere, display everywhere (editable paragraph)</p>
</div>

<p><deckgo-inline-editor sticky-desktop="true" sticky-mobile="true"></deckgo-inline-editor></p>
<h2 id="app-components-inline-editor-getting-started">Getting started</h2>
<p>This Web Component is an inline WYSIWYG HTML Editor, It creates a floating editor bar or a sticky footer bar that shows up when you select a piece of text of your page.</p>
<p>To add the component to your web applications, it could be use directly in your project from a CDN, using a simple script include, or could be installed from <a href="https://www.npmjs.com/package/deckdeckgo-qrcode">npm</a>.</p>
<h3 id="app-components-inline-editor-using-from-a-cdn">Using from a CDN</h3>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> inline editor from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script src=&quot;https:&#47;&#47;unpkg.com&#47;deckdeckgo-inline-editor@latest&#47;dist&#47;deckdeckgo-inline-editor.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-inline-editor-install-from-npm">Install from NPM</h3>
<p>Install it in your project from <a href="https://www.npmjs.com/package/deckdeckgo-qrcode">npm</a> using the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install deckdeckgo-inline-editor</code>
    </deckgo-highlight-code><h3 id="app-components-inline-editor-framework-integration">Framework integration</h3>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
<h2 id="app-components-inline-editor-usage">Usage</h2>
<p>The <code>&lt;deckgo-inline-editor/&gt;</code> should be added once only in your page. It will interact with all elements of types <code>p</code>, <code>h1</code>, <code>h2</code>  and <code>h3</code> you would have set as <code>contenteditable</code>.</p>
<h3 id="app-components-inline-editor-properties">Properties</h3>
<p>The <code>&lt;deckgo-inline-editor/&gt;</code> expose the following properties:</p>
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
<td><code>mobile</code></td>
<td><code>mobile</code></td>
<td>The mobile mode is automatically recognize, but just it case you would like to &quot;force&quot; it</td>
<td><code>boolean</code></td>
<td><code>false</code> or <code>true</code> according the device</td>
</tr>
<tr>
<td><code>stickyDesktop</code></td>
<td><code>sticky-desktop</code></td>
<td>Use a sticky footer toolbar on desktop</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>stickyMobile</code></td>
<td><code>sticky-mobile</code></td>
<td>Use a sticky footer toolbar on mobile. Note: except iOS, feel free to send a PR if you know how to handle this with the software keyboard</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
</tbody></table>
<h3 id="app-components-inline-editor-styling">Styling</h3>
<p>The <code>&lt;deckgo-inline-editor/&gt;</code> could be styled using the following CSS4 variables which would only applies on the type <code>&lt;svg/&gt;</code>:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-inline-editor-background-top</td>
<td>rgba(56, 128, 255, .99)</td>
<td>The top background of the toolbar (linear gradient)</td>
</tr>
<tr>
<td>--deckgo-inline-editor-background-bottom</td>
<td>#3171e0</td>
<td>The bottom background of the toolbar (linear gradient)</td>
</tr>
<tr>
<td>--deckgo-inline-editor-border-radius</td>
<td>8px</td>
<td>The border radius of the toolbar</td>
</tr>
<tr>
<td>--deckgo-inline-editor-padding</td>
<td>0 8px</td>
<td>The padding of the toolbar</td>
</tr>
<tr>
<td>--deckgo-inline-editor-zindex</td>
<td>1</td>
<td>The z-Index of the toolbar</td>
</tr>
<tr>
<td>--deckgo-inline-editor-transform</td>
<td></td>
<td>The transform property of the toolbar, useful for example if your viewport contains a split menu pane</td>
</tr>
<tr>
<td>--deckgo-inline-editor-sticky-bottom</td>
<td>0</td>
<td>The bottom attribute of the sticky toolbar</td>
</tr>
<tr>
<td>--deckgo-inline-editor-separator-background</td>
<td>rgba(255, 255, 255, .2)</td>
<td>The color of the separator</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-color</td>
<td>#fff</td>
<td>The buttons color</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-font-size</td>
<td>1.4rem</td>
<td>The buttons font size</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-font-family</td>
<td>inherit</td>
<td>The buttons font family</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-color-active</td>
<td>#ffce00</td>
<td>The color of the buttons when active</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-color-disabled</td>
<td>#4c8dff</td>
<td>The color of the buttons when disabled</td>
</tr>
<tr>
<td>--deckgo-inline-editor-link-color</td>
<td>#fff</td>
<td>The color of the input field for the url</td>
</tr>
<tr>
<td>--deckgo-inline-editor-link-placeholder-color</td>
<td>#fff</td>
<td>Ther color of the placeholder of the input field for the url</td>
</tr>
</tbody></table>
<p>Furthermore, the following variables are also available but only have an effects on mobile devices:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-inline-editor-mobile-box-shadow</td>
<td>0 0px 1px rgba(0, 0, 0, 0.16), 0 1px 3px rgba(0, 0, 0, 0.15)</td>
<td>A box shadow for the toolbar</td>
</tr>
<tr>
<td>--deckgo-inline-editor-mobile-background-top</td>
<td>#fff</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-mobile-color</td>
<td>inherit</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-mobile-background-bottom</td>
<td>#fff</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-mobile-color-active</td>
<td>#3880ff</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-button-mobile-color-disabled</td>
<td>#f4f5f8</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-separator-mobile-background</td>
<td>#f4f5f8</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-link-mobile-color</td>
<td>inherit</td>
<td>**</td>
</tr>
<tr>
<td>--deckgo-inline-editor-link-mobile-placeholder-color</td>
<td>inherit</td>
<td>**</td>
</tr>
</tbody></table>
<p>** like above but for mobile</p>
<h3 id="app-components-inline-editor-examples">Examples</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;p contenteditable&gt;This text will be editable&lt;&#47;p&gt;{'\n'}{'\n'}&lt;h1 contenteditable&gt;This title too&lt;&#47;h1&gt;{'\n'}{'\n'}&lt;deckgo-inline-editor&gt;&lt;&#47;deckgo-inline-editor&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
