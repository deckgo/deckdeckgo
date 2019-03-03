import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slide-qrcode',
  styleUrl: 'app-slides-qrcode.scss'
})
export class AppSlideQRCode {

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
        <main><h1 id="app-slide-qrcode-slide-qr-code">Slide: QR Code</h1>
<p>The &quot;QR code&quot; slide is an handy slide in case you would like to display a QR code. It could for example be use as the very last slide of your presentation to display an easy link pointing to your deck, you previously published online. It would allow your audience to get easily your slides without any delay on their phone.</p>
<h2 id="app-slide-qrcode-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-qrcode-layout">Layout</a></li>
<li><a href="#app-slide-qrcode-usage">Usage</a><ul>
<li><a href="#app-slide-qrcode-usage-1">Usage</a></li>
<li><a href="#app-slide-qrcode-slots">Slots</a></li>
<li><a href="#app-slide-qrcode-notes">Notes</a></li>
</ul>
</li>
<li><a href="#app-slide-qrcode-code-components">Code components</a></li>
<li><a href="#app-slide-qrcode-installation">Installation</a></li>
<li><a href="#app-slide-qrcode-attributes">Attributes</a><ul>
<li><a href="#app-slide-qrcode-example-without-any-slots">Example without any slots</a></li>
</ul>
</li>
<li><a href="#app-slide-qrcode-theming">Theming</a></li>
</ul>
<h2 id="app-slide-qrcode-layout">Layout</h2>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-qrcode content="https://deckdeckgo.com">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>

<h2 id="app-slide-qrcode-usage">Usage</h2>
<p>The &quot;QR code&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-qrcode/&gt;</code>.</p>
<h3 id="app-slide-qrcode-usage-1">Usage</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-qrcode content=&quot;https:&#47;&#47;deckdeckgo.com&quot;&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My QR code&lt;&#47;h1&gt;{'\n'}    &lt;p slot=&quot;content&quot;&gt;An optional additional content&lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;  </code>
    </deckgo-highlight-code><h3 id="app-slide-qrcode-slots">Slots</h3>
<p>The slots <code>title</code> and <code>content</code> are optional.</p>
<h3 id="app-slide-qrcode-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<h2 id="app-slide-qrcode-code-components">Code components</h2>
<p>The slide &quot;QR Code&quot; relies on the code component <code>&lt;deckgo-qrcode/&gt;</code> which is described in the components <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.</p>
<h2 id="app-slide-qrcode-installation">Installation</h2>
<p>The <a href="https://deckdeckgo.com">DeckDeckGo</a> - QR Code component is provided in separate extra library. If you don&#39;t use the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to add the <a href="https://deckdeckgo.com">DeckDeckGo</a> QR code to your project, you will need to install and integrate it from a CDN or <a href="https://www.npmjs.com/package/deckdeckgo-qrcode">npm</a> as described in its <a href="https://github.com/deckgo/deckdeckgo-qrcode#getting-started">installation guide</a>.</p>
<h2 id="app-slide-qrcode-attributes">Attributes</h2>
<p>The attribute <code>content</code> should be provided in order to render a QR code in this template. It offers the same attributes as the <a href="https://deckdeckgo.com">DeckDeckGo</a> QR code Web Component, see its <a href="https://github.com/deckgo/deckdeckgo-qrcode">documentation</a> for the details.</p>
<h3 id="app-slide-qrcode-example-without-any-slots">Example without any slots</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-qrcode content=&quot;An encoded text&quot;&gt;{'\n'}  &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;  </code>
    </deckgo-highlight-code><h2 id="app-slide-qrcode-theming">Theming</h2>
<p>The following theming options will affect this component if set on its host or parent.</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--background</td>
<td></td>
<td></td>
</tr>
<tr>
<td>--color</td>
<td></td>
<td></td>
</tr>
<tr>
<td>--slide-padding-top</td>
<td>16px</td>
<td>Padding top of the all slide</td>
</tr>
<tr>
<td>--slide-padding-end</td>
<td>32px</td>
<td>Padding right of the all slide</td>
</tr>
<tr>
<td>--slide-padding-bottom</td>
<td>16px</td>
<td>Padding bottom of the all slide</td>
</tr>
<tr>
<td>--slide-padding-start</td>
<td>32px</td>
<td>Padding left of the all slide</td>
</tr>
<tr>
<td>--slide-qrcode-align</td>
<td>center</td>
<td>QR code vertical alignment</td>
</tr>
<tr>
<td>--slide-qrcode-text-align</td>
<td>center</td>
<td>QR code horizontal alignment</td>
</tr>
<tr>
<td>--slide-qrcode-background</td>
<td></td>
<td>QR code column&#39;s background</td>
</tr>
<tr>
<td>--slide-qrcode-title-display</td>
<td>inherit</td>
<td>If you wish to hide the slot=&quot;title&quot;</td>
</tr>
</tbody></table>
<p>Furthermore, this slide component offers the exact same CSS4 variables as the <a href="https://deckdeckgo.com">DeckDeckGo</a> - QR code Web Component, see its <a href="https://github.com/deckgo/deckdeckgo-qrcode">documentation</a> for the details.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
