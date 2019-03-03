import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slide-gif',
  styleUrl: 'app-slides-gif.scss'
})
export class AppSlideGif {

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
        <main><h1 id="app-slide-gif-slide-gif">Slide: Gif</h1>
<p>The &quot;Gif&quot; slide let you add easily a gif, like those provided by <a href="https://giphy.com">Giphy</a>, to your presentation.</p>
<h2 id="app-slide-gif-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-gif-layout">Layout</a></li>
<li><a href="#app-slide-gif-for-images-too">For images too</a></li>
<li><a href="#app-slide-gif-usage">Usage</a><ul>
<li><a href="#app-slide-gif-slots">Slots</a></li>
<li><a href="#app-slide-gif-notes">Notes</a></li>
</ul>
</li>
<li><a href="#app-slide-gif-gif-component">Gif component</a></li>
<li><a href="#app-slide-gif-attributes">Attributes</a></li>
<li><a href="#app-slide-gif-theming">Theming</a></li>
</ul>
<h2 id="app-slide-gif-layout">Layout</h2>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
      <h1 slot="title">My title</h1>
      <h1 slot="header">Hey</h1>
      <h2 slot="footer">It's a cool gif</h2>
    </deckgo-slide-gif>
  </deckgo-deck>
</div>

<h2 id="app-slide-gif-for-images-too">For images too</h2>
<p>The slide Gif is useful for Gifs but could be use for any images too, in case you would like for example to display an image fullscreen.</p>
<h2 id="app-slide-gif-usage">Usage</h2>
<p>The &quot;Gif&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-gif/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-slide-gif src=&quot;https:&#47;&#47;media.giphy.com&#47;media&#47;xUA7baWfTjfHGLZc3e&#47;giphy.gif&quot; alt=&quot;My gif&quot; fullscreen=&quot;true&quot;&gt;{'\n'}  &lt;h1 slot=&quot;title&quot;&gt;My title&lt;&#47;h1&gt;{'\n'}  &lt;h1 slot=&quot;header&quot;&gt;Hey&lt;&#47;h1&gt;{'\n'}  &lt;h2 slot=&quot;footer&quot;&gt;It&#039;s a cool gif&lt;&#47;h2&gt;{'\n'}&lt;&#47;deckgo-slide-gif&gt;</code>
    </deckgo-highlight-code><h3 id="app-slide-gif-slots">Slots</h3>
<p>The slots <code>title</code>, <code>header</code> and <code>footer</code> are both optional. <code>header</code> and <code>footer</code> would be displayed over the gif.</p>
<h3 id="app-slide-gif-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<h2 id="app-slide-gif-gif-component">Gif component</h2>
<p>The slide &quot;Gif&quot; relies on the component <code>&lt;deckgo-gif/&gt;</code> which is described in the components <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.</p>
<h2 id="app-slide-gif-attributes">Attributes</h2>
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
<td>src</td>
<td>string</td>
<td></td>
<td>The source url, the src, of the Gif. Could be an embeddable external url or a local one.</td>
</tr>
<tr>
<td>alt</td>
<td>string</td>
<td></td>
<td>And alt information could be provided for accessibility reason.</td>
</tr>
<tr>
<td>fullscreen</td>
<td>number</td>
<td>false</td>
<td>If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen.</td>
</tr>
</tbody></table>
<h2 id="app-slide-gif-theming">Theming</h2>
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
<td>--zIndex</td>
<td>1</td>
<td>The z-index of the slide</td>
</tr>
</tbody></table>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
