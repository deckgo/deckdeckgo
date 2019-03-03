import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-components-youtube'
})
export class AppComponentsYoutube {

  @Element() el: HTMLElement;

  constructor(private menuService: MenuService) {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.loadVideo(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content padding>
        <main><h1 id="app-components-youtube-youtube">Youtube</h1>
<p>The &quot;Youtube&quot; component allows you to easily add a <a href="https://youtube.com">Youtube</a> video in almost any slide of your presentation.</p>
<h2 id="app-components-youtube-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-components-youtube-showcase">Showcase</a></li>
<li><a href="#app-components-youtube-usage">Usage</a><ul>
<li><a href="#app-components-youtube-slots">Slots</a></li>
<li><a href="#app-components-youtube-attributes">Attributes</a></li>
</ul>
</li>
</ul>
<h2 id="app-components-youtube-showcase">Showcase</h2>
<div>
  <deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw" width={300} height={200}>
  </deckgo-youtube>
</div>

<h2 id="app-components-youtube-usage">Usage</h2>
<p>The &quot;Youtube&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-youtube/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-youtube src=&quot;https:&#47;&#47;www.youtube.com&#47;watch?v=oUOjJIfPIjw&quot;&gt;{'\n'}&lt;&#47;deckgo-youtube&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-youtube-slots">Slots</h3>
<p>No slots are available for this component.</p>
<h3 id="app-components-youtube-attributes">Attributes</h3>
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
<td>The source url, the Youtube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by Youtube.</td>
</tr>
<tr>
<td>width</td>
<td>number</td>
<td></td>
<td>The width of the video player.</td>
</tr>
<tr>
<td>height</td>
<td>number</td>
<td></td>
<td>The height of the video player.</td>
</tr>
<tr>
<td>frame-title</td>
<td>string</td>
<td></td>
<td>A title for the frame, could be use for accessibility reason.</td>
</tr>
</tbody></table>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
