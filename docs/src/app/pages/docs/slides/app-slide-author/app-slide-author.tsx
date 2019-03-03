import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slide-author',
  styleUrl: 'app-slides-author.scss'
})
export class AppSlideAuthor {

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
        <main><h1 id="app-slide-author-slide-author">Slide: Author</h1>
<p>The &quot;Author&quot; slide let you introduce the author of the presentation.</p>
<h2 id="app-slide-author-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-author-layout">Layout</a></li>
<li><a href="#app-slide-author-usage">Usage</a><ul>
<li><a href="#app-slide-author-slots">Slots</a></li>
<li><a href="#app-slide-author-notes">Notes</a></li>
<li><a href="#app-slide-author-social-components">Social components</a></li>
</ul>
</li>
<li><a href="#app-slide-author-attributes">Attributes</a><ul>
<li><a href="#app-slide-author-example">Example</a></li>
</ul>
</li>
<li><a href="#app-slide-author-theming">Theming</a></li>
</ul>
<h2 id="app-slide-author-layout">Layout</h2>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-author img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
        <h1 slot="title">slot="title"</h1>
        <div slot="author">slot="author"</div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social twitter="daviddalbusco"><ion-icon area-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon> twitter</deckgo-social></div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social linkedin="david-dal-busco/"><ion-icon area-label="David on Linkedin" slot="icon" name="logo-linkedin"></ion-icon> linkedin</deckgo-social></div>
    </deckgo-slide-author>
  </deckgo-deck>
</div>

<h2 id="app-slide-author-usage">Usage</h2>
<p>The &quot;Author&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-author/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-author img-src=&quot;&#47;assets&#47;author.jpeg&quot; img-alt=&quot;My self&quot;&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;Author&lt;&#47;h1&gt;{'\n'}    &lt;div slot=&quot;author&quot;&gt;{'\n'}      &lt;h2&gt;David&lt;&#47;h2&gt;{'\n'}      &lt;p&gt;Something about me&lt;&#47;p&gt;{'\n'}    &lt;&#47;div&gt;{'\n'}    &lt;div slot=&quot;social-link&quot;&gt;&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;twitter&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'}  &lt;&#47;deckgo-slide-author&gt;{'\n'}&lt;&#47;deckgo-deck&gt;  </code>
    </deckgo-highlight-code><h3 id="app-slide-author-slots">Slots</h3>
<p>Both slots <code>title</code>, <code>author</code> and <code>social-link</code> are optional, but of course the slide would looks better with at least the slot <code>author</code> would be provided.</p>
<p>Notes: </p>
<ul>
<li><p>The slot <code>title</code> is hidden. If you use the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter, it will be use for the navigation modal</p>
</li>
<li><p>You could provide up to six <code>social-link</code> slots. Each of these could be your custom code or you could use the component <code>&lt;deckgo-social/&gt;</code> to easily provide a link to an external URI.</p>
</li>
</ul>
<h3 id="app-slide-author-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<h3 id="app-slide-author-social-components">Social components</h3>
<p>The details of the component <code>&lt;deckgo-social/&gt;</code> is described in the components <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.</p>
<h2 id="app-slide-author-attributes">Attributes</h2>
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
<td>img-src</td>
<td>string</td>
<td></td>
<td>An image URI, for example a picture of the author. Note: this image will be displayed as a circle.</td>
</tr>
<tr>
<td>img-alt</td>
<td>string</td>
<td></td>
<td>An optional accessibility alt for the image.</td>
</tr>
</tbody></table>
<h3 id="app-slide-author-example">Example</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-author img-src=&quot;&#47;assets&#47;author.jpeg&quot;&gt;{'\n'}    &lt;div slot=&quot;author&quot;&gt;{'\n'}      &lt;h2&gt;David&lt;&#47;h2&gt;{'\n'}      &lt;p&gt;Something about me&lt;&#47;p&gt;{'\n'}    &lt;&#47;div&gt;{'\n'}    &lt;div slot=&quot;social-link&quot;&gt;&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;twitter&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'}    &lt;div slot=&quot;social-link&quot;&gt;&lt;deckgo-social linkedin=&quot;david-dal-busco&#47;&quot;&gt;linkedin&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'}    &lt;div slot=&quot;social-link&quot;&gt;&lt;deckgo-social medium=&quot;david.dalbusco&quot;&gt;medium&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'}  &lt;&#47;deckgo-slide-author&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><h2 id="app-slide-author-theming">Theming</h2>
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
<td>--slide-author-background-start</td>
<td></td>
<td>Left pane background</td>
</tr>
<tr>
<td>--slide-author-color-start</td>
<td></td>
<td>Left pane color</td>
</tr>
<tr>
<td>--slide-author-background-end</td>
<td></td>
<td>Right pane background</td>
</tr>
<tr>
<td>--slide-author-color-end</td>
<td></td>
<td>Right pane color</td>
</tr>
<tr>
<td>--slide-author-padding-top</td>
<td>16px</td>
<td>Padding top of a slide</td>
</tr>
<tr>
<td>--slide-author-padding-end</td>
<td>32px</td>
<td>Padding right of a slide</td>
</tr>
<tr>
<td>--slide-author-padding-bottom</td>
<td>16px</td>
<td>Padding bottom of a slide</td>
</tr>
<tr>
<td>--slide-author-padding-start</td>
<td>32px</td>
<td>Padding left of a slide</td>
</tr>
<tr>
<td>--slide-padding-start</td>
<td>32px</td>
<td>Modify slotted ul and ol padding-inline-start</td>
</tr>
<tr>
<td>--slide-author-align</td>
<td>inherit</td>
<td>Modify for example to center if you want to align the content in the middle</td>
</tr>
<tr>
<td>--slide-author-text-align</td>
<td>inherit</td>
<td>Modify for example to center if you want to align the text in the middle</td>
</tr>
<tr>
<td>--slide-author-img-size</td>
<td>80%</td>
<td>The size of the image of the left pane</td>
</tr>
<tr>
<td>--slide-author-social-padding-top</td>
<td>32px</td>
<td>The spacing between the author description and the social links</td>
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
