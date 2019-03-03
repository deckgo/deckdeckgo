import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slide-title',
  styleUrl: 'app-slides-title.scss'
})
export class AppSlideTitle {

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
        <main><h1 id="app-slide-title-slide-title">Slide: Title</h1>
<p>The &quot;Title&quot; slide is a simple slide which display its title and content center in the middle of the page.</p>
<p>This slide could be for example use for the very first and last slide of your presentation.</p>
<h2 id="app-slide-title-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-title-layout">Layout</a></li>
<li><a href="#app-slide-title-usage">Usage</a><ul>
<li><a href="#app-slide-title-slots">Slots</a></li>
<li><a href="#app-slide-title-notes">Notes</a></li>
</ul>
</li>
<li><a href="#app-slide-title-attributes">Attributes</a></li>
<li><a href="#app-slide-title-example">Example</a></li>
<li><a href="#app-slide-title-theming">Theming</a></li>
</ul>
<h2 id="app-slide-title-layout">Layout</h2>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-title>
      <h1 slot="title">My presentation title</h1>
      <p slot="content">
        Hello World 🚀
      </p>
    </deckgo-slide-title>
  </deckgo-deck>
</div>

<h2 id="app-slide-title-usage">Usage</h2>
<p>The &quot;Title&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-title/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;p slot=&quot;content&quot;&gt;{'\n'}      Hello World 🚀{'\n'}    &lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><h3 id="app-slide-title-slots">Slots</h3>
<p>Both slots <code>title</code> and <code>content</code> are optional. Without providing one of them, the page will remain empty.</p>
<h3 id="app-slide-title-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;div slot=&quot;notes&quot;&gt;A note regarding this particular slide&lt;&#47;div&gt;{'\n'}{'\n'}And another note on a new line about it too.{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;div slot=&quot;notes&quot; show&gt;A note displayed in the presentation within a modal accessible for anyone&lt;&#47;div&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><h2 id="app-slide-title-attributes">Attributes</h2>
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
<td>reveal</td>
<td>boolean</td>
<td>false</td>
<td>Hide the slotted elements <code>li</code>, <code>p</code> an <code>img</code> and display them when navigating using <code>slideNext()</code> or <code>slidePrev()</code> (see <a href="/doc/features/navigation.md">documention</a>)</td>
</tr>
<tr>
<td>reveal-show-first</td>
<td>boolean</td>
<td>false</td>
<td>Show the first elements which would be hidden if <code>reveal</code> is set to <code>true</code></td>
</tr>
</tbody></table>
<h2 id="app-slide-title-example">Example</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title reveal=&quot;true&quot; reveal-show-first=&quot;true&quot;&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;ul slot=&quot;content&quot;&gt;{'\n'}      &lt;li&gt;Hello&lt;&#47;li&gt;{'\n'}      &lt;li&gt;World&lt;&#47;li&gt;{'\n'}      &lt;li&gt;🚀&lt;&#47;li&gt;{'\n'}    &lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><h2 id="app-slide-title-theming">Theming</h2>
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
