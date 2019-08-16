import {Component, Element, h} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-edit-reveal'
})
export class AppEditReveal {

  @Element() el: HTMLElement;

  private menuService: MenuService;

  constructor() {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-edit-reveal-reveal">Reveal</h1>
<p>Make elements and texts appear one line at a time in <a href="https://deckdeckgo.com">DeckDeckGo</a>.</p>
<h2 id="app-edit-reveal-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-edit-reveal-introduction">Introduction</a></li>
<li><a href="#app-edit-reveal-triggers">Triggers</a></li>
<li><a href="#app-edit-reveal-exception">Exception</a></li>
<li><a href="#app-edit-reveal-edit">Edit</a><ul>
<li><a href="#app-edit-reveal-examples">Examples</a></li>
</ul>
</li>
<li><a href="#app-edit-reveal-list">List</a><ul>
<li><a href="#app-edit-reveal-examples-for-list">Examples for list</a></li>
</ul>
</li>
</ul>
<h2 id="app-edit-reveal-introduction">Introduction</h2>
<p>Per default all the content of each slides and components is visible. If you wish to make elements and texts appear one line at a time, it&#39;s up to you using the following supported options.</p>
<h2 id="app-edit-reveal-triggers">Triggers</h2>
<p>The animation of such elements will happen when you or your users will use the keyboard, the navigation buttons or the navigation buttons in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<h2 id="app-edit-reveal-exception">Exception</h2>
<p>Elements set as &quot;to be animated&quot; are going to be displayed in any case on mobile devices, that&#39;s a design choice.</p>
<blockquote>
<p>I (David here) think that it is better in terms of mobile UX. For example, if a slides would contains for example 10 elements, the users would have to swipe the slide 10 times before being able to read the all content and navigate. I&#39;m open to suggestion and discussion about it, ping me, open a feature request or even submit a PR if you see this differently.   </p>
</blockquote>
<h2 id="app-edit-reveal-edit">Edit</h2>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> provide a component <code>&lt;deckgo-reveal/&gt;</code> which should be use in case you would like to make elements appear one at a time. Simply put your element with your content inside, that&#39;s it.</p>
<p>Good to know, the component could be use as a child of a <code>slot</code> you would pass to a slide or could also be use as <code>slot</code> value, as you wish.</p>
<p>Notabene, at least an element should be provided, adding only text inside the component would not work as the detection is based on elements. </p>
<h3 id="app-edit-reveal-examples">Examples</h3>
<p>The component <code>deckgo-reveal</code> use as <code>slot</code>:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;deckgo-reveal slot=&quot;content&quot;&gt;{'\n'}      &lt;p&gt;Hello World 🚀&lt;&#47;p&gt;{'\n'}    &lt;&#47;deckgo-reveal&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><p>Many components <code>deckgo-reveal</code> as children:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;p slot=&quot;content&quot;&gt;{'\n'}      &lt;deckgo-reveal&gt;&lt;span&gt;Hello World One 🚀&lt;&#47;span&gt;&lt;&#47;deckgo-reveal&gt;{'\n'}      &lt;deckgo-reveal&gt;&lt;span&gt;Hello World Two 🚀&lt;&#47;span&gt;&lt;&#47;deckgo-reveal&gt;{'\n'}      &lt;deckgo-reveal&gt;&lt;span&gt;Hello World Three 🚀&lt;&#47;span&gt;&lt;&#47;deckgo-reveal&gt;{'\n'}    &lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><p>Or a component <code>deckgo-reveal</code> as child containing children:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;div slot=&quot;content&quot;&gt;{'\n'}      &lt;deckgo-reveal&gt;{'\n'}         &lt;p&gt;Hello World One 🚀&lt;&#47;p&gt;{'\n'}         &lt;p&gt;Hello World Two 🚀&lt;&#47;p&gt;{'\n'}         &lt;p&gt;Hello World Three 🚀&lt;&#47;p&gt;{'\n'}      &lt;&#47;deckgo-reveal&gt;{'\n'}    &lt;&#47;div&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><h2 id="app-edit-reveal-list">List</h2>
<p>You could use the above component to encapsulate each <code>li</code> elements of your lists, I guess that would work out, but <a href="https://deckdeckgo.com">DeckDeckGo</a> also provide a dedicated component <code>&lt;deckgo-reveal-list/&gt;</code> to reveal list.</p>
<p>To use it, simply replace the opening tag of your list (<code>ul</code>, <code>ol</code> or <code>dl</code>) with it.</p>
<h3 id="app-edit-reveal-attributes">Attributes</h3>
<p>The following attribute could be applied to the element:</p>
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
<td><code>listTag</code></td>
<td><code>list-tag</code></td>
<td></td>
<td>The type of list (<code>ol</code> default, <code>ul</code> or <code>dl</code>)</td>
<td><code>string</code></td>
<td><code>ol</code></td>
</tr>
</tbody></table>
<h2 id="app-edit-reveal-theming">Theming</h2>
<p>The following theming options are also available:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--reveal-list-style</td>
<td></td>
<td>The list-style property of the list</td>
</tr>
<tr>
<td>--reveal-list-style-image</td>
<td></td>
<td>The list-style property of the list</td>
</tr>
<tr>
<td>--reveal-list-style-position</td>
<td></td>
<td>The list-style-position property of the list</td>
</tr>
<tr>
<td>--reveal-list-margin</td>
<td></td>
<td>The list-margin property of the list</td>
</tr>
<tr>
<td>--reveal-list-padding</td>
<td></td>
<td>The list-padding property of the list</td>
</tr>
<tr>
<td>--reveal-list-background</td>
<td></td>
<td>The list-background property of the list</td>
</tr>
<tr>
<td>--reveal-list-style-type</td>
<td><code>disc</code></td>
<td>The list-style-type property in case of <code>ul</code> container</td>
</tr>
<tr>
<td>--reveal-list-style-type</td>
<td><code>decimal</code></td>
<td>The list-style-type property in case of <code>ol</code> container</td>
</tr>
<tr>
<td>--reveal-list-style-type</td>
<td><code>none</code></td>
<td>The list-style-type property in case of <code>dl</code> container</td>
</tr>
</tbody></table>
<h2 id="app-edit-reveal-examples-for-list">Examples for list</h2>
<p>Likewise, the component could be use as a child of a <code>slot</code> you would pass to a slide or could also be use as <code>slot</code> value, as you wish.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}    &lt;div slot=&quot;content&quot;&gt;{'\n'}      &lt;deckgo-reveal-list list-tag=&quot;ul&quot;&gt;{'\n'}         &lt;li&gt;Hello World One 🚀&lt;&#47;li&gt;{'\n'}         &lt;li&gt;Hello World Two 🚀&lt;&#47;li&gt;{'\n'}         &lt;li&gt;Hello World Three 🚀&lt;&#47;li&gt;{'\n'}      &lt;&#47;deckgo-reveal-list&gt;{'\n'}    &lt;&#47;div&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
