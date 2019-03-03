import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-edit-markdown'
})
export class AppEditMarkdown {

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
        <main><h1 id="app-edit-markdown-markdown">Markdown</h1>
<p>If you wish, you could edit your <a href="https://deckdeckgo.com">DeckDeckGo</a> presentation using Markdown. This chapter has for goal to introduce you briefly on how editing your slides with Markdown differs from the HTML method.</p>
<h2 id="app-edit-markdown-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-edit-markdown-introduction">Introduction</a></li>
<li><a href="#app-edit-markdown-edit">Edit</a><ul>
<li><a href="#app-edit-markdown-example">Example</a></li>
<li><a href="#app-edit-markdown-attributes">Attributes</a></li>
<li><a href="#app-edit-markdown-notes">Notes</a></li>
</ul>
</li>
<li><a href="#app-edit-markdown-summary">Summary</a></li>
</ul>
<h2 id="app-edit-markdown-introduction">Introduction</h2>
<p>When you edit your talk with Markdown, the <a href="https://github.com/deckgo/deckdeckgo-webpack-plugins">DeckDeckGo Webpack Markdown Plugin</a> will convert, at bundle time, your code to HTML and will inject the results in the <code>index.html</code> file. Doing so, your <a href="https://deckdeckgo.com">DeckDeckGo</a> presentation will remain SEO friendly even without server side rendering. </p>
<h2 id="app-edit-markdown-edit">Edit</h2>
<p>To begin to edit your talk, instead of editing the <code>index.html</code> you will instead have to edit the <code>index.md</code> file provided by the starter kit.</p>
<p>Furthermore than the standard Markdown tags, you will be able to use extended tags in order to specify which types of slides you would like to use. For that purpose, use the separator <code>---</code> followed by a shortened version of the template&#39;s name, like for example <code>--- title</code> for <code>&lt;/deckgo-slide-title&gt;</code>.</p>
<p>The plugin also takes care of injecting the content you would provide in the right slots.</p>
<h3 id="app-edit-markdown-example">Example</h3>
<p>The following <code>&lt;deckgo-slide-title/&gt;</code> slide:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">--- title{'\n'}# My presentation title{'\n'}{'\n'}Hello World 🚀</code>
    </deckgo-highlight-code><p>will be parsed into:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-slide-title&gt;{'\n'}  &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}  &lt;div slot=&quot;content&quot;&gt;{'\n'}    &lt;p&gt;Hello World 🚀&lt;&#47;p&gt;{'\n'}  &lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-slide-title&gt;</code>
    </deckgo-highlight-code><h3 id="app-edit-markdown-attributes">Attributes</h3>
<p>As some templates needs attributes, you will also be able to specify them in Markdown.</p>
<p>For example the <code>&lt;deckgo-slide-gif/&gt;</code> slide:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">--- gif src=&quot;https:&#47;&#47;media.giphy.com&#47;media&#47;xUA7baWfTjfHGLZc3e&#47;giphy.gif&quot; alt=&quot;My gif&quot; fullscreen=&quot;true&quot;{'\n'}# My title{'\n'}{'\n'}# Hey{'\n'}{'\n'}### It&#039;s cool gif</code>
    </deckgo-highlight-code><p>will be parsed into:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-slide-gif src=&quot;https:&#47;&#47;media.giphy.com&#47;media&#47;xUA7baWfTjfHGLZc3e&#47;giphy.gif&quot; alt=&quot;My gif&quot; fullscreen=&quot;true&quot;&gt;{'\n'}  &lt;h1 slot=&quot;title&quot;&gt;My title&lt;&#47;h1&gt;{'\n'}  &lt;h1 slot=&quot;header&quot;&gt;Hey&lt;&#47;h1&gt;{'\n'}  &lt;h2 slot=&quot;footer&quot;&gt;It&#039;s a cool gif&lt;&#47;h2&gt;{'\n'}&lt;&#47;deckgo-slide-gif&gt;</code>
    </deckgo-highlight-code><h3 id="app-edit-markdown-notes">Notes</h3>
<p>To add some notes to a particular slide, use the separator <code>***</code> and write down your notes afterwards.</p>
<p>Optionally, if you wish to display your notes in your deck, you could also use the attribute <code>show</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">--- title{'\n'}# My presentation title{'\n'}{'\n'}Hello World 🚀{'\n'}{'\n'}***{'\n'}{'\n'}I should not forget to think about that during my talk{'\n'}{'\n'}...and say hello to mum</code>
    </deckgo-highlight-code><p>will be parsed into:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-slide-title&gt;{'\n'}  &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}  &lt;div slot=&quot;content&quot;&gt;{'\n'}    &lt;p&gt;Hello World 🚀&lt;&#47;p&gt;{'\n'}  &lt;&#47;div&gt;{'\n'}  &lt;div slot=&quot;notes&quot;&gt;{'\n'}    I should not forget to think about that during my talk{'\n'}{'\n'}    ...and say hello to mum{'\n'}  &lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-slide-title&gt;</code>
    </deckgo-highlight-code><h2 id="app-edit-markdown-summary">Summary</h2>
<p>When you choose Markdown, you edit your slides in <code>index.md</code> and use standard Markdown except the extra tags <code>---</code> to declare your slides and <code>***</code> if you wish to add notes. </p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
