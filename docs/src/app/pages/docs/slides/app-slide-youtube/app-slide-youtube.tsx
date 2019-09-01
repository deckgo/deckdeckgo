import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-youtube',
  styleUrl: 'app-slides-youtube.scss'
})
export class AppSlideYoutube {

  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  @Listen('slidesDidLoad')
  async onSlidesDidLoad($event: CustomEvent) {
    if ($event) {
      await DeckdeckgoDocsUtils.initSlideSize($event.target as HTMLElement);
    }
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-slide-youtube-slide-youtube">Slide: Youtube</h1>
<p>The &quot;Youtube&quot; slide let you add easily a <a href="https://youtube.com">Youtube</a> video to your presentation.</p>
<h2 id="app-slide-youtube-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-youtube-layout">Layout</a></li>
<li><a href="#app-slide-youtube-installation">Installation</a><ul>
<li><a href="#app-slide-youtube-from-a-cdn">From a CDN</a></li>
<li><a href="#app-slide-youtube-from-npm">From NPM</a></li>
<li><a href="#app-slide-youtube-framework-integration">Framework integration</a></li>
</ul>
</li>
<li><a href="#app-slide-youtube-usage">Usage</a><ul>
<li><a href="#app-slide-youtube-slots">Slots</a></li>
<li><a href="#app-slide-youtube-notes">Notes</a></li>
<li><a href="#app-slide-youtube-youtube-component">Youtube component</a></li>
</ul>
</li>
<li><a href="#app-slide-youtube-attributes">Attributes</a></li>
<li><a href="#app-slide-youtube-theming">Theming</a></li>
<li><a href="#app-slide-youtube-methods">Methods</a><ul>
<li><a href="#app-slide-youtube-play-the-video">Play the video</a></li>
<li><a href="#app-slide-youtube-pause-the-video">Pause the video</a></li>
</ul>
</li>
</ul>
<h2 id="app-slide-youtube-layout">Layout</h2>
<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
      <h1 slot="title">A 16/9 video</h1>
    </deckgo-slide-youtube>
  </deckgo-deck>
</div>

<h2 id="app-slide-youtube-installation">Installation</h2>
<p>This template could be added to your presentation using the following methods.</p>
<blockquote>
<p>If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot; chapter.</p>
</blockquote>
<h3 id="app-slide-youtube-from-a-cdn">From a CDN</h3>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script type=&quot;module&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-youtube@latest&#47;dist&#47;deckdeckgo-slide-youtube&#47;deckdeckgo-slide-youtube.esm.js&quot;&gt;&lt;&#47;script&gt;{'\n'}&lt;script nomodule=&quot;&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-youtube@latest&#47;dist&#47;deckdeckgo-slide-youtube&#47;deckdeckgo-slide-youtube.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h3 id="app-slide-youtube-from-npm">From NPM</h3>
<p>To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/core">npm</a> run the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install @deckdeckgo&#47;slide-youtube</code>
    </deckgo-highlight-code><h3 id="app-slide-youtube-framework-integration">Framework integration</h3>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
<p>That being said, commonly, you might either <code>import</code> or <code>load</code> it:</p>
<h4 id="app-slide-youtube-import">Import</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#039;@deckdeckgo&#47;slide-youtube&#039;;</code>
    </deckgo-highlight-code><h4 id="app-slide-youtube-loader">Loader</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-youtube&#47;dist&#47;loader&#039;;{'\n'}deckDeckGoSlideElement(window);</code>
    </deckgo-highlight-code><h2 id="app-slide-youtube-usage">Usage</h2>
<p>The &quot;Youtube&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-youtube/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-slide-youtube src=&quot;https:&#47;&#47;www.youtube.com&#47;watch?v=oUOjJIfPIjw&quot;&gt;{'\n'}  &lt;h1 slot=&quot;title&quot;&gt;A 16&#47;9 video&lt;&#47;h1&gt;{'\n'}&lt;&#47;deckgo-slide-youtube&gt;</code>
    </deckgo-highlight-code><h3 id="app-slide-youtube-slots">Slots</h3>
<p>The slot <code>title</code> and <code>content</code> are optional. The slot <code>content</code> is displayed before the video.</p>
<h3 id="app-slide-youtube-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<h3 id="app-slide-youtube-youtube-component">Youtube component</h3>
<p>The slide &quot;Youtube&quot; relies on the component <code>&lt;deckgo-youtube/&gt;</code> which is described in the components <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.</p>
<h2 id="app-slide-youtube-attributes">Attributes</h2>
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
<td>Per default the video width will be calculated according the content size available.</td>
<td>Using this option you would be able to define your own width.</td>
</tr>
<tr>
<td>height</td>
<td>number</td>
<td>Per default the video height will be calculated according the content size available.</td>
<td>Using this option you would be able to define your own height.</td>
</tr>
<tr>
<td>custom-background</td>
<td>boolean</td>
<td>false</td>
<td>If you would provide a background for the all deck and a specific one for this slide, set this option to <code>true</code></td>
</tr>
<tr>
<td>custom-actions</td>
<td>boolean</td>
<td>false</td>
<td>If you would provide actions for the all deck and a specific one for this slide, set this option to <code>true</code></td>
</tr>
</tbody></table>
<h2 id="app-slide-youtube-theming">Theming</h2>
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
<h2 id="app-slide-youtube-methods">Methods</h2>
<p>The slide &quot;Youtube&quot; offers extra methods to play and pause the Youtube video clip. These methods are notably used by the [DeckDecGo]&#39;s remote control.</p>
<h3 id="app-slide-youtube-play-the-video">Play the video</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-youtube&#039;);{'\n'}await slide.play();</code>
    </deckgo-highlight-code><h3 id="app-slide-youtube-pause-the-video">Pause the video</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-youtube&#039;);{'\n'}await slide.pause();</code>
    </deckgo-highlight-code><h3 id="app-slide-youtube-toggle-the-video">Toggle the video</h3>
<p>Toggle will take care to pause or play the video according its current state.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-youtube&#039;);{'\n'}await slide.toggle();</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
