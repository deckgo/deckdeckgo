import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slides-concept',
  styleUrl: 'app-slides-concept.scss'
})
export class AppSlidesConcept {

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
        <main><h1 id="app-slides-concept-concept">Concept</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> is a deck of slides where each slide is based on a template which has its own layout and behaviour. Their content could be edited and structured using <code>slots</code> and other attributes.</p>
<p>The parent deck should be declared using the tag <code>&lt;deckgo-deck/&gt;</code> and each slide should be added as its children.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-title&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;The first slide&lt;&#47;h1&gt;{'\n'}    &lt;p slot=&quot;content&quot;&gt;{'\n'}      Hello World 🚀{'\n'}    &lt;&#47;p&gt;{'\n'}  &lt;&#47;deckgo-slide-title&gt;{'\n'}{'\n'}  &lt;deckgo-slide-content&gt;{'\n'}      &lt;h1 slot=&quot;title&quot;&gt;The second slide&lt;&#47;h1&gt;{'\n'}  &lt;&#47;deckgo-slide-content&gt;{'\n'}&lt;&#47;deckgo-deck&gt;</code>
    </deckgo-highlight-code><p>In the previous example, the presentation contains two slides. The first slide use the template <code>deckgo-slide-title</code> and the second slide use the template <code>deckgo-slide-content</code>.</p>
<h1 id="app-slides-concept-templates">Templates</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> provide the following templates:</p>
<ul>
<li>Slide: <a href="/slides/title">Title</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-title>
        <h1 slot="title">slot="title"</h1>
        <p slot="content">
          slot="content"
        </p>
      </deckgo-slide-title>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/content">Content</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-content>
      <h1 slot="title">slot="title"</h1>
      <p slot="content">
        slot="content"
      </p>
    </deckgo-slide-content>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/split">Split</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-split>
      <h1 slot="title">slot="title"</h1>
      <p slot="start">
        slot="start"
      </p>
      <p slot="end">
        slot="end"
      </p>
    </deckgo-slide-split>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/gif">Gif</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
      <h1 slot="title">slot="title"</h1>
      <h1 slot="header" style={{fontSize: 'var(--font-size-h1)'}}>slot="header"</h1>
      <h2 slot="footer" style={{fontSize: 'var(--font-size-normal)'}}>slot="footer"</h2>
    </deckgo-slide-gif>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/chart">Chart</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true}>
    <deckgo-slide-chart width={200} height={100} src="https://raw.githubusercontent.com/fluster/deckdeckgo-charts/master/showcase/data-pie-chart.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100} type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                        src="https://raw.githubusercontent.com/fluster/deckdeckgo-charts/master/showcase/data-line-chart-to-compare.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100}
                        type="bar" src="https://raw.githubusercontent.com/fluster/deckdeckgo-charts/master/showcase/data-bar-chart-to-compare.csv"
                        style={{'--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)'}}
                        >
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/youtube">Youtube</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-youtube>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/code">Code</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-code src="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-code>
  </deckgo-deck>
</div>

<ul>
<li>Slide: <a href="/slides/author">Author</a></li>
</ul>
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

<ul>
<li>Slide: <a href="/slides/qrcode">QR Code</a></li>
</ul>
<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-qrcode content="https://deckdeckgo.com">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>

<h2 id="app-slides-concept-note">Note</h2>
<p>If you would miss or need further templates, don&#39;t hesitate to open an issue and/or submit a PR, it would be my pleasure to add more options.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
