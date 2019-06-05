import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slide-chart',
  styleUrl: 'app-slides-chart.scss'
})
export class AppSlideChart {

  @Element() el: HTMLElement;

  private menuService: MenuService;

  constructor() {
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

      <ion-content class="ion-padding">
        <main><h1 id="app-slide-chart-slide-chart">Slide: Chart</h1>
<p>The &quot;Chart&quot; slide let you draw easily charts in your presentation.</p>
<h2 id="app-slide-chart-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-chart-layout">Layout</a></li>
<li><a href="#app-slide-chart-usage">Usage</a><ul>
<li><a href="#app-slide-chart-slots">Slots</a></li>
<li><a href="#app-slide-chart-notes">Notes</a></li>
</ul>
</li>
<li><a href="#app-slide-chart-chart-components">Chart components</a></li>
<li><a href="#app-slide-chart-installation">Installation</a></li>
<li><a href="#app-slide-chart-attributes">Attributes</a></li>
<li><a href="#app-slide-chart-theming">Theming</a></li>
</ul>
<h2 id="app-slide-chart-layout">Layout</h2>
<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-chart width={200} height={100} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100} type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100}
                        type="bar" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                        style={{'--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)'}}
                        >
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
  </deckgo-deck>
</div>

<h2 id="app-slide-chart-usage">Usage</h2>
<p>The &quot;Chart&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-chart/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-slide-chart src=&quot;.&#47;assets&#47;csv&#47;data-pie-chart.csv&quot;&gt;{'\n'}    &lt;h1 slot=&quot;title&quot;&gt;My Pie chart&lt;&#47;h1&gt;{'\n'}&lt;&#47;deckgo-slide-chart&gt;</code>
    </deckgo-highlight-code><h3 id="app-slide-chart-slots">Slots</h3>
<p>The slot <code>title</code> is optional.</p>
<h3 id="app-slide-chart-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<h2 id="app-slide-chart-chart-components">Chart components</h2>
<p>The slide &quot;Chart&quot; relies on the charts components <code>&lt;deckgo-pie-chart/&gt;</code>, <code>&lt;deckgo-line-chart/&gt;</code> and  <code>&lt;deckgo-bar-chart/&gt;</code> which are described in the components <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.</p>
<h2 id="app-slide-chart-installation">Installation</h2>
<p>The <a href="https://deckdeckgo.com">DeckDeckGo</a> charts components are provided in separate extra library. If you don&#39;t use the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to add the <a href="https://deckdeckgo.com">DeckDeckGo</a> chart to your project, you will need to install and integrate it from a CDN or <a href="https://www.npmjs.com/package/@deckdeckgo/charts">npm</a> as described in its <a href="https://docs.deckdeckgo.com/components/charts">installation guide</a>.</p>
<h2 id="app-slide-chart-attributes">Attributes</h2>
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
<td>type</td>
<td>string</td>
<td>pie</td>
<td>The type of the chart, <code>pie</code>, <code>line</code> or <code>bar</code></td>
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
<p>Furthermore, this slide component offers the same attributes as the <a href="https://deckdeckgo.com">DeckDeckGo</a> charts Web Component, see its <a href="https://docs.deckdeckgo.com/components/charts">documentation</a> for the details.</p>
<h2 id="app-slide-chart-theming">Theming</h2>
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
<tr>
<td>--slide-chart-margin-top</td>
<td>32px</td>
<td>Margin top of the chart inside its container</td>
</tr>
<tr>
<td>--slide-chart-margin-end</td>
<td>96px</td>
<td>Margin right of the chart inside its container</td>
</tr>
<tr>
<td>--slide-chart-margin-bottom</td>
<td>32px</td>
<td>Margin bottom of the chart inside its container</td>
</tr>
<tr>
<td>--slide-chart-margin-start</td>
<td>32px</td>
<td>Margin left of the chart inside its container</td>
</tr>
</tbody></table>
<p>Furthermore, this slide component offers the exact same CSS4 variables as the <a href="https://deckdeckgo.com">DeckDeckGo</a> charts Web Component, see its <a href="https://docs.deckdeckgo.com/components/charts">documentation</a> for the details.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
