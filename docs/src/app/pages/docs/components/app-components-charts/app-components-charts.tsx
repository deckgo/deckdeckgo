import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-components-charts'
})
export class AppComponentsCharts {

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

  private async prev(cmpId: string) {
    const elem = this.el.querySelector('#' + cmpId);
    if (elem) {
      await (elem as any).prev();
    }
  }

  private async next(cmpId: string) {
    const elem = this.el.querySelector('#' + cmpId);
    if (elem) {
      await (elem as any).next();
    }
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-components-charts-charts">Charts</h1>
<p>The &quot;Charts&quot; component is an extra component which let you draw charts easily.</p>
<p>To create and draw the charts, this project is using <a href="https://d3js.org">D3js</a>.</p>
<h2 id="app-components-charts-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-components-chart-showcase">Showcase</a><ul>
<li><a href="#app-components-chart-pie">Pie</a></li>
<li><a href="#app-components-chart-donut">Donut</a></li>
<li><a href="#app-components-chart-animated-pie">Animated pie</a></li>
<li><a href="#app-components-chart-compare-two-graphs">Compare two graphs</a></li>
<li><a href="#app-components-chart-line-and-area">Line and area</a></li>
<li><a href="#app-components-chart-compare-two-lines-and-smoothing-effect">Compare two lines and smoothing effect</a></li>
<li><a href="#app-components-chart-compare-two-lines-and-display-a-grid">Compare two lines and display a grid</a></li>
<li><a href="#app-components-chart-animated-lines">Animated lines</a></li>
<li><a href="#app-components-chart-bar">Bar</a></li>
<li><a href="#app-components-chart-compare-multiple-bars">Compare multiple bars</a></li>
<li><a href="#app-components-chart-animated-bars">Animated bars</a></li>
</ul>
</li>
<li><a href="#app-components-chart-installation">Installation</a><ul>
<li><a href="#app-components-chart-using-deckdeckgo-charts-from-a-cdn">Using DeckDeckGo charts from a CDN</a></li>
<li><a href="#app-components-chart-install-deckdeckgo-charts-from-npm">Install DeckDeckGo charts from NPM</a></li>
<li><a href="#app-components-chart-framework-integration">Framework integration</a></li>
</ul>
</li>
<li><a href="#app-components-chart-usage">Usage</a><ul>
<li><a href="#app-components-chart-pie-usage">Pie usage</a><ul>
<li><a href="#app-components-chart-csv">CSV</a><ul>
<li><a href="#app-components-chart-example">Example</a></li>
</ul>
</li>
<li><a href="#app-components-chart-properties">Properties</a></li>
<li><a href="#app-components-chart-styling">Styling</a></li>
<li><a href="#app-components-chart-methods">Methods</a></li>
<li><a href="#app-components-chart-examples">Examples</a></li>
</ul>
</li>
<li><a href="#app-components-chart-line-usage">Line usage</a><ul>
<li><a href="#app-components-chart-csv-1">CSV</a><ul>
<li><a href="#app-components-chart-two-columns-1">Two columns</a></li>
<li><a href="#app-components-chart-three-columns-or-more">Three columns or more</a></li>
</ul>
</li>
<li><a href="#app-components-chart-properties-1">Properties</a></li>
<li><a href="#app-components-chart-styling-1">Styling</a></li>
<li><a href="#app-components-chart-methods-1">Methods</a></li>
<li><a href="#app-components-chart-examples-1">Examples</a></li>
</ul>
</li>
<li><a href="#app-components-chart-bar-usage">Bar usage</a><ul>
<li><a href="#app-components-chart-csv-2">CSV</a><ul>
<li><a href="#app-components-chart-multiple-columns">Multiple columns</a></li>
</ul>
</li>
<li><a href="#app-components-chart-properties-2">Properties</a></li>
<li><a href="#app-components-chart-styling-2">Styling</a></li>
<li><a href="#app-components-chart-methods-2">Methods</a></li>
<li><a href="#app-components-chart-examples-2">Examples</a></li>
</ul>
</li>
</ul>
</li>
</ul>
<h2 id="app-components-charts-showcase">Showcase</h2>
<p>The following examples are the one provided in the <a href="https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html">src/index.html</a> of this component. If you would like to run them locally, proceed as follow:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">git clone https:&#47;&#47;github.com&#47;deckgo&#47;deckdeckgo{'\n'}cd webcomponents&#47;charts{'\n'}npm install{'\n'}npm run start</code>
    </deckgo-highlight-code><h3 id="app-components-charts-pie">Pie</h3>
<p><deckgo-pie-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv"></deckgo-pie-chart></p>
<h3 id="app-components-charts-donut">Donut</h3>
<p><deckgo-pie-chart width={500} height={400} inner-radius={100} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv"></deckgo-pie-chart></p>
<h3 id="app-components-charts-animated-pie">Animated pie</h3>
<div>
  <deckgo-pie-chart id="animatedPie" animation={true} width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"></deckgo-pie-chart>

  <div>
    <ion-button fill="outline" size="small" onClick={() => this.prev('animatedPie')}><ion-label>Prev</ion-label></ion-button>
    <ion-button fill="outline" size="small" onClick={() => this.next('animatedPie')}><ion-label>Next</ion-label></ion-button>
  </div>
</div>

<h3 id="app-components-charts-compare-two-graphs">Compare two graphs</h3>
<p><deckgo-line-chart width={500} height={400} y-axis-domain="extent" date-pattern="dd.MM.yyyy" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv"></deckgo-line-chart></p>
<h3 id="app-components-charts-line-and-area">Line and area</h3>
<p><deckgo-line-chart width={500} height={400} smooth={false} ticks={5} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart.csv" date-pattern="dd.MM.yyyy"></deckgo-line-chart></p>
<h3 id="app-components-charts-compare-two-lines-and-smoothing-effect">Compare two lines and smoothing effect</h3>
<div>
  <deckgo-line-chart width={500} height={400} area={false} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-no-dates.csv"
                    style={{'--deckgo-chart-fill-color-0': 'none', '--deckgo-chart-stroke-0': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-1': 'none', '--deckgo-chart-fill-opacity-1': '1', '--deckgo-chart-stroke-1': 'var(--ion-color-secondary)', '--deckgo-chart-stroke-width-1': '3px'}}></deckgo-line-chart>
</div>

<h3 id="app-components-charts-compare-two-lines-and-display-a-grid">Compare two lines and display a grid</h3>
<div>
  <deckgo-line-chart width={500} height={400} smooth={false} area={false} grid={true} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-no-dates.csv"
                    style={{'--deckgo-chart-fill-color-0': 'none', '--deckgo-chart-stroke-0': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-1': 'none', '--deckgo-chart-fill-opacity-1': '1', '--deckgo-chart-stroke-1': 'var(--ion-color-secondary)', '--deckgo-chart-stroke-width-1': '3px'}}></deckgo-line-chart>
</div>

<h3 id="app-components-charts-animated-lines">Animated lines</h3>
<div>
  <deckgo-line-chart id="animatedLine" animation={true} y-axis-domain="extent" date-pattern="dd.MM.yyyy" width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-multiple.csv"></deckgo-line-chart>

  <div>
    <ion-button fill="outline" size="small" onClick={() => this.prev('animatedLine')}><ion-label>Prev</ion-label></ion-button>
    <ion-button fill="outline" size="small" onClick={() => this.next('animatedLine')}><ion-label>Next</ion-label></ion-button>
  </div>
</div>

<h3 id="app-components-charts-bar">Bar</h3>
<div>
  <deckgo-bar-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                    style={{'--deckgo-chart-fill-color-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'}}></deckgo-bar-chart>
</div>

<h3 id="app-components-charts-compare-multiple-bars">Compare multiple bars</h3>
<div>
  <deckgo-bar-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv" 
                    style={{'--deckgo-chart-fill-color-1': 'var(--ion-color-primary)'}}></deckgo-bar-chart>
</div>

<h3 id="app-components-charts-animated-bars">Animated bars</h3>
<div>
  <deckgo-bar-chart id="animatedBar" animation={true} width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                      style={{'--deckgo-chart-fill-color-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'}}></deckgo-bar-chart>

  <div>
    <ion-button fill="outline" size="small" onClick={() => this.prev('animatedBar')}><ion-label>Prev</ion-label></ion-button>
    <ion-button fill="outline" size="small" onClick={() => this.next('animatedBar')}><ion-label>Next</ion-label></ion-button>
  </div>
</div>

<h2 id="app-components-charts-installation">Installation</h2>
<p>This component could be added to your web application using the following methods.</p>
<blockquote>
<p>If you are using our Starter Kit to develop your presentation, no need to worry about this, this component is included, therefore you could skip the &quot;Installation&quot; chapter.</p>
</blockquote>
<h3 id="app-components-charts-using-deckdeckgo-charts-from-a-cdn">Using DeckDeckGo charts from a CDN</h3>
<p>It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> charts from a CDN. To do so, add the following include script in the main HTML file of your project:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;script type=&quot;module&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;charts@latest&#47;dist&#47;deckdeckgo-charts&#47;deckdeckgo-charts.esm.js&quot;&gt;&lt;&#47;script&gt;{'\n'}&lt;script nomodule=&quot;&quot; src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;charts@latest&#47;dist&#47;deckdeckgo-charts&#47;deckdeckgo-charts.js&quot;&gt;&lt;&#47;script&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-charts-install-deckdeckgo-charts-from-npm">Install DeckDeckGo charts from NPM</h3>
<p>Install <a href="https://deckdeckgo.com">DeckDeckGo</a> charts in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/charts">npm</a> using the following command:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm install @deckdeckgo&#47;charts</code>
    </deckgo-highlight-code><h3 id="app-components-charts-framework-integration">Framework integration</h3>
<p>The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>, <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.</p>
<p>That being said, commonly, you might either <code>import</code> or <code>load</code> it:</p>
<h4 id="app-components-charts-import">Import</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#039;@deckdeckgo&#47;charts&#039;;</code>
    </deckgo-highlight-code><h4 id="app-components-charts-loader">Loader</h4>
<deckgo-highlight-code language="javascript">
      <code slot="code">import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;charts&#47;dist&#47;loader&#039;;{'\n'}deckDeckGoElement(window);</code>
    </deckgo-highlight-code><h2 id="app-components-charts-usage">Usage</h2>
<p>The main idea behind the <a href="https://deckdeckgo.com">DeckDeckGo</a> charts is to be able to drop a <strong>csv</strong> file, provide it to the component and let it do the job respectively draw the chart.</p>
<h3 id="app-components-charts-pie-usage">Pie usage</h3>
<p>The <code>&lt;deckgo-pie-chart/&gt;</code> Web Component draw a Pie chart.</p>
<h4 id="app-components-charts-csv">CSV</h4>
<p>The csv file should contains at least two columns. The first one should be a label, which could be displayed or not, and the second one should be a value.</p>
<p>If more than two columns are provided, all columns beside the first one will be interpreted as values for comparison, in case you would like to displayed multiple graphs.</p>
<p>The values could be provided as <code>number</code> or <code>percent</code>.</p>
<h5 id="app-components-charts-example">Example</h5>
<p>In this example, the first column contains a label for the category and the second contains the value in percent.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">Wind;53.13%{'\n'}Solar:1.96%{'\n'}Geothermal;7.52%{'\n'}Landfill Gas;15.67%{'\n'}Biomass;14.79%{'\n'}Qualified Hydropower;6.93%</code>
    </deckgo-highlight-code><h4 id="app-components-charts-properties">Properties</h4>
<p>The <code>&lt;deckgo-pie-chart/&gt;</code> expose the following properties:</p>
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
<td><code>src</code></td>
<td><code>src</code></td>
<td>X</td>
<td>The path to the source file of the data</td>
<td><code>string</code></td>
<td></td>
</tr>
<tr>
<td><code>width</code></td>
<td><code>width</code></td>
<td>X</td>
<td>The width of the chart</td>
<td><code>number</code></td>
<td></td>
</tr>
<tr>
<td><code>height</code></td>
<td><code>height</code></td>
<td>X</td>
<td>The height of the chart</td>
<td><code>number</code></td>
<td></td>
</tr>
<tr>
<td><code>innerRadius</code></td>
<td><code>inner-radius</code></td>
<td></td>
<td>To plot a <code>donut</code> instead of a <code>pie</code>, provide an inner radius</td>
<td><code>number</code></td>
<td><code>0</code></td>
</tr>
<tr>
<td><code>range</code></td>
<td><code>range</code></td>
<td></td>
<td>A list of custom colors which should be to draw the chart</td>
<td><code>string[]</code></td>
<td></td>
</tr>
<tr>
<td><code>separator</code></td>
<td><code>separator</code></td>
<td></td>
<td>The line separator use in your csv file</td>
<td><code>string</code></td>
<td><code>&#39;;&#39;</code></td>
</tr>
<tr>
<td><code>animation</code></td>
<td><code>animation</code></td>
<td></td>
<td>Display multiple graphs and animate the transition between these</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>animationDuration</code></td>
<td><code>animation-duration</code></td>
<td></td>
<td>Duration of the transition between graphs</td>
<td><code>numer</code></td>
<td><code>1000</code> (aka 1 second)</td>
</tr>
</tbody></table>
<h4 id="app-components-charts-styling">Styling</h4>
<p>The <code>&lt;deckgo-pie-chart/&gt;</code> could be styled using the following CSS4 variables:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-chart-text-color</td>
<td>black</td>
<td>The color of the labels</td>
</tr>
<tr>
<td>--deckgo-chart-text-display</td>
<td></td>
<td>The display property of the labels</td>
</tr>
<tr>
<td>--deckgo-chart-path-stroke</td>
<td></td>
<td>The stroke property of the pie, in case you would like to display a line between the slices</td>
</tr>
</tbody></table>
<h4 id="app-components-charts-methods">Methods</h4>
<p>The <code>&lt;deckgo-pie-chart/&gt;</code> expose the following methods.</p>
<h5 id="app-components-charts-draw">Draw</h5>
<p>In case you would like to redraw your chart, for example on resize of the window:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">draw(width?: number, height?: number) =&gt; Promise&lt;void&gt;</code>
    </deckgo-highlight-code><h5 id="app-components-charts-next">Next</h5>
<p>If you are using <code>animation</code>, this method is used to display the next data respectively the next chart.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">async next()</code>
    </deckgo-highlight-code><h5 id="app-components-charts-previous">Previous</h5>
<p>If you are using <code>animation</code>, this method is used to display the previous data respectively the previous chart.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">async prev()</code>
    </deckgo-highlight-code><h4 id="app-components-charts-examples">Examples</h4>
<p>You could find other examples of pie charts in the <a href="https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html">src/index.html</a> of the project.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-pie-chart width=&#123;500&#125; height=&#123;400&#125; src=&quot;https:&#47;&#47;raw.githubusercontent.com&#47;deckgo&#47;deckdeckgo&#47;master&#47;webcomponents&#47;charts&#47;showcase&#47;data-pie-chart.csv&quot;&gt;&lt;&#47;deckgo-pie-chart&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-charts-line-usage">Line usage</h3>
<p>The <code>&lt;deckgo-line-chart/&gt;</code> Web Component draw a line chart.</p>
<h4 id="app-components-charts-csv-1">CSV</h4>
<p>The csv file should contains two or multiple columns.</p>
<p>The first column contains the values for the <code>x</code> axis. These should be provided as <code>date</code> or <code>number</code>.</p>
<p>The second and other columns contains the values for the <code>y</code> axis. These should be provided as <code>number</code>.</p>
<p>Use multiple columns in case you would like to compare multiple graphs.</p>
<h5 id="app-components-charts-two-columns">Two columns</h5>
<p>With dates as <code>x</code> axis:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">01.01.2018;5{'\n'}01.03.2018;10{'\n'}01.06.2018;8</code>
    </deckgo-highlight-code><p>With numbers as <code>x</code> axis:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">1;5{'\n'}2;10{'\n'}3;8</code>
    </deckgo-highlight-code><h5 id="app-components-charts-three-columns-or-more">Three columns or more</h5>
<p>The third columns or any others is optional, it could be use in case you would like to plot multiple charts on the same graph or animate a transition between these. </p>
<p>With dates as <code>x</code> axis:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">01.01.2018;5;4{'\n'}01.03.2018;10;3{'\n'}01.06.2018;8;19</code>
    </deckgo-highlight-code><p>With numbers as <code>x</code> axis:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">1;5;7{'\n'}2;10;13{'\n'}3;8;5</code>
    </deckgo-highlight-code><h4 id="app-components-charts-properties-1">Properties</h4>
<p>The <code>&lt;deckgo-line-chart/&gt;</code> expose the following properties:</p>
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
<td><code>src</code></td>
<td><code>src</code></td>
<td>X</td>
<td>The path to the source file of the data</td>
<td><code>string</code></td>
<td></td>
</tr>
<tr>
<td><code>width</code></td>
<td><code>width</code></td>
<td>X</td>
<td>The width of the chart</td>
<td><code>number</code></td>
<td></td>
</tr>
<tr>
<td><code>height</code></td>
<td><code>height</code></td>
<td>X</td>
<td>The height of the chart</td>
<td><code>number</code></td>
<td></td>
</tr>
<tr>
<td><code>marginBottom</code></td>
<td><code>margin-bottom</code></td>
<td></td>
<td>The margin bottom of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>marginLeft</code></td>
<td><code>margin-left</code></td>
<td></td>
<td>The margin left of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>marginRight</code></td>
<td><code>margin-right</code></td>
<td></td>
<td>The margin right of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>marginTop</code></td>
<td><code>margin-top</code></td>
<td></td>
<td>The margin top of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>datePattern</code></td>
<td><code>date-pattern</code></td>
<td></td>
<td>In case the <code>x</code> are made of dates, the pattern to be use to parse the dates. Have a look to <a href="https://date-fns.org/v2.0.0-alpha.26/docs/parse">date-fns</a> for a list of the supported format.</td>
<td><code>string</code></td>
<td><code>&#39;yyyy-MM-dd&#39;</code></td>
</tr>
<tr>
<td><code>yAxisDomain</code></td>
<td><code>y-axis-domain</code></td>
<td></td>
<td>Should the <code>y</code> axis plot the values from <code>0</code> to <code>max</code> or be extended (use <code>extent</code>) to cover all values</td>
<td><code>string</code></td>
<td><code>max</code></td>
</tr>
<tr>
<td><code>smooth</code></td>
<td><code>smooth</code></td>
<td></td>
<td>Draw a smooth line or a line with edges</td>
<td><code>boolean</code></td>
<td><code>true</code></td>
</tr>
<tr>
<td><code>area</code></td>
<td><code>area</code></td>
<td></td>
<td>Draw the area or just a line</td>
<td><code>boolean</code></td>
<td><code>true</code></td>
</tr>
<tr>
<td><code>ticks</code></td>
<td><code>ticks</code></td>
<td></td>
<td>Specify the ticks of the axis</td>
<td><code>number</code></td>
<td>``</td>
</tr>
<tr>
<td><code>grid</code></td>
<td><code>grid</code></td>
<td></td>
<td>Draw a grid behin the graph</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>separator</code></td>
<td><code>separator</code></td>
<td></td>
<td>The line separator use in your csv file</td>
<td><code>string</code></td>
<td><code>&#39;;&#39;</code></td>
</tr>
<tr>
<td><code>animation</code></td>
<td><code>animation</code></td>
<td></td>
<td>Display multiple graphs and animate the transition between these</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>animationDuration</code></td>
<td><code>animation-duration</code></td>
<td></td>
<td>Duration of the transition between graphs</td>
<td><code>numer</code></td>
<td><code>1000</code> (aka 1 second)</td>
</tr>
</tbody></table>
<h4 id="app-components-charts-styling-1">Styling</h4>
<p>The <code>&lt;deckgo-line-chart/&gt;</code> could be styled using the following CSS4 variables:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-chart-axis-color</td>
<td>black</td>
<td>The color of the axis</td>
</tr>
<tr>
<td>--deckgo-chart-text-color</td>
<td>black</td>
<td>The color of the labels</td>
</tr>
<tr>
<td>--deckgo-chart-text-display</td>
<td></td>
<td>The display property of the labels</td>
</tr>
<tr>
<td>--deckgo-chart-grid-stroke</td>
<td>#989aa2</td>
<td>The stroke of the grid</td>
</tr>
<tr>
<td>--deckgo-chart-grid-stroke-opacity</td>
<td>0.7</td>
<td>The stroke opacity of the grid</td>
</tr>
</tbody></table>
<p>Furthermore, for each generated charts, the following CSS4 variables could be applied:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-chart-fill-color-index</td>
<td>#3880ff</td>
<td>The fill color of the area of the chart</td>
</tr>
<tr>
<td>--deckgo-chart-fill-opacity-index</td>
<td></td>
<td>The opacity of the area of the chart</td>
</tr>
<tr>
<td>--deckgo-chart-stroke-index</td>
<td></td>
<td>The stroke of the area of the chart</td>
</tr>
<tr>
<td>--deckgo-chart-stroke-width-index</td>
<td></td>
<td>The stroke width of the area of the chart</td>
</tr>
</tbody></table>
<p>Note: Replace <code>-index</code> with the index of the chart, for example: <code>--deckgo-chart-fill-color-1: red;</code></p>
<h4 id="app-components-charts-methods-1">Methods</h4>
<p>The <code>&lt;deckgo-line-chart/&gt;</code> expose the following methods.</p>
<h5 id="app-components-charts-draw-1">Draw</h5>
<p>In case you would like to redraw your chart, for example on resize of the window:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">draw(width?: number, height?: number) =&gt; Promise&lt;void&gt;</code>
    </deckgo-highlight-code><h5 id="app-components-charts-next-1">Next</h5>
<p>If you are using <code>animation</code>, this method is used to display the next data respectively the next chart.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">async next()</code>
    </deckgo-highlight-code><h5 id="app-components-charts-previous-1">Previous</h5>
<p>If you are using <code>animation</code>, this method is used to display the previous data respectively the previous chart.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">async prev()</code>
    </deckgo-highlight-code><h4 id="app-components-charts-examples-1">Examples</h4>
<p>You could find other examples of line charts in the <a href="https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html">src/index.html</a> of the project.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-line-chart width=&#123;500&#125; height=&#123;400&#125; date-pattern=&quot;dd.MM.yyyy&quot; src=&quot;https:&#47;&#47;raw.githubusercontent.com&#47;deckgo&#47;deckdeckgo&#47;master&#47;webcomponents&#47;charts&#47;showcase&#47;data-line-chart.csv&quot;&gt;&lt;&#47;deckgo-line-chart&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-charts-bar-usage">Bar usage</h3>
<p>The <code>&lt;deckgo-bar-chart/&gt;</code> Web Component draw a Bar chart.</p>
<h4 id="app-components-charts-csv-2">CSV</h4>
<p>The csv file should contains at least two columns. The first column should contains the labels. The other columns should contains values.</p>
<p>Use multiple columns in case you would like to compare multiple graphs.</p>
<p>The values could be provided as <code>number</code> or <code>percent</code>.</p>
<h5 id="app-components-charts-multiple-columns">Multiple columns</h5>
<p>The first column should contains the labels or groups used for the X axis. The other columns are the values use for the Y axis.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">Group A;5;0;10{'\n'}Group B;10;6;12{'\n'}Group C;8;14;8{'\n'}Group D;14;8;16{'\n'}Group E;18;4;5</code>
    </deckgo-highlight-code><h4 id="app-components-charts-properties-2">Properties</h4>
<p>The <code>&lt;deckgo-bar-chart/&gt;</code> expose the following properties:</p>
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
<td><code>src</code></td>
<td><code>src</code></td>
<td>X</td>
<td>The path to the source file of the data</td>
<td><code>string</code></td>
<td></td>
</tr>
<tr>
<td><code>width</code></td>
<td><code>width</code></td>
<td>X</td>
<td>The width of the chart</td>
<td><code>number</code></td>
<td></td>
</tr>
<tr>
<td><code>height</code></td>
<td><code>height</code></td>
<td>X</td>
<td>The height of the chart</td>
<td><code>number</code></td>
<td></td>
</tr>
<tr>
<td><code>marginBottom</code></td>
<td><code>margin-bottom</code></td>
<td></td>
<td>The margin bottom of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>marginLeft</code></td>
<td><code>margin-left</code></td>
<td></td>
<td>The margin left of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>marginRight</code></td>
<td><code>margin-right</code></td>
<td></td>
<td>The margin right of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>marginTop</code></td>
<td><code>margin-top</code></td>
<td></td>
<td>The margin top of the chart in pixel</td>
<td><code>number</code></td>
<td><code>32</code></td>
</tr>
<tr>
<td><code>separator</code></td>
<td><code>separator</code></td>
<td></td>
<td>The line separator use in your csv file</td>
<td><code>string</code></td>
<td><code>&#39;;&#39;</code></td>
</tr>
<tr>
<td><code>animation</code></td>
<td><code>animation</code></td>
<td></td>
<td>Display multiple graphs and animate the transition between these</td>
<td><code>boolean</code></td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>animationDuration</code></td>
<td><code>animation-duration</code></td>
<td></td>
<td>Duration of the transition between graphs</td>
<td><code>numer</code></td>
<td><code>1000</code> (aka 1 second)</td>
</tr>
</tbody></table>
<h4 id="app-components-charts-styling-2">Styling</h4>
<p>The <code>&lt;deckgo-bar-chart/&gt;</code> could be styled using the following CSS4 variables:</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-chart-axis-color</td>
<td>black</td>
<td>The color of the axis</td>
</tr>
</tbody></table>
<p>Furthermore, as the Bar chart could draw dynamically multiple bars, it will generate dynamically the following CSS4 variable for each series of data where <code>X</code> is an index between <code>1</code> and the number of bars.</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-chart-text-color</td>
<td></td>
<td>The color of the labels</td>
</tr>
<tr>
<td>--deckgo-chart-text-display</td>
<td></td>
<td>The display property of the labels</td>
</tr>
<tr>
<td>--deckgo-chart-fill-color-bar-X</td>
<td></td>
<td>The fill color of the bar chart identified with index <code>X</code></td>
</tr>
<tr>
<td>--deckgo-chart-fill-opacity-bax-X</td>
<td></td>
<td>The opacity of the bar chart identified with index <code>X</code></td>
</tr>
<tr>
<td>--deckgo-chart-stroke-bar-X</td>
<td></td>
<td>The stroke of the bar chart identified with index <code>X</code></td>
</tr>
<tr>
<td>--deckgo-chart-stroke-width-bar-X</td>
<td></td>
<td>The stroke width of the chart identified with index <code>X</code></td>
</tr>
</tbody></table>
<h4 id="app-components-charts-methods-2">Methods</h4>
<p>The <code>&lt;deckgo-bar-chart/&gt;</code> expose the following methods:</p>
<h5 id="app-components-charts-draw-2">Draw</h5>
<p>In case you would like to redraw your chart, for example on resize of the window:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">draw(width?: number, height?: number) =&gt; Promise&lt;void&gt;</code>
    </deckgo-highlight-code><h5 id="app-components-charts-next-2">Next</h5>
<p>If you are using <code>animation</code>, this method is used to display the next data respectively the next chart.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">async next()</code>
    </deckgo-highlight-code><h5 id="app-components-charts-previous-2">Previous</h5>
<p>If you are using <code>animation</code>, this method is used to display the previous data respectively the previous chart.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">async prev()</code>
    </deckgo-highlight-code><h4 id="app-components-charts-examples-2">Examples</h4>
<p>You could find other examples of bar charts in the <a href="https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html">src/index.html</a> of the project.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-bar-chart width=&#123;500&#125; {'\n'}                  height=&#123;400&#125;{'\n'}                   src=&quot;https:&#47;&#47;raw.githubusercontent.com&#47;deckgo&#47;deckdeckgo&#47;master&#47;webcomponents&#47;charts&#47;showcase&#47;data-pie-chart.csv&quot;{'\n'}                   style=&quot;--deckgo-chart-fill-color-bar1: #3880ff;&quot;&gt;{'\n'}&lt;&#47;deckgo-bar-chart&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
