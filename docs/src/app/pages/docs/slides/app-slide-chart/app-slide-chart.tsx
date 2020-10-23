import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-chart',
  styleUrl: 'app-slide-chart.scss',
})
export class AppSlideChart {
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
        <main>
          <h1 id="app-slide-chart-slide-chart">Slide: Chart</h1>
          <p>The &quot;Chart&quot; slide let you draw easily charts in your presentation.</p>
          <h2 id="app-slide-chart-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-chart-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-chart-video">Video</a>
            </li>
            <li>
              <a href="#app-slide-chart-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-chart-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-chart-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-chart-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-chart-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-chart-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-chart-chart-components">Chart components</a>
            </li>
            <li>
              <a href="#app-slide-chart-installation">Installation</a>
            </li>
            <li>
              <a href="#app-slide-chart-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-chart-theming">Theming</a>
            </li>
            <li>
              <a href="#app-slide-chart-methods">Methods</a>
              <ul>
                <li>
                  <a href="#app-slide-chart-draw">Draw</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-slide-chart-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-chart
                width={200}
                height={100}
                src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-chart>
              <deckgo-slide-chart
                width={200}
                height={100}
                type="line"
                y-axis-domain="extent"
                date-pattern="dd.MM.yyyy"
                src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-chart>
              <deckgo-slide-chart
                width={200}
                height={100}
                type="bar"
                src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                style={{
                  '--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)',
                  '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)',
                  '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)',
                }}>
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-chart>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-chart-video">Video</h2>
          <p>Have a look at this video where we demonstrate how to use it!</p>
          <iframe width="560" height="315" src="https://www.youtube.com/embed/ESD-K7zZT-c" frameborder="0"></iframe>

          <h2 id="app-slide-chart-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-chart-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-chart@latest&#47;dist&#47;deckdeckgo-slide-chart&#47;deckdeckgo-slide-chart.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-chart-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-chart">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-chart</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-chart-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-chart-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-chart&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-chart-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-chart&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-chart-usage">Usage</h2>
          <p>
            The &quot;Chart&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-chart/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-slide-chart src=&quot;.&#47;assets&#47;csv&#47;data-pie-chart.csv&quot;&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My Pie
              chart&lt;&#47;h1&gt;{'\n'}&lt;&#47;deckgo-slide-chart&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-chart-slots">Slots</h3>
          <p>
            The slot <code>title</code> is optional.
          </p>
          <h2 id="app-slide-chart-chart-components">Chart components</h2>
          <p>
            The slide &quot;Chart&quot; relies on the charts components <code>&lt;deckgo-pie-chart/&gt;</code>, <code>&lt;deckgo-line-chart/&gt;</code> and{' '}
            <code>&lt;deckgo-bar-chart/&gt;</code> which are described in the components <a href="/components/charts">documentation</a>.
          </p>
          <h2 id="app-slide-chart-installation-1">Installation</h2>
          <p>
            The <a href="https://deckdeckgo.com">DeckDeckGo</a> charts components are provided in separate extra library. If you don&#39;t use the{' '}
            <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to add the <a href="https://deckdeckgo.com">DeckDeckGo</a> chart to your
            project, you will need to install and integrate it from a CDN or <a href="https://www.npmjs.com/package/@deckdeckgo/charts">npm</a> as described in
            its <a href="https://docs.deckdeckgo.com/components/charts">installation guide</a>.
          </p>
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
            <tbody>
              <tr>
                <td>type</td>
                <td>string</td>
                <td>pie</td>
                <td>
                  The type of the chart, <code>pie</code>, <code>line</code> or <code>bar</code>
                </td>
              </tr>
              <tr>
                <td>custom-background</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you would provide a background for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
              <tr>
                <td>custom-actions</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you would provide actions for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
            </tbody>
          </table>
          <p>
            Furthermore, this slide component offers the same attributes as the <a href="https://deckdeckgo.com">DeckDeckGo</a> charts Web Component, see its{' '}
            <a href="/components/charts">documentation</a> for the details.
          </p>
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
            <tbody>
              <tr>
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
            </tbody>
          </table>
          <p>
            Furthermore, this slide component offers the exact same CSS4 variables as the <a href="https://deckdeckgo.com">DeckDeckGo</a> charts Web Component,
            see its <a href="/components/charts">documentation</a> for the details.
          </p>
          <h2 id="app-slide-chart-methods">Methods</h2>
          <p>The slide &quot;Chart&quot; exposes the following methods:</p>
          <h3 id="app-slide-chart-draw">Draw</h3>
          <p>In case you would like to draw or redraw your chart.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-chart&#039;);{'\n'}await slide.draw();</code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
