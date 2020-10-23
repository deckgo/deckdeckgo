import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-demo',
})
export class AppComponentsDemo {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-demo-demo">Demo</h1>
          <p>Display your apps or websites inside an Android or iOS frame.</p>
          <p>
            It is largely inspired and based on the awesome{' '}
            <a href="https://github.com/ionic-team/ionic-docs/tree/c5a624ac35d5285b871e7d8513d3849bdea63271/src/components/demo">work</a> of the{' '}
            <a href="https://ionicframework.com/">Ionic</a> team.
          </p>
          <h2 id="app-components-demo-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-demo-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-demo-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-components-demo-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-components-demo-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-components-demo-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-demo-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-demo-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-demo-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-demo-methods">Methods</a>
                </li>
                <li>
                  <a href="#app-components-demo-theming">Theming</a>
                </li>
                <li>
                  <a href="#app-components-demo-sizing">Sizing</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-components-demo-showcase">Showcase</h2>
          <div style={{position: 'relative'}}>
            <deckgo-demo style={{width: '304px', height: '704px'}} src="https://deckdeckgo.com" instant={true}></deckgo-demo>
          </div>

          <h2 id="app-components-demo-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <h3 id="app-components-demo-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> lazy image component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;demo@latest&#47;dist&#47;deckdeckgo-demo&#47;deckdeckgo-demo.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-demo-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/demo">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;demo</code>
          </deckgo-highlight-code>
          <h3 id="app-components-demo-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-demo-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;demo&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-demo-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;demo&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-demo-usage">Usage</h2>
          <p>
            The &quot;Demo&quot; Web Component could be integrated using the tag <code>&lt;deckgo-demo/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">&lt;deckgo-demo src=&quot;https:&#47;&#47;deckdeckgo.app&quot;&gt;&lt;&#47;deckgo-demo&gt;</code>
          </deckgo-highlight-code>
          <h3 id="app-components-demo-slots">Slots</h3>
          <p>No slots are available for this component.</p>
          <h3 id="app-components-demo-attributes">Attributes</h3>
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
                <td>src</td>
                <td>string</td>
                <td></td>
                <td>
                  The source Url of your application or website. This will be used as <code>src</code> attribute of the encapsulated <code>iframe</code>.
                </td>
              </tr>
              <tr>
                <td>frame-title</td>
                <td>string</td>
                <td></td>
                <td>A title for the frame, could be use for accessibility reason.</td>
              </tr>
              <tr>
                <td>mode</td>
                <td>&#39;md&#39; or &#39;ios&#39;</td>
                <td>&#39;md&#39;</td>
                <td>
                  The type of device frame. <code>md</code> for Android, <code>ios</code> for iPhone.
                </td>
              </tr>
              <tr>
                <td>instant</td>
                <td>boolean</td>
                <td>false</td>
                <td>In case you would like to load the frame as soon as the component is loaded.</td>
              </tr>
            </tbody>
          </table>
          <p>
            Per default the <code>iframe</code> is not be loaded (expect if you specify <code>instant</code> to <code>true</code>). Therefore it&#39;s up to you
            to call the method <code>lazyLoadContent</code> to create it. The reason behind this decision is allowing you to lazy load your content.
          </p>
          <h3 id="app-components-demo-methods">Methods</h3>
          <p>
            The <code>&lt;deckgo-demo/&gt;</code> component exposes the following methods:
          </p>
          <h4 id="app-components-demo-lazy-load-the-iframe">Lazy load the iframe</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">lazyLoadContent(): Promise&lt;void&gt;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-demo-refresh-iframe-size-and-reload-content">Refresh iframe size and reload content</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">updateIFrame(): Promise&lt;void&gt;</code>
          </deckgo-highlight-code>
          <h3 id="app-components-demo-theming">Theming</h3>
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
                <td>--deckgo-demo-placeholder-background</td>
                <td>#323639</td>
                <td>A background color for the content of the device until the frame is loaded.</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-demo-sizing">Sizing</h3>
          <p>
            On load and on window resize, the component will take care of resizing itself. It will compute the <code>width</code> and <code>height</code> of the
            host element to apply these to its content, to the shadowed iframe.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
