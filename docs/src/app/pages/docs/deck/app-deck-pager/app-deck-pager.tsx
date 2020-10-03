import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-pager',
})
export class AppDeckPager {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-pager-pager">Pager</h1>
          <p>A pager for the progression of the presentation can optionally be displayed in form of a progress circle bar.</p>
          <ul>
            <li>
              <a href="#app-deck-pager-limitation">Limitation</a>
            </li>
            <li>
              <a href="#app-deck-pager-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-deck-pager-install-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-deck-pager-install-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-deck-pager-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-deck-pager-usge">Usage</a>
            </li>
            <li>
              <a href="#app-deck-pager-customization">Customization</a>
            </li>
            <li>
              <a href="#app-deck-pager-events">Events</a>
            </li>
          </ul>
          <h2 id="app-deck-pager-limitation">Limitation</h2>
          <p>
            This pager only works in a single page presentation because it listens to events emitted by the deck on the <code>document</code> level.
          </p>
          <h2 id="app-deck-pager-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <h3 id="app-deck-pager-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> pager from a CDN. To
            do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;pager@latest&#47;dist&#47;deckdeckgo-pager&#47;deckdeckgo-pager.esm.js&quot;&gt;&lt;&#47;script&gt;
              {'\n'}&lt;script nomodule=&quot;&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;pager@latest&#47;dist&#47;deckdeckgo-pager&#47;deckdeckgo-pager.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-deck-pager-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/pager">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;pager</code>
          </deckgo-highlight-code>
          <h3 id="app-deck-pager-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-deck-pager-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;pager&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-deck-pager-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;pager&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-deck-pager-usage">Usage</h2>
          <p>
            In order to the pager to your presentation, provide it as last child of your deck using the slot <code>pager</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-pager slot=&quot;pager&quot;&gt;{'\n'} &lt;&#47;deckgo-pager&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-deck-pager-customization">Customization</h2>
          <p>The following options are available to style the pager:</p>
          <table>
            <thead>
              <tr>
                <th>CSS4 variable</th>
                <th>Default</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>--pager-size</td>
                <td>56px</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-margin-top</td>
                <td>8px</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-margin-end</td>
                <td>8px</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-margin-bottom</td>
                <td>8px</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-margin-start</td>
                <td>8px</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-background</td>
                <td>#eee</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-text-color</td>
                <td>#4c8dff</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-text-size</td>
                <td>0.5em</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-stroke-outer-width</td>
                <td>2.8</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-stroke-inner-width</td>
                <td>1.8</td>
                <td></td>
              </tr>
              <tr>
                <td>--pager-text-percentage-display</td>
                <td>none</td>
                <td>
                  Set to <code>block</code> to display a progression with percentage (for example: 35%)
                </td>
              </tr>
              <tr>
                <td>--pager-text-slides-display</td>
                <td>none</td>
                <td>
                  Set to <code>block</code> to display a progression as slides&#39; count (for example: 2/15)
                </td>
              </tr>
              <tr>
                <td>--pager-position-left</td>
                <td></td>
                <td>The left attribute of the absolute positioning of the pager over the deck</td>
              </tr>
              <tr>
                <td>--pager-position-right</td>
                <td></td>
                <td>The right attribute of the absolute positioning of the pager over the deck</td>
              </tr>
            </tbody>
          </table>
          <p>
            Note: of course if you would display both <code>--pager-text-percentage-display</code> and <code>--pager-text-slides-display</code> it would be a
            weird display, use just one at once.
          </p>
          <h1 id="app-deck-pager-events">Events</h1>
          <p>In case you would like to hook on the pager click, it triggers the following event:</p>
          <table>
            <thead>
              <tr>
                <th>Event</th>
                <th>Emitted value</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>pagerClick</td>
                <td></td>
                <td>Emitted when the user click on the pager.</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
