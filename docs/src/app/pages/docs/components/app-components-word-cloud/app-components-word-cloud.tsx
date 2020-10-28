import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-word-cloud',
})
export class AppComponentsWordCloud {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-word-cloud-word-cloud">Word Cloud</h1>
          <p>Write and render word clouds.</p>
          <p>
            This component is using <a href="https://d3js.org/">D3</a> and <a href="https://github.com/jasondavies/d3-cloud">d3-cloud</a> to render these.
          </p>
          <h2 id="app-components-word-cloud-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-word-cloud-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-word-cloud-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-components-word-cloud-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-components-word-cloud-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-components-word-cloud-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-word-cloud-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-word-cloud-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-word-cloud-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-word-cloud-events">Events</a>
                </li>
                <li>
                  <a href="#app-components-word-cloud-theming">Theming</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-components-word-cloud-showcase">Showcase</h2>
          <div style={{position: 'relative', width: '560px', height: '560px'}}>
            <deckgo-word-cloud>
              <code slot="words">
                How the Word Cloud Generator Works The layout algorithm for positioning words without overlap is available on GitHub under an open source
                license as d3-cloud. Note that this is the only the layout algorithm and any code for converting text into words and rendering the final output
                requires additional development.
              </code>
            </deckgo-word-cloud>
          </div>

          <h2 id="app-components-word-cloud-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-components-word-cloud-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> word cloud component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;word-cloud@latest&#47;dist&#47;deckdeckgo-word-cloud&#47;deckdeckgo-word-cloud.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-word-cloud-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/word-cloud">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;word-cloud</code>
          </deckgo-highlight-code>
          <h3 id="app-components-word-cloud-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-word-cloud-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;word-cloud&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-word-cloud-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;word-cloud&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-word-cloud-usage">Usage</h2>
          <p>
            The &quot;Word Cloud&quot; Web Component could be integrated using the tag <code>&lt;deckgo-word-cloud/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              {' '}
              &lt;deckgo-word-cloud&gt;{'\n'} &lt;code slot=&quot;words&quot;&gt;{'\n'} Each Word entered it will become part of the cloud{'\n'}{' '}
              &lt;&#47;code&gt;{'\n'} &lt;&#47;deckgo-word-cloud&gt;
            </code>
          </deckgo-highlight-code>
          <p>Becomes editable by setting the &quot;editable&quot; property to &quot;true&quot;.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              {' '}
              &lt;deckgo-word-cloud editable=&quot;true&quot;&gt;{'\n'} &lt;code slot=&quot;words&quot;&gt;&lt;&#47;code&gt;{'\n'}{' '}
              &lt;&#47;deckgo-word-cloud&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-word-cloud-slots">Slots</h3>
          <p>
            The words have to be provided using the slot <code>words</code>.
          </p>
          <h3 id="app-components-word-cloud-attributes">Attributes</h3>
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
                <td>editable</td>
                <td>boolean</td>
                <td>
                  <code>false</code>
                </td>
                <td>
                  To set the component has being editable (<code>contenteditable</code> will be applied on the <code>slot</code> on <code>click</code>)
                </td>
              </tr>
              <tr>
                <td>marginTop</td>
                <td>number</td>
                <td>32</td>
                <td>Margin top in pixels</td>
              </tr>
              <tr>
                <td>marginBottom</td>
                <td>number</td>
                <td>32</td>
                <td>Margin bottom in pixels</td>
              </tr>
              <tr>
                <td>marginLeft</td>
                <td>number</td>
                <td>32</td>
                <td>Margin left in pixels</td>
              </tr>
              <tr>
                <td>marginRight</td>
                <td>number</td>
                <td>32</td>
                <td>Margin right in pixels</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-word-cloud-events">Events</h3>
          <p>
            The <code>&lt;deckgo-word-cloud/&gt;</code> component triggers the following event.
          </p>
          <table>
            <thead>
              <tr>
                <th>Event</th>
                <th>Description</th>
                <th>Type</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <code>wordCloudDidChange</code>
                </td>
                <td>Emit the host element when modified.</td>
                <td>
                  <code>CustomEvent&lt;HTMLElement&gt;</code>
                </td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-word-cloud-theming">Theming</h3>
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
                <td>--deckgo-word-cloud-empty-text</td>
                <td>&quot;Click to add your words&quot;</td>
                <td>
                  Place holder in case <code>editable</code> is set to <code>true</code>
                </td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
