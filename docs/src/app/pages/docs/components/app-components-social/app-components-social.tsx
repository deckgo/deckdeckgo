import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-social',
})
export class AppComponentsSocial {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-social-social">Social</h1>
          <p>The &quot;Social&quot; component helps you generate a social link to your Twitter, Dev, Medium, LinkedIn, GitHub accounts or a custom uri.</p>
          <h2 id="app-components-social-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-social-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-social-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-components-social-install-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-components-social-install-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-components-social-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-social-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-social-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-social-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-social-examples">Examples</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-components-social-showcase">Showcase</h2>
          <p>
            <deckgo-social twitter="daviddalbusco">
              <ion-icon slot="icon" name="logo-twitter"></ion-icon>
            </deckgo-social>
          </p>

          <p>
            <deckgo-social github="deckgo/deckdeckgo">
              <ion-icon slot="icon" name="logo-github"></ion-icon> DeckDeckGo on Github
            </deckgo-social>
          </p>

          <h2 id="app-components-social-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <h3 id="app-components-social-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> social component from
            a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;social@latest&#47;dist&#47;deckdeckgo-social&#47;deckdeckgo-social.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-social-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/social">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;social</code>
          </deckgo-highlight-code>
          <h3 id="app-components-social-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-social-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;social&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-social-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;social&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-social-usage">Usage</h2>
          <p>
            The &quot;Social&quot; Web Component could be integrated using the tag <code>&lt;deckgo-social/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'} &lt;img data-src=&quot;&#47;assets&#47;twitter.svg&quot;
              slot=&quot;icon&quot;&#47;&gt;{'\n'}&lt;&#47;deckgo-social&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-social-slots">Slots</h3>
          <p>
            The slot <code>icon</code> and the text are both optional.
          </p>
          <p>If you don&#39;t provide a text, component will renders a corresponding text for you.</p>
          <h4 id="app-components-social-examples">Examples</h4>
          <p>Automatic text:</p>
          <p>
            <deckgo-social twitter="daviddalbusco">
              <ion-icon slot="icon" name="logo-twitter"></ion-icon>
            </deckgo-social>
          </p>

          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'} &lt;ion-icon slot=&quot;icon&quot;
              name=&quot;logo-twitter&quot;&gt;&lt;&#47;ion-icon&gt;{'\n'}&lt;&#47;deckgo-social&gt;
            </code>
          </deckgo-highlight-code>
          <p>Custom text:</p>
          <p>
            <deckgo-social twitter="daviddalbusco">
              <ion-icon slot="icon" name="logo-twitter"></ion-icon>
              <span>A link to Twitter</span>
            </deckgo-social>
          </p>

          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'} &lt;ion-icon slot=&quot;icon&quot;
              name=&quot;logo-twitter&quot;&gt;&lt;&#47;ion-icon&gt;{'\n'} &lt;span&gt;A link to Twitter&lt;&#47;span&gt;{'\n'}&lt;&#47;deckgo-social&gt;
            </code>
          </deckgo-highlight-code>
          <p>Without icon:</p>
          <p>
            <deckgo-social twitter="daviddalbusco"></deckgo-social>
          </p>

          <deckgo-highlight-code language="javascript">
            <code slot="code">&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'}&lt;&#47;deckgo-social&gt;</code>
          </deckgo-highlight-code>
          <h3 id="app-components-social-attributes">Attributes</h3>
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
                <td>twitter</td>
                <td>string</td>
                <td></td>
                <td>
                  Your Twitter username. It will be concatenated automatically with <code>https://twitter.com/</code>
                </td>
              </tr>
              <tr>
                <td>linkedin</td>
                <td>string</td>
                <td></td>
                <td>
                  Your Linkedin username. It will be concatenated automatically with <code>https://www.linkedin.com/in/</code>
                </td>
              </tr>
              <tr>
                <td>medium</td>
                <td>string</td>
                <td></td>
                <td>
                  Your Medium username. <code>username</code> will be replaced automatically from <code>https://username.medium.com/</code>
                </td>
              </tr>
              <tr>
                <td>dev</td>
                <td>string</td>
                <td></td>
                <td>
                  Your Dev username. It will be concatenated automatically with <code>https://dev.to/</code>
                </td>
              </tr>
              <tr>
                <td>github</td>
                <td>string</td>
                <td></td>
                <td>
                  Your Github username. It will be concatenated automatically with <code>https://github.com/</code>
                </td>
              </tr>
              <tr>
                <td>fullUrl</td>
                <td>string</td>
                <td></td>
                <td>In case you would like to provide the URI of your choice</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-social-examples-1">Examples</h3>
          <p>Without any icons:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;Twitter&lt;&#47;deckgo-social&gt;{'\n'}&lt;deckgo-social
              linkedin=&quot;david-dal-busco&quot;&gt;Linkedin&lt;&#47;deckgo-social&gt;{'\n'}&lt;deckgo-social
              medium=&quot;david.dalbusco&quot;&gt;Medium&lt;&#47;deckgo-social&gt;{'\n'}&lt;deckgo-social
              full-url=&quot;https:&#47;&#47;stackoverflow.com&#47;users&#47;5404186&#47;peter-parker&quot;&gt;Stackoverflow&lt;&#47;deckgo-social&gt;
            </code>
          </deckgo-highlight-code>
          <p>
            With for example <code>ion-icon</code>:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;{'\n'} &lt;ion-icon slot=&quot;icon&quot;
              name=&quot;logo-twitter&quot;&gt;&lt;&#47;ion-icon&gt;{'\n'} Twitter{'\n'}&lt;&#47;deckgo-social&gt;{'\n'}
              {'\n'}&lt;deckgo-social github=&quot;fluster&#47;deckdeckgo&quot;&gt;{'\n'} &lt;ion-icon slot=&quot;icon&quot;
              name=&quot;logo-github&quot;&gt;&lt;&#47;ion-icon&gt;{'\n'} DeckDeckGo on Github{'\n'}&lt;&#47;deckgo-social&gt;
            </code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
