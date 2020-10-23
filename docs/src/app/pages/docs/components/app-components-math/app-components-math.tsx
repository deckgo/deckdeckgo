import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-math',
})
export class AppComponentsMath {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-math-demo">Demo</h1>
          <p>Write and render math expressions.</p>
          <p>
            This component is using <a href="https://katex.org/">Katex</a> to renders these.
          </p>
          <h2 id="app-components-math-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-math-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-math-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-components-math-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-components-math-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-components-math-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-math-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-math-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-math-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-math-events">Events</a>
                </li>
                <li>
                  <a href="#app-components-math-theming">Theming</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-components-math-showcase">Showcase</h2>
          <div style={{position: 'relative'}}>
            <deckgo-math>
              <code slot="math">{`% \\f is defined as f(#1) using the macro
      \\f{x} = \\int_{-\\infty}^\\infty
      \\hat \\f\\xi\\,e^{2 \\pi i \\xi x}
      \\,d\\xi`}</code>
            </deckgo-math>
          </div>

          <h2 id="app-components-math-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <h3 id="app-components-math-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> lazy image component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;math@latest&#47;dist&#47;deckdeckgo-math&#47;deckdeckgo-math.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-math-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/math">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;math</code>
          </deckgo-highlight-code>
          <h3 id="app-components-math-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-math-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;math&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-math-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;math&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-math-usage">Usage</h2>
          <p>
            The &quot;Math&quot; Web Component could be integrated using the tag <code>&lt;deckgo-math/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-math&gt;{'\n'} &lt;code slot=&quot;math&quot;&gt;% &#092;f is defined as f(#1) using the macro{'\n'} &#092;f&#123;x&#125; =
              &#092;int_&#123;-&#092;infty&#125;^&#092;infty{'\n'} &#092;hat &#092;f&#092;xi&#092;,e^&#123;2 &#092;pi i &#092;xi x&#125;{'\n'}{' '}
              &#092;,d&#092;xi&lt;&#47;code&gt;{'\n'}&lt;&#47;deckgo-math&gt;
            </code>
          </deckgo-highlight-code>
          <p>It either supports a single expression, as displayed above, or expressions within paragraphs.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-math editable=&quot;true&quot;&gt;{'\n'} &lt;code slot=&quot;math&quot;&gt;{'\n'} You can write math expression inside paragraph like
              this: $x + 1${'\n'}
              {'\n'} Inline formulas can be written with &#092;$ e.g: $c = &#092;pm&#092;sqrt&#123;a^2 + b^2&#125;${'\n'}
              {'\n'} and displayed equations can be written using &#092;$ e.g: $&#092;sum_&#123;i=1&#125;^n 2^i${'\n'} &lt;&#47;code&gt;{'\n'}
              &lt;&#47;deckgo-math&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-math-slots">Slots</h3>
          <p>
            The expressions have to be provided using the slot <code>math</code>.
          </p>
          <h3 id="app-components-math-attributes">Attributes</h3>
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
                <td>macros</td>
                <td>string</td>
                <td>&#123;&quot;\\f&quot;:&quot;f(#1)&quot;&#123;</td>
                <td>
                  A collection of custom macros. Each macro is a property with a name like \name (written &quot;\name&quot; in JavaScript) which maps to a
                  string that describes the expansion of the macro, or a function that accepts an instance of MacroExpander as first argument and returns the
                  expansion as a string.
                </td>
              </tr>
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
            </tbody>
          </table>
          <p>
            See the <a href="https://katex.org/docs/options.html">Katex</a> documentation for more information.
          </p>
          <h3 id="app-components-math-events">Events</h3>
          <p>
            The <code>&lt;deckgo-math/&gt;</code> component triggers the following event.
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
                  <code>mathDidChange</code>
                </td>
                <td>Emit the host element when modified.</td>
                <td>
                  <code>CustomEvent&lt;HTMLElement&gt;</code>
                </td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-math-theming">Theming</h3>
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
                <td>--deckgo-math-color</td>
                <td>inherit</td>
                <td>Color</td>
              </tr>
              <tr>
                <td>--deckgo-math-background</td>
                <td></td>
                <td>Background</td>
              </tr>
              <tr>
                <td>--deckgo-math-padding</td>
                <td>8px</td>
                <td>Padding</td>
              </tr>
              <tr>
                <td>--deckgo-math-border-radius</td>
                <td></td>
                <td>Border radius</td>
              </tr>
              <tr>
                <td>--deckgo-math-margin</td>
                <td>0px</td>
                <td>Margin</td>
              </tr>
              <tr>
                <td>--deckgo-math-direction</td>
                <td></td>
                <td>Direction</td>
              </tr>
              <tr>
                <td>--deckgo-math-text-align</td>
                <td></td>
                <td>Text alignment</td>
              </tr>
              <tr>
                <td>--deckgo-math-container-display</td>
                <td>block</td>
                <td>Container display</td>
              </tr>
              <tr>
                <td>--deckgo-math-container-justify-content</td>
                <td></td>
                <td>Container justify-content attribute</td>
              </tr>
              <tr>
                <td>--deckgo-math-container-flex-direction</td>
                <td></td>
                <td>Container flex-direction attribute</td>
              </tr>
              <tr>
                <td>--deckgo-math-container-align-items</td>
                <td></td>
                <td>Container align-items attribute</td>
              </tr>
              <tr>
                <td>--deckgo-math-scroll</td>
                <td>scroll</td>
                <td>Scroll property of the expression(s)</td>
              </tr>
              <tr>
                <td>--deckgo-math-font-size</td>
                <td></td>
                <td>Font size property of the expression(s)</td>
              </tr>
              <tr>
                <td>--deckgo-math-min-height</td>
                <td>23px</td>
                <td>Minimal height property of the expression(s)</td>
              </tr>
              <tr>
                <td>--deckgo-math-display</td>
                <td>block</td>
                <td>Display property of the expression(s)</td>
              </tr>
              <tr>
                <td>--deckgo-math-code-empty-text</td>
                <td>&quot;Click to add your math expression&quot;</td>
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
