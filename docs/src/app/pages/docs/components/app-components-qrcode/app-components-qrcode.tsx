import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-qrcode',
})
export class AppComponentsQRCode {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-qrcode-qr-code">QR Code</h1>
          <p>
            The &quot;QR Code&quot; component is an extra component which let you add QR code in your slides, useful for example to display links and url and if
            you wish your audience to easily access them.
          </p>
          <p>
            To generate the QR code, the project <a href="https://github.com/kazuhikoarase/qrcode-generator">qrcode-generator</a> from{' '}
            <a href="https://github.com/kazuhikoarase">Kazuhiko Arase</a> is used.
          </p>
          <h2 id="app-components-qrcode-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-qrcode-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-qrcode-installation">Installation</a> -{' '}
              <a href="#app-components-qrcode-using-deckdeckgo-qr-code-from-a-cdn">Using DeckDeckGo QR Code from a CDN</a> -{' '}
              <a href="#app-components-qrcode-install-deckdeckgo-qr-code-from-npm">Install DeckDeckGo QR Code from NPM</a> -{' '}
              <a href="#app-components-qrcode-framework-integration">Framework integration</a>
            </li>
            <li>
              <a href="#app-components-qrcode-usage">Usage</a> - <a href="#app-components-qrcode-slot">Slot</a> -{' '}
              <a href="#app-components-qrcode-properties">Properties</a> - <a href="#app-components-qrcode-styling">Styling</a> -{' '}
              <a href="#app-components-qrcode-styling-type-img">Styling type img</a>
              <ul>
                <li>
                  <a href="#app-components-qrcode-methods">Methods</a>
                </li>
                <li>
                  <a href="#app-components-qrcode-examples">Examples</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-qrcode-qr-code-with-logo">QR Code with logo</a>
            </li>
          </ul>
          <h2 id="app-components-qrcode-showcase">Showcase</h2>
          <p>
            This Web Component let you generate QR code like the following as <code>svg</code> (default) or <code>img</code>:
          </p>
          <div>
            <deckgo-qrcode
              content="https://deckdeckgo.com"
              style={{'--deckgo-qrcode-size': '300px', '--deckgo-qrcode-color-fill': 'var(--ion-color-primary)'}}></deckgo-qrcode>
          </div>

          <p>Optionally you could also display a logo over your QR code:</p>
          <div>
            <deckgo-qrcode content="https://deckdeckgo.com" style={{'--deckgo-qrcode-size': '300px', '--deckgo-qrcode-color-fill': 'var(--ion-color-primary)'}}>
              <img slot="logo" src="/assets/img/deckdeckgo-logo.svg" />
            </deckgo-qrcode>
          </div>

          <h2 id="app-components-qrcode-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit this template is included. You don&#39;t need to install it so therefore you should skip the
              &quot;Installation&quot; chapter.
            </p>
          </blockquote>
          <h3 id="app-components-qrcode-using-deckdeckgo-qr-code-from-a-cdn">Using DeckDeckGo QR Code from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> Code from a CDN. To do
            so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;qrcode@latest&#47;dist&#47;deckdeckgo-qrcode&#47;deckdeckgo-qrcode.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-qrcode-install-deckdeckgo-qr-code-from-npm">Install DeckDeckGo QR Code from NPM</h3>
          <p>
            Install <a href="https://deckdeckgo.com">DeckDeckGo</a> - QR Code in your project from{' '}
            <a href="https://www.npmjs.com/package/@deckdeckgo/qrcode">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;qrcode</code>
          </deckgo-highlight-code>
          <h3 id="app-components-qrcode-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-qrcode-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;qrcode&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-qrcode-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;qrcode&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-qrcode-usage">Usage</h2>
          <p>
            The <code>&lt;deckgo-qrcode/&gt;</code> Web Component will generate per default a <code>&lt;svg/&gt;</code> QR code with a correction level set to
            high.
          </p>
          <p>
            Optionally, it&#39;s also possible to generate the QR code as an <code>&lt;img/&gt;</code> and/or to display a logo over it.
          </p>
          <h3 id="app-components-qrcode-slot">Slot</h3>
          <p>
            To display a logo over your QR code, this Web Component provide a <code>slot</code> called <code>logo</code>.
          </p>
          <h3 id="app-components-qrcode-properties">Properties</h3>
          <p>
            The <code>&lt;deckgo-qrcode/&gt;</code> expose the following properties:
          </p>
          <table>
            <thead>
              <tr>
                <th>Property</th>
                <th>Attribute</th>
                <th>Description</th>
                <th>Type</th>
                <th>Default</th>
                <th>
                  Only applies for type <code>&lt;img/&gt;</code>
                </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <code>content</code>
                </td>
                <td>
                  <code>content</code>
                </td>
                <td>The content, a text or an url, of the QR code to generate</td>
                <td>
                  <code>string</code>
                </td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>
                  <code>type</code>
                </td>
                <td>
                  <code>type</code>
                </td>
                <td>
                  The type of QR code to generate, <code>&lt;svg/&gt;</code> or <code>&lt;img/&gt;</code>
                </td>
                <td>
                  <code>string</code>
                </td>
                <td>
                  <code>svg</code>
                </td>
                <td></td>
              </tr>
              <tr>
                <td>
                  <code>qrCellSize</code>
                </td>
                <td>
                  <code>qr-cell-size</code>
                </td>
                <td>
                  The size of the cell, useful to generate a bigger QR code, specially in case of <code>&lt;img/&gt;</code>. Use it wisely, I suggest a value
                  between 0 and 20 for example.
                </td>
                <td>
                  <code>number</code>
                </td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>
                  <code>qrMargin</code>
                </td>
                <td>
                  <code>qr-margin</code>
                </td>
                <td>The size of the code margin, in case you would like more spacing</td>
                <td>
                  <code>number</code>
                </td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>
                  <code>qrAlt</code>
                </td>
                <td>
                  <code>qr-img-alt</code>
                </td>
                <td>An alternate text for the image of the QR code</td>
                <td>
                  <code>string</code>
                </td>
                <td></td>
                <td>X</td>
              </tr>
              <tr>
                <td>
                  <code>qrBackgroundColor</code>
                </td>
                <td>
                  <code>qr-img-background-color</code>
                </td>
                <td>
                  The background color of the QR code. The value should be provided in a RGB-hex format. For example: <code>FF0000</code>.
                </td>
                <td>
                  <code>string</code>
                </td>
                <td></td>
                <td>X</td>
              </tr>
              <tr>
                <td>
                  <code>qrFillColor</code>
                </td>
                <td>
                  <code>qr-img-fill-color</code>
                </td>
                <td>
                  The color use to fill the QR code. The value should be provided in a RGB-hex format. For example: <code>FF0000</code>.
                </td>
                <td>
                  <code>string</code>
                </td>
                <td></td>
                <td>X</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-qrcode-styling">Styling</h3>
          <p>
            The <code>&lt;deckgo-qrcode/&gt;</code> could be styled using the following CSS4 variables which would only applies on the type{' '}
            <code>&lt;svg/&gt;</code>:
          </p>
          <table>
            <thead>
              <tr>
                <th>CSS4 variable</th>
                <th>Default</th>
                <th>Note</th>
                <th>
                  Only applies for type <code>&lt;svg/&gt;</code>
                </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>--deckgo-qrcode-container-display</td>
                <td>inline-block</td>
                <td>The display property of the shadow host and the code container</td>
                <td></td>
              </tr>
              <tr>
                <td>--deckgo-qrcode-size</td>
                <td></td>
                <td>The size, width and height, of the QR code</td>
                <td>X</td>
              </tr>
              <tr>
                <td>--deckgo-qrcode-logo-ratio-size</td>
                <td>3</td>
                <td>
                  If you are injecting a logo, its size, width and height, will be calculated with <code>--deckgo-qrcode-size</code> (or <code>100%</code> if
                  not provided) divided by this value
                </td>
                <td>X</td>
              </tr>
              <tr>
                <td>--deckgo-qrcode-border-stroke</td>
                <td></td>
                <td>The border color of the QR code</td>
                <td>X</td>
              </tr>
              <tr>
                <td>--deckgo-qrcode-background-fill</td>
                <td>transparent</td>
                <td>The QR code&#39;s background</td>
                <td>X</td>
              </tr>
              <tr>
                <td>--deckgo-qrcode-color-fill</td>
                <td></td>
                <td>The QR code&#39;s color (the color of the QR code&#39;s squares it contains)</td>
                <td>X</td>
              </tr>
            </tbody>
          </table>
          <h4 id="app-components-qrcode-styling-type-img">Styling type img</h4>
          <p>
            In oder to style QR code if its type is set to <code>&lt;img/&gt;</code>, you will need to use properties instead of CSS4 variables.
          </p>
          <h3 id="app-components-qrcode-methods">Methods</h3>
          <p>
            The <code>&lt;deckgo-qrcode/&gt;</code> component exposes the following method in case you would like to refresh your QR code, for example on resize
            of the window on in case you would set its content asynchronously:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">generate() =&gt; Promise&lt;void&gt;</code>
          </deckgo-highlight-code>
          <h3 id="app-components-qrcode-examples">Examples</h3>
          <p>
            You could find all the examples in the{' '}
            <a href="https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/qrcode/src/index.html">src/index.html</a> of the project.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-qrcode content=&quot;https:&#47;&#47;deckdeckgo.com&quot; style=&quot;--deckgo-qrcode-size: 300px;&quot;&gt;{'\n'}
              &lt;&#47;deckgo-qrcode&gt;
            </code>
          </deckgo-highlight-code>
          <p>Example with a logo:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-qrcode content=&quot;https:&#47;&#47;myurl.com&quot;&gt;{'\n'} &lt;img slot=&quot;logo&quot; src=&quot;my-logo.svg&quot;&#47;&gt;{'\n'}
              &lt;&#47;deckgo-qrcode&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-qrcode-qr-code-with-logo">QR Code with logo</h2>
          <p>
            It&#39;s possible to display a logo over your QR Code as the code generated with this Web Component have a correction level set to high meaning, if
            I understand correctly, that your content is encoded and displayed multiple times inside the QR code. Therefore, even if the logo cover a part of
            it, it will be still possible for a reader to read the content from &quot;somewhere else&quot; in the code.
          </p>
          <p>However, test it carefully and play with the colours, cell-size and size of your code to ensure its readability.</p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
