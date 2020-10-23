import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-qrcode',
  styleUrl: 'app-slide-qrcode.scss',
})
export class AppSlideQRCode {
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
          <h1 id="app-slide-qrcode-slide-qr-code">Slide: QR Code</h1>
          <p>
            The &quot;QR code&quot; slide is an handy slide in case you would like to display a QR code. It could for example be use as the very last slide of
            your presentation to display an easy link pointing to your deck, you previously published online. It would allow your audience to get easily your
            slides without any delay on their phone.
          </p>
          <h2 id="app-slide-qrcode-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-qrcode-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-qrcode-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-qrcode-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-qrcode-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-qrcode-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-qrcode-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-qrcode-usage-1">Usage</a>
                </li>
                <li>
                  <a href="#app-slide-qrcode-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-qrcode-code-components">Code components</a>
            </li>
            <li>
              <a href="#app-slide-qrcode-installation">Installation</a>
            </li>
            <li>
              <a href="#app-slide-qrcode-attributes">Attributes</a>
              <ul>
                <li>
                  <a href="#app-slide-qrcode-example-without-any-slots">Example without any slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-qrcode-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-qrcode-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-qrcode content="https://deckdeckgo.com">
                <h1 slot="title">slot="title"</h1>
                <p slot="content">slot="content"</p>
              </deckgo-slide-qrcode>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-qrcode-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-qrcode-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-qrcode@latest&#47;dist&#47;deckdeckgo-slide-qrcode&#47;deckdeckgo-slide-qrcode.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-qrcode-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-qrcode">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-qrcode</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-qrcode-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-qrcode-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-qrcode&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-qrcode-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-qrcode&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-qrcode-usage">Usage</h2>
          <p>
            The &quot;QR code&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-qrcode/&gt;</code>.
          </p>
          <h3 id="app-slide-qrcode-usage-1">Usage</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-qrcode content=&quot;https:&#47;&#47;deckdeckgo.com&quot;&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My
              QR code&lt;&#47;h1&gt;{'\n'} &lt;p slot=&quot;content&quot;&gt;An optional additional content&lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-code&gt;
              {'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-qrcode-slots">Slots</h3>
          <p>
            The slots <code>title</code> and <code>content</code> are optional.
          </p>
          <h2 id="app-slide-qrcode-code-components">Code components</h2>
          <p>
            The slide &quot;QR Code&quot; relies on the code component <code>&lt;deckgo-qrcode/&gt;</code> which is described in the components{' '}
            <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.
          </p>
          <h2 id="app-slide-qrcode-installation-1">Installation</h2>
          <p>
            The <a href="https://deckdeckgo.com">DeckDeckGo</a> - QR Code component is provided in separate extra library. If you don&#39;t use the{' '}
            <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to add the <a href="https://deckdeckgo.com">DeckDeckGo</a> QR code to your
            project, you will need to install and integrate it from a CDN or <a href="https://www.npmjs.com/package/@deckdeckgo/qrcode">npm</a> as described in
            its <a href="https://docs.deckdeckgo.com/components/qrcode#app-components-qrcode-getting-started">installation guide</a>.
          </p>
          <h2 id="app-slide-qrcode-attributes">Attributes</h2>
          <p>
            The attribute <code>content</code> should be provided in order to render a QR code in this template. It offers the same attributes as the{' '}
            <a href="https://deckdeckgo.com">DeckDeckGo</a> QR code Web Component, see its{' '}
            <a href="https://docs.deckdeckgo.com/components/qrcode">documentation</a> for the details, and the following other attributes:
          </p>
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
                <td>img-src</td>
                <td>string</td>
                <td></td>
                <td>In case you would like to display a logo over the QR code, provide the source of the image. Note: this image is lazy loaded too.</td>
              </tr>
              <tr>
                <td>img-alt</td>
                <td>string</td>
                <td></td>
                <td>In case you would display a logo over the QR code, you could provide an accessibility attribute using this option.</td>
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
          <h3 id="app-slide-qrcode-example-without-any-slots">Example without any slots</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-qrcode content=&quot;An encoded text&quot;&gt;{'\n'} &lt;&#47;deckgo-slide-code&gt;{'\n'}
              &lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-qrcode-theming">Theming</h2>
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
                <td>--slide-qrcode-align</td>
                <td>center</td>
                <td>QR code vertical alignment</td>
              </tr>
              <tr>
                <td>--slide-qrcode-text-align</td>
                <td>center</td>
                <td>QR code horizontal alignment</td>
              </tr>
              <tr>
                <td>--slide-qrcode-background</td>
                <td></td>
                <td>QR code column&#39;s background</td>
              </tr>
              <tr>
                <td>--slide-qrcode-title-display</td>
                <td>block</td>
                <td>If you wish to hide the slot=&quot;title&quot;</td>
              </tr>
            </tbody>
          </table>
          <p>
            Furthermore, this slide component offers the exact same CSS4 variables as the <a href="https://deckdeckgo.com">DeckDeckGo</a> - QR code Web
            Component, see its <a href="https://docs.deckdeckgo.com/components/qrcode">documentation</a> for the details.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
