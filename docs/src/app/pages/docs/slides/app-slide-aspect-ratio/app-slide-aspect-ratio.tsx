import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-aspect-ratio',
  styleUrl: 'app-slide-aspect-ratio.scss',
})
export class AppSlideAspectRatio {
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
          <h1 id="app-slide-aspect-ratio-slide-aspect-ratio">Slide: Aspect Ratio</h1>
          <p>The &quot;Aspect Ratio&quot; slide is a template which preserves the content ratio regardless of the devices.</p>
          <p>
            We use this slide in the <a href="https://deckdeckgo.com">DeckDeckGo</a> editor to let users create slides containing shapes, for example to create
            schema.
          </p>
          <h2 id="app-slide-aspect-ratio-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-aspect-ratio-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-aspect-ratio-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-aspect-ratio-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-aspect-ratio-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-aspect-ratio-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-aspect-ratio-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-aspect-ratio-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-aspect-ratio-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-aspect-ratio-example">Example</a>
            </li>
            <li>
              <a href="#app-slide-aspect-ratio-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-aspect-ratio-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-aspect-ratio grid={true}>
                <h1 style={{position: 'absolute', top: '50%', left: '50%', transform: 'translate(-50%, -50%)', margin: '0'}}>Any elements</h1>
              </deckgo-slide-aspect-ratio>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-aspect-ratio-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-aspect-ratio-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use{' '}
            <a href="https://unpkg.com/" rel="noopener noreferrer">
              unpkg
            </a>{' '}
            if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-aspect-ratio@latest&#47;dist&#47;deckdeckgo-slide-aspect-ratio&#47;deckdeckgo-slide-aspect-ratio.esm.js&quot;&gt;&lt;&#47;script&gt;
              {'\n'}&lt;script nomodule=&quot;&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-aspect-ratio@latest&#47;dist&#47;deckdeckgo-slide-aspect-ratio&#47;deckdeckgo-slide-aspect-ratio.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-aspect-ratio-from-npm">From NPM</h3>
          <p>
            To install this template in your project from{' '}
            <a href="https://www.npmjs.com/package/@deckdeckgo/slide-aspect-ratio" rel="noopener noreferrer">
              npm
            </a>{' '}
            run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-aspect-ratio</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-aspect-ratio-framework-integration">Framework integration</h3>
          <p>
            The{' '}
            <a href="https://stenciljs.com/docs/overview" rel="noopener noreferrer">
              Stencil documentation
            </a>{' '}
            provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular" rel="noopener noreferrer">
              Angular
            </a>
            ,{' '}
            <a href="https://stenciljs.com/docs/react" rel="noopener noreferrer">
              React
            </a>
            ,{' '}
            <a href="https://stenciljs.com/docs/vue" rel="noopener noreferrer">
              Vue
            </a>{' '}
            and{' '}
            <a href="https://stenciljs.com/docs/ember" rel="noopener noreferrer">
              Ember
            </a>
            .
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-aspect-ratio-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-aspect-ratio&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-aspect-ratio-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-aspect-ratio&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-aspect-ratio-usage">Usage</h2>
          <p>
            The &quot;Aspect Ratio&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-aspect-ratio/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-aspect-ratio&gt;{'\n'} &lt;h1 style=&quot;position: absolute; top: 50%; left: 25%&quot;&gt;An
              element&lt;&#47;h1&gt;{'\n'} &lt;p style=&quot;position: absolute; top: 4%; left: 5%&quot;&gt;{'\n'} Another element{'\n'} &lt;&#47;p&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-aspect-ratio&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-aspect-ratio-slots">Slots</h3>
          <p>
            The slots <code>title</code>, <code>top</code> and <code>bottom</code> are both optional. <code>top</code> and <code>bottom</code> would be
            displayed over the content.
          </p>
          <h2 id="app-slide-aspect-ratio-attributes">Attributes</h2>
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
                <td>ratio</td>
                <td>number</td>
                <td>16 / 9</td>
                <td>The aspect ratio of the displayed content. Per default 16 being the width and 9 the height</td>
              </tr>
              <tr>
                <td>grid</td>
                <td>boolean</td>
                <td>false</td>
                <td>Display a grid behind the content. Note that the grid would only be display if not fullscreen</td>
              </tr>
              <tr>
                <td>editable</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  Per default <code>point-events</code> are set to <code>none</code> for this template making it read-only respectively not editable
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
          <h2 id="app-slide-aspect-ratio-example">Example</h2>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck embedded=&#123;true&#125;&gt;{'\n'} &lt;deckgo-slide-aspect-ratio grid=&#123;true&#125;&gt;{'\n'} &lt;h1
              style=&#123;&#123;position: &#039;absolute&#039;, top: &#039;50%&#039;, left: &#039;50%&#039;, transform: &#039;translate(-50%, -50%)&#039;,
              margin: &#039;0&#039;&#125;&#125;&gt;Any elements&lt;&#47;h1&gt;{'\n'} &lt;&#47;deckgo-slide-aspect-ratio&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-aspect-ratio-theming">Theming</h2>
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
                <td>--slide-grid-background</td>
                <td>
                  linear-gradient(to bottom, rgba(0, 0, 0, 0) 98%, rgba(110, 109, 111, 0.4) 98%), linear-gradient(to right, rgba(0, 0, 0, 0) 98%, rgba(110, 109,
                  111, 0.4) 98%)
                </td>
                <td>The default grid color</td>
              </tr>
              <tr>
                <td>--slide-grid-background-size</td>
                <td>2em 2em</td>
                <td>The default size of each squares of the grid</td>
              </tr>
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
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
