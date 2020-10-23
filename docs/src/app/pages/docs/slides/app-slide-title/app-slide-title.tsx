import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-title',
  styleUrl: 'app-slide-title.scss',
})
export class AppSlideTitle {
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
          <h1 id="app-slide-title-slide-title">Slide: Title</h1>
          <p>The &quot;Title&quot; slide is a simple slide which display its title and content centered in the middle of the page.</p>
          <p>This slide could be for example use for the very first and last slide of your presentation.</p>
          <h2 id="app-slide-title-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-title-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-title-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-title-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-title-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-title-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-title-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-title-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-title-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-title-example">Example</a>
            </li>
            <li>
              <a href="#app-slide-title-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-title-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-title>
                <h1 slot="title">My presentation title</h1>
                <p slot="content">Hello World 🚀</p>
              </deckgo-slide-title>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-title-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-title-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-title@latest&#47;dist&#47;deckdeckgo-slide-title&#47;deckdeckgo-slide-title.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-title-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-title">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-title</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-title-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-title-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-title&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-title-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-title&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-title-usage">Usage</h2>
          <p>
            The &quot;Title&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-title/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'} &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-title-slots">Slots</h3>
          <p>
            Both slots <code>title</code> and <code>content</code> are optional. Without providing one of them, the page will remain empty.
          </p>
          <h2 id="app-slide-title-attributes">Attributes</h2>
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
          <h2 id="app-slide-title-example">Example</h2>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;ul
              slot=&quot;content&quot;&gt;{'\n'} &lt;li&gt;Hello&lt;&#47;li&gt;{'\n'} &lt;li&gt;World&lt;&#47;li&gt;{'\n'} &lt;li&gt;🚀&lt;&#47;li&gt;{'\n'}{' '}
              &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-title-theming">Theming</h2>
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
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
