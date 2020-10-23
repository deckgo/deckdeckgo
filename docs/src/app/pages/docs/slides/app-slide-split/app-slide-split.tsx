import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-split',
  styleUrl: 'app-slide-split.scss',
})
export class AppSlideContent {
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
          <h1 id="app-slide-split-slide-split">Slide: Split</h1>
          <p>The &quot;Split&quot; slide is a simple slide which display two panes on the page.</p>
          <h2 id="app-slide-split-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-split-layout">Layout</a>
              <ul>
                <li>
                  <a href="#app-slide-split-layout-horizontal">Horizontal</a>
                </li>
                <li>
                  <a href="#app-slide-split-layout-vertical">Vertical</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-split-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-split-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-split-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-split-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-split-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-split-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-split-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-split-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-split-layout">Layout</h2>
          <p>This template could split the content in two different ways.</p>
          <h3 id="app-slide-split-horizontal">Horizontal</h3>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-split>
                <p slot="start">The content you want to display on the left side of the page</p>
                <p slot="end">The content you want to display on the right side of the page</p>
              </deckgo-slide-split>
            </deckgo-deck>
          </div>

          <h3 id="app-slide-split-vertical">Vertical</h3>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-split vertical={true}>
                <p slot="start">The content you want to display on the top of the page</p>
                <p slot="end">The content you want to display on the bottom of the page</p>
              </deckgo-slide-split>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-split-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-split-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-split@latest&#47;dist&#47;deckdeckgo-slide-split&#47;deckdeckgo-slide-split.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-split-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-split">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-split</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-split-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-split-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-split&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-split-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-split&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-split-usage">Usage</h2>
          <p>
            The &quot;Split&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-split/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-split&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;Two columns subject&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;start&quot;&gt;{'\n'} The content you want to display on the left side of the page{'\n'} &lt;&#47;p&gt;{'\n'} &lt;p
              slot=&quot;end&quot;&gt;{'\n'} The content you want to display on the right side of the page{'\n'} &lt;&#47;p&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-split&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-split-slots">Slots</h3>
          <p>
            Both slots <code>title</code>, <code>start</code> and <code>end</code> are optional. Without providing one of them, the page will remain empty.
          </p>
          <p>
            The <code>start</code> slot is the content of the left pane respectively the slot <code>end</code> is the content of the right pane.
          </p>
          <p>
            Note: The slot <code>title</code> is per default hidden even if you provide it. See attributes below if you wish to display it.
          </p>
          <h2 id="app-slide-split-attributes">Attributes</h2>
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
                <td>vertical</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  Per default this template is horizontally split (two columns). Turn this property to <code>true</code> too display two rows respectively split
                  vertically
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
          <h2 id="app-slide-split-theming">Theming</h2>
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
                <td>--slide-split-background-start</td>
                <td></td>
                <td>Left split pane background</td>
              </tr>
              <tr>
                <td>--slide-split-color-start</td>
                <td></td>
                <td>Left split pane color</td>
              </tr>
              <tr>
                <td>--slide-split-background-end</td>
                <td></td>
                <td>Right split pane background</td>
              </tr>
              <tr>
                <td>--slide-split-color-end</td>
                <td></td>
                <td>Right split pane color</td>
              </tr>
              <tr>
                <td>--slide-split-padding-top</td>
                <td>0</td>
                <td>Padding top of a slide split pane</td>
              </tr>
              <tr>
                <td>--slide-split-padding-end</td>
                <td>64px</td>
                <td>Padding right of a slide split pane</td>
              </tr>
              <tr>
                <td>--slide-split-padding-bottom</td>
                <td>0</td>
                <td>Padding bottom of a slide split pane</td>
              </tr>
              <tr>
                <td>--slide-split-padding-start</td>
                <td>64px</td>
                <td>Padding left of a slide split pane</td>
              </tr>
              <tr>
                <td>--slide-split-title-padding-top</td>
                <td>16px</td>
                <td>Padding top of the title of the</td>
              </tr>
              <tr>
                <td>--slide-split-title-padding-end</td>
                <td>32px</td>
                <td>Padding right of the title of the</td>
              </tr>
              <tr>
                <td>--slide-split-title-padding-bottom</td>
                <td>16px</td>
                <td>Padding bottom of the title of the</td>
              </tr>
              <tr>
                <td>--slide-split-title-padding-start</td>
                <td>32px</td>
                <td>Padding left of the title of the</td>
              </tr>
              <tr>
                <td>--slide-padding-start</td>
                <td>32px</td>
                <td>Modify slotted ul and ol padding-inline-start</td>
              </tr>
              <tr>
                <td>--slide-split-align</td>
                <td>inherit</td>
                <td>Modify for example to center if you want to align the content in the middle</td>
              </tr>
              <tr>
                <td>--slide-split-text-align</td>
                <td>inherit</td>
                <td>Modify for example to center if you want to align the text in the middle</td>
              </tr>
              <tr>
                <td>--slide-split-title-display</td>
                <td>none</td>
                <td>
                  The <code>slot</code> title is per default hidden even if you provide it. If you wish to displays it, modify this attribute
                </td>
              </tr>
              <tr>
                <td>--zIndex</td>
                <td>1</td>
                <td>The z-index of the slide</td>
              </tr>
              <tr>
                <td>--slide-split-display-start</td>
                <td>flex</td>
                <td>Start side display property</td>
              </tr>
              <tr>
                <td>--slide-split-display-end</td>
                <td>flex</td>
                <td>End side display property</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
