import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-playground',
  styleUrl: 'app-slide-playground.scss',
})
export class AppSlidePlayground {
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
          <h1 id="app-slide-playground-slide-playground">Slide: Playground</h1>
          <p>
            The &quot;Playground&quot; template helps embed easily playgrounds as <a href="https://codepen.io">Codepen</a>,{' '}
            <a href="https://jsfiddle.net/">JSFiddle</a> and <a href="https://webcomponents.dev">WebComponents.dev</a> in your presentation.
          </p>
          <h2 id="app-slide-playground-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-playground-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-playground-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-playground-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-playground-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-playground-framework-integration">Framework integration</a>
                </li>
                <li>
                  <a href="#app-slide-playground-youtube-component">YouTube component</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-playground-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-playground-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-slide-playground-youtube-component">YouTube component</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-playground-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-playground-theming">Theming</a>
            </li>
            <li>
              <a href="#app-slide-playground-methods">Methods</a>
              <ul>
                <li>
                  <a href="#app-slide-playground-play-the-video">Play the video</a>
                </li>
                <li>
                  <a href="#app-slide-playground-pause-the-video">Pause the video</a>
                </li>
                <li>
                  <a href="#app-slide-playground-toggle-the-video">Toggle the video</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-slide-playground-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-playground src="https://codepen.io/peterpeterparker/pen/dyGbOZm">
                <h1 slot="title">My Codepen</h1>
              </deckgo-slide-playground>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-playground-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-playground-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-playground@latest&#47;dist&#47;deckdeckgo-slide-playground&#47;deckdeckgo-slide-playground.esm.js&quot;&gt;&lt;&#47;script&gt;
              {'\n'}&lt;script nomodule=&quot;&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-playground@latest&#47;dist&#47;deckdeckgo-slide-playground&#47;deckdeckgo-slide-playground.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-playground-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-playground">npm</a> run the following
            command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-playground</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-playground-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-playground-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-playground&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-playground-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-playground&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-playground-usage">Usage</h2>
          <p>
            The &quot;Playground&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-playground/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-slide-playground src=&quot;https:&#47;&#47;codepen.io&#47;peterpeterparker&#47;pen&#47;dyGbOZm&quot;&gt;{'\n'} &lt;h1
              slot=&quot;title&quot;&gt;My Codepen&lt;&#47;h1&gt;{'\n'}&lt;&#47;deckgo-slide-playground&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-playground-slots">Slots</h3>
          <p>
            Both slots <code>title</code> and <code>content</code> are optional.
          </p>
          <h2 id="app-slide-playground-attributes">Attributes</h2>
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
                <td>The full link to your Pen, Fiddle oder WebComponents.dev. The component will take care of converting the link to an embeddable one.</td>
              </tr>
              <tr>
                <td>theme</td>
                <td>&#39;default&#39; or &#39;light&#39; or &#39;dark&#39;</td>
                <td>&#39;default</td>
                <td>The theming option if it can be applied respectivelly if supported by the third party playground, otherwise, &#39;default&#39;.</td>
              </tr>
              <tr>
                <td>width</td>
                <td>number</td>
                <td>Per default the playground width will be calculated according the content size available.</td>
                <td>Using this option you would be able to define your own width.</td>
              </tr>
              <tr>
                <td>height</td>
                <td>number</td>
                <td>Per default the playground height will be calculated according the content size available.</td>
                <td>Using this option you would be able to define your own height.</td>
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
          <h2 id="app-slide-playground-theming">Theming</h2>
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
              <tr>
                <td>--slide-playground-margin</td>
                <td>32px 0 32px</td>
                <td>The margin of the playground&#39;s container</td>
              </tr>
              <tr>
                <td>--slide-playground-height</td>
                <td>calc(100% - 32px)</td>
                <td>The height of the playground&#39;s container</td>
              </tr>
              <tr>
                <td>--slide-playground-overflow</td>
                <td>auto</td>
                <td>The overflow of the playground&#39;s container</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
