import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-big-img',
  styleUrl: 'app-slide-big-img.scss',
})
export class AppSlideBigImg {
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
          <h1 id="app-slide-big-img-slide-big-image">Slide: Big Image</h1>
          <p>
            If you would like to display a fullscreen image in your presentation and select specific part of it, in order to to zoom in/highlight them, the
            &quot;Big Image&quot; slide is the one you are looking for.
          </p>
          <h2 id="app-slide-big-img-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-bigimg-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-bigimg-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-bigimg-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-bigimg-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-bigimg-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-bigimg-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-bigimg-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-bigimg-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-bigimg-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-big-img-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-big-img
                img-src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/big-img/showcase/big-deckdeckgo.jpg"
                img-divisions="500;1100;1700"
                axis="y"></deckgo-slide-big-img>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-big-img-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-big-img-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-big-img@latest&#47;dist&#47;deckdeckgo-slide-big-img&#47;deckdeckgo-slide-big-img.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-big-img-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-big-img">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-big-img</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-big-img-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-big-img-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-big-img&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-big-img-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-big-img&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-big-img-usage">Usage</h2>
          <p>
            The &quot;Big Image&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-big-img/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-slide-big-img{'\n'}{' '}
              img-src=&quot;https:&#47;&#47;raw.githubusercontent.com&#47;noelmace&#47;deckdeckgo&#47;big-img&#47;webcomponents&#47;slides&#47;big-img&#47;showcase&#47;big-deckdeckgo.jpg&quot;
              {'\n'} img-divisions=&quot;500;1100;1700&quot;{'\n'} axis=&quot;y&quot;&gt;{'\n'}&lt;&#47;deckgo-slide-big-img&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-big-img-slots">Slots</h3>
          <p>
            The slots <code>title</code> is optional (it is not displayed but could be use for the navigation). Otherwise no particular slots are currently
            available in order to display additional information on this template.
          </p>
          <h2 id="app-slide-big-img-attributes">Attributes</h2>
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
                <td>img-src</td>
                <td>string</td>
                <td></td>
                <td>An image URI which should be first displayed full screen before being divided in separate parts presented on the next slides.</td>
              </tr>
              <tr>
                <td>img-alt</td>
                <td>string</td>
                <td></td>
                <td>An optional accessibility alt for the image.</td>
              </tr>
              <tr>
                <td>img-divisions</td>
                <td>string</td>
                <td></td>
                <td>A list of anchors for the divisions of the image (in pixels).</td>
              </tr>
              <tr>
                <td>axis</td>
                <td>&#39;x&#39; or &#39;y&#39;</td>
                <td></td>
                <td>The axis which should be used to apply the division.</td>
              </tr>
              <tr>
                <td>reverse</td>
                <td>boolean</td>
                <td>false</td>
                <td>In which order should the specific part be highlighted.</td>
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
          <h2 id="app-slide-big-img-theming">Theming</h2>
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
                <td>--slide-img-max-width</td>
                <td></td>
                <td>
                  A maximal width value for the image. Useful in case you would like to display your deck in a container respectively not full window/screen.
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
