import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-lazy-img',
})
export class AppComponentsLazyImg {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-lazy-img-lazy-image">Lazy Image</h1>
          <p>The &quot;Lazy Image&quot; component is a dead simple component to lazy load images.</p>
          <p>
            It leverages the native lazy-loading and the IntersectionObserver API (default behavior) to lazy load images. It also allows you to trigger
            &quot;manually&quot; their loading.
          </p>
          <p>
            An <code>&lt;img/&gt;</code> tag is per default use to display the image but optionally it could parse <code>SVG</code> too.
          </p>
          <h2 id="app-components-lazy-img-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-lazy-img-installation">Installation</a> - <a href="#app-components-lazy-img-from-a-cdn">Using from a CDN</a> -{' '}
              <a href="#app-components-lazy-img-from-npm">Install from NPM</a> -{' '}
              <a href="#app-components-lazy-img-framework-integration">Framework integration</a>
            </li>
            <li>
              <a href="#app-components-lazy-img-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-lazy-img-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-lazy-img-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-lazy-img-theming">Theming</a>
                </li>
                <li>
                  <a href="#app-components-lazy-img-methods">Methods</a>
                </li>
                <li>
                  <a href="#app-components-lazy-img-events">Events</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="(#app-components-lazy-img-fallbacks">Fallbacks</a>
            </li>
            <li>
              <a href="#app-components-lazy-img-trying-it-out">Trying it out</a>
            </li>
          </ul>
          <h2 id="app-components-lazy-img-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit this template is included. You don&#39;t need to install it so therefore you should skip the
              &quot;Installation&quot; chapter.
            </p>
          </blockquote>
          <h3 id="app-components-lazy-img-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> lazy image component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;lazy-img@latest&#47;dist&#47;deckdeckgo-lazy-img&#47;deckdeckgo-lazy-img.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-lazy-img-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/qrcode">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;lazy-img</code>
          </deckgo-highlight-code>
          <h3 id="app-components-lazy-img-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-lazy-img-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;lazy-img&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-lazy-img-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;lazy-img&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-lazy-img-usage">Usage</h2>
          <p>
            The &quot;Lazy Image&quot; Web Component could be integrated using the tag <code>&lt;deckgo-lazy-img/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">&lt;deckgo-lazy-img img-src=&quot;&#47;assets&#47;twitter.svg&quot;&gt;{'\n'}&lt;&#47;deckgo-lazy-img&gt;</code>
          </deckgo-highlight-code>
          <h3 id="app-components-lazy-img-slots">Slots</h3>
          <p>No slots are available for this component.</p>
          <h3 id="app-components-lazy-img-attributes">Attributes</h3>
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
                <td>The image source (= URI) to lazy load</td>
              </tr>
              <tr>
                <td>img-src-set</td>
                <td>string</td>
                <td></td>
                <td>The attribute &quot;srcset&quot; (= multiple URI) to lazy load in case you would like to provide multiple images for responsiveness</td>
              </tr>
              <tr>
                <td>img-error-src</td>
                <td>string</td>
                <td></td>
                <td>An optional image which could be displayed in case the main image would not be resolved</td>
              </tr>
              <tr>
                <td>img-sizes</td>
                <td>string</td>
                <td></td>
                <td>The set of media conditions to indicates what image size would be best to choose</td>
              </tr>
              <tr>
                <td>img-alt</td>
                <td>string</td>
                <td></td>
                <td>The image alternate text</td>
              </tr>
              <tr>
                <td>img-width</td>
                <td>number</td>
                <td></td>
                <td>The image width</td>
              </tr>
              <tr>
                <td>img-height</td>
                <td>number</td>
                <td></td>
                <td>The image height</td>
              </tr>
              <tr>
                <td>svg-src</td>
                <td>string</td>
                <td></td>
                <td>
                  The SVG image source (= URI) to lazy load and to parse (no <code>&lt;img/&gt;</code> tag will be use to render the svg)
                </td>
              </tr>
              <tr>
                <td>aria-label</td>
                <td>string</td>
                <td></td>
                <td>If you are using the above SVG option, provide the accessibility information using this attribute</td>
              </tr>
              <tr>
                <td>observer-root-margin</td>
                <td>string</td>
                <td>100px 0px</td>
                <td>
                  A string which specifies a set of offsets to add to the root&#39;s bounding_box when calculating intersections, effectively shrinking or
                  growing the root for calculation purposes.{' '}
                  <a href="https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver">More info.</a>
                </td>
              </tr>
              <tr>
                <td>observer-threshold</td>
                <td>number or number[]</td>
                <td></td>
                <td>
                  Either a single number or an array of numbers between 0.0 and 1.0, specifying a ratio of intersection area to total bounding box area for the
                  observed target. <a href="https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver">More info.</a>
                </td>
              </tr>
              <tr>
                <td>intrinsicsize</td>
                <td>string</td>
                <td>
                  An intrinsicsize for the native lazy-loading (see <a href="https://web.dev/native-lazy-loading">Native lazy-loading for the web</a>)
                </td>
                <td></td>
              </tr>
              <tr>
                <td>custom-loader</td>
                <td>boolean</td>
                <td>
                  In case you would like to take care by yourself to apply the load of the image. If turn to <code>true</code> then the component will emit an
                  event <code>customLoad</code> when the image intersect the viewport instead of displaying it (doesn&#39;t apply for <code>svg</code> but only
                  for <code>img-src</code> and <code>img-src-set</code>)
                </td>
                <td></td>
              </tr>
              <tr>
                <td>loading</td>
                <td>
                  <code>eager</code> or <code>lazy</code>
                </td>
                <td>
                  Per default <code>eager</code> because the intersection observer is used to defer the loading.
                </td>
                <td>
                  If set to <code>lazy</code>, the web native lazy capability of the browser, if available, will be used to lazy load the image
                </td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-lazy-img-theming">Theming</h3>
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
                <td>--deckgo-lazy-img-max-height</td>
                <td></td>
                <td>Image max height</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-max-width</td>
                <td>100%</td>
                <td>Image max width</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-min-height</td>
                <td></td>
                <td>Image min height</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-min-width</td>
                <td></td>
                <td>Image min width</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-pointer-events</td>
                <td>none</td>
                <td>Image pointer events</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-height</td>
                <td></td>
                <td>Image height</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-width</td>
                <td></td>
                <td>Image width</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-float</td>
                <td></td>
                <td>Image float</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-padding</td>
                <td></td>
                <td>Image padding</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-vertical-align</td>
                <td></td>
                <td>Image vertical alignment</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-display</td>
                <td></td>
                <td>The display property of the image</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-border-radius</td>
                <td></td>
                <td>In case you would like to specify a border radius for the image</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-object-fit</td>
                <td></td>
                <td>The property object-fit of the image</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-opacity-not-loaded</td>
                <td>0</td>
                <td>The opacity of the image when not loaded</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-opacity-loaded</td>
                <td>1</td>
                <td>The opacity of the image when loaded</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-transition</td>
                <td>opacity 0.15s linear</td>
                <td>The animation of the image, notably use to display smoothly the image when loaded</td>
              </tr>
              <tr>
                <td>--deckgo-lazy-img-box-shadow</td>
                <td></td>
                <td>Image box-shadow</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-lazy-img-methods">Methods</h3>
          <p>
            This component also export an async method <code>lazyLoad()</code> in case you would like to trigger &quot;manually&quot; the loading of the image.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">const element = document.querySelector(&#039;deckgo-lazy-img&#039;);{'\n'}await element.lazyLoad();</code>
          </deckgo-highlight-code>
          <h3 id="app-components-lazy-img-events">Events</h3>
          <p>
            The <code>&lt;deckgo-lazy-img/&gt;</code> will bubble the following events:
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
                  <code>customLoad</code>
                </td>
                <td>
                  Emitted if component property <code>custom-loader</code> is set to <code>true</code> and if an image (<code>img-src</code> or{' '}
                  <code>img-src-set</code>) as to be loaded.
                </td>
                <td>
                  <code>CustomEvent&lt;DeckDeckGoCustomLoad&gt;</code>
                </td>
              </tr>
            </tbody>
          </table>
          <p>
            Where <code>DeckDeckGoCustomLoad</code> contains the shadowed image element and both information <code>img-src</code> and <code>img-src-set</code>.
          </p>
          <h3 id="app-components-lazy-img-fallbacks">Fallbacks</h3>
          <p>
            In case the browser would not support the native native lazy-loading or the Intersection Observer API, images are going to be loaded without any
            delay when the component load respectively if the browser does not implement the Intersection Observer API images are displayed and not lazy loaded.
          </p>
          <h3 id="app-components-lazy-img-trying-it-out">Trying it out</h3>
          <p>
            This component lazy load images when these are not presented in the viewport. If you would use this component in a simple test containing only a
            couple of images, respectively no content or no real use case where the images are effectively offscreen, assign a default height to components in
            order to ensure that some are effectively placed outside of the window{' '}
            <a href="https://github.com/deckgo/deckdeckgo/issues/128#issuecomment-493979841">[#128]</a>.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
