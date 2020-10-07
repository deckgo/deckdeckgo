import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-gif',
  styleUrl: 'app-slide-gif.scss',
})
export class AppSlideGif {
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
          <h1 id="app-slide-gif-slide-gif">Slide: GIF</h1>
          <p>
            The &quot;GIF&quot; slide let you add easily a gif, like those provided by <a href="https://giphy.com">Giphy</a>, to your presentation.
          </p>
          <h2 id="app-slide-gif-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-gif-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-gif-video">Video</a>
            </li>
            <li>
              <a href="#app-slide-gif-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-gif-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-gif-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-gif-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-gif-for-images-too">For images too</a>
            </li>
            <li>
              <a href="#app-slide-gif-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-gif-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-gif-gif-component">GIF component</a>
            </li>
            <li>
              <a href="#app-slide-gif-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-gif-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-gif-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
                <h1 slot="title">My title</h1>
                <h1 slot="top">Hey</h1>
                <h2 slot="bottom">It's a cool gif</h2>
              </deckgo-slide-gif>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-gif-video">Video</h2>
          <p>Have a look at this video where we demonstrate how to use it!</p>
          <iframe width="560" height="315" src="https://www.youtube.com/embed/0X3k3-yP7-Q" frameborder="0"></iframe>

          <h2 id="app-slide-gif-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-gif-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-gif@latest&#47;dist&#47;deckdeckgo-slide-gif&#47;deckdeckgo-slide-gif.esm.js&quot;&gt;&lt;&#47;script&gt;
              {'\n'}&lt;script nomodule=&quot;&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-gif@latest&#47;dist&#47;deckdeckgo-slide-gif&#47;deckdeckgo-slide-gif.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-gif-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-gif">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-gif</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-gif-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-gif-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-gif&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-gif-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-gif&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-gif-for-images-too">For images too</h2>
          <p>The slide GIF is useful for GIFs but could be use for any images too, in case you would like for example to display an image fullscreen.</p>
          <h2 id="app-slide-gif-usage">Usage</h2>
          <p>
            The &quot;GIF&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-gif/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-slide-gif src=&quot;https:&#47;&#47;media.giphy.com&#47;media&#47;xUA7baWfTjfHGLZc3e&#47;giphy.gif&quot; alt=&quot;My gif&quot;
              fullscreen=&quot;true&quot;&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My title&lt;&#47;h1&gt;{'\n'} &lt;h1
              slot=&quot;header&quot;&gt;Hey&lt;&#47;h1&gt;{'\n'} &lt;h2 slot=&quot;footer&quot;&gt;It&#039;s a cool gif&lt;&#47;h2&gt;{'\n'}
              &lt;&#47;deckgo-slide-gif&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-gif-slots">Slots</h3>
          <p>
            The slots <code>title</code>, <code>top</code> and <code>bottom</code> are both optional. <code>top</code> and <code>bottom</code> would be
            displayed over the gif.
          </p>
          <h2 id="app-slide-gif-gif-component">GIF component</h2>
          <p>
            The slide &quot;GIF&quot; relies on the component <code>&lt;deckgo-gif/&gt;</code> which is described in the components{' '}
            <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.
          </p>
          <h2 id="app-slide-gif-attributes">Attributes</h2>
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
                <td>The source url, the src, of the GIF. Could be an embeddable external url or a local one.</td>
              </tr>
              <tr>
                <td>alt</td>
                <td>string</td>
                <td></td>
                <td>And alt information could be provided for accessibility reason.</td>
              </tr>
              <tr>
                <td>fullscreen</td>
                <td>number</td>
                <td>true</td>
                <td>If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen.</td>
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
          <h2 id="app-slide-gif-theming">Theming</h2>
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
