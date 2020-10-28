import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-youtube',
})
export class AppComponentsYoutube {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.lazyLoadElements(this.el, 'deckgo-youtube');
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-youtube-youtube">YouTube</h1>
          <p>
            The &quot;YouTube&quot; component allows you to easily add a <a href="https://youtube.com">YouTube</a> video in almost any slide of your
            presentation.
          </p>
          <h2 id="app-components-youtube-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-youtube-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-youtube-installation">Installation</a> - <a href="#app-components-youtube-from-a-cdn">Using from a CDN</a> -{' '}
              <a href="#app-components-youtube-from-npm">Install from NPM</a> -{' '}
              <a href="#app-components-youtube-framework-integration">Framework integration</a>
            </li>
            <li>
              <a href="#app-components-youtube-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-youtube-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-youtube-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-youtube-methods">Methods</a>
                  <ul>
                    <li>
                      <a href="#app-components-youtube-lazy-load-the-video">Lazy load the video</a>
                    </li>
                    <li>
                      <a href="#app-components-youtube-modify-video-size-on-the-fly">Modify video size on the fly</a>
                    </li>
                    <li>
                      <a href="#app-components-youtube-play-the-video">Play the video</a>
                    </li>
                    <li>
                      <a href="#app-components-youtube-pause-the-video">Pause the video</a>
                    </li>
                  </ul>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-components-youtube-showcase">Showcase</h2>
          <div>
            <deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw" width={300} height={200}></deckgo-youtube>
          </div>

          <h2 id="app-components-youtube-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-components-youtube-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> lazy image component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;youtube@latest&#47;dist&#47;deckdeckgo-youtube&#47;deckdeckgo-youtube.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-youtube-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/qrcode">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;youtube</code>
          </deckgo-highlight-code>
          <h3 id="app-components-youtube-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-youtube-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;youtube&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-youtube-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;youtube&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-youtube-usage">Usage</h2>
          <p>
            The &quot;YouTube&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-youtube/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-youtube src=&quot;https:&#47;&#47;www.youtube.com&#47;watch?v=oUOjJIfPIjw&quot;&gt;{'\n'}&lt;&#47;deckgo-youtube&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-youtube-slots">Slots</h3>
          <p>No slots are available for this component.</p>
          <h3 id="app-components-youtube-attributes">Attributes</h3>
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
                <td>
                  The source url, the YouTube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by YouTube.
                </td>
              </tr>
              <tr>
                <td>width</td>
                <td>number</td>
                <td></td>
                <td>The width of the video player.</td>
              </tr>
              <tr>
                <td>height</td>
                <td>number</td>
                <td></td>
                <td>The height of the video player.</td>
              </tr>
              <tr>
                <td>frame-title</td>
                <td>string</td>
                <td></td>
                <td>A title for the frame, could be use for accessibility reason.</td>
              </tr>
              <tr>
                <td>instant</td>
                <td>boolean</td>
                <td>false</td>
                <td>In case you would like to load the video as soon as the component is loaded.</td>
              </tr>
            </tbody>
          </table>
          <p>
            Per default the video, respectively its <code>iframe</code>, won&#39;t be loaded (expect if you specify <code>instant</code> to <code>true</code>).
            Therefore it&#39;s up to you to call the method <code>lazyLoadContent</code> to create the video. The reason behind this decision is allowing you to
            lazy load your content.
          </p>
          <h3 id="app-components-youtube-methods">Methods</h3>
          <p>
            The <code>&lt;deckgo-youtube/&gt;</code> component exposes the following methods:
          </p>
          <h4 id="app-components-youtube-lazy-load-the-video">Lazy load the video</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">lazyLoadContent(): Promise&lt;void&gt;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-youtube-modify-video-size-on-the-fly">Modify video size on the fly</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">async updateIFrame(width: number, height: number)</code>
          </deckgo-highlight-code>
          <h4 id="app-components-youtube-play-the-video">Play the video</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">play(): Promise&lt;void&gt;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-youtube-pause-the-video">Pause the video</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">pause(): Promise&lt;void&gt;</code>
          </deckgo-highlight-code>
          <h3 id="app-components-youtube-examples">Examples</h3>
          <p>The following code:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-youtube width=&quot;200&quot; height=&quot;100&quot; instant=&quot;true&quot;
              src=&quot;https:&#47;&#47;www.youtube.com&#47;embed&#47;Y97mEj9ZYmE&quot; frameTitle=&quot;DeckDeckGo editor
              demo&quot;&gt;&lt;&#47;deckgo-youtube&gt;
            </code>
          </deckgo-highlight-code>
          <p>Renders the following video:</p>
          <div class="container ion-margin">
            <deckgo-youtube
              width={200}
              height={100}
              instant={true}
              src="https://www.youtube.com/embed/Y97mEj9ZYmE"
              frameTitle="DeckDeckGo editor demo"></deckgo-youtube>
          </div>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
