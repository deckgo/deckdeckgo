import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-video',
  styleUrl: 'app-slide-video.scss',
})
export class AppSlideVideo {
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

  playPauseVideo(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const element: any = this.el.querySelector('deckgo-slide-video');

      if (!element) {
        resolve();
        return;
      }

      await element.toggle();

      resolve();
    });
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-slide-video-slide-video">Slide: Video</h1>
          <p>The &quot;Video&quot; slide let you add your own video or for example a GIF as MPEG-4 (MP4) to your presentation.</p>
          <h2 id="app-slide-video-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-video-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-video-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-video-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-video-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-video-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-video-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-video-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-slide-video-youtube-component">YouTube component</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-video-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-video-theming">Theming</a>
            </li>
            <li>
              <a href="#app-slide-video-methods">Methods</a>
              <ul>
                <li>
                  <a href="#app-slide-video-play-the-video">Play the video</a>
                </li>
                <li>
                  <a href="#app-slide-video-pause-the-video">Pause the video</a>
                </li>
                <li>
                  <a href="#app-slide-video-toggle-the-video">Toggle the video</a>
                </li>
                <li>
                  <a href="#app-slide-video-get-the-video">Get the video</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-slide-video-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-video src="https://media.giphy.com/media/vv41HlvfogHAY/giphy.mp4">
                <h1 slot="title">A GIF as video</h1>
                <button slot="actions" onClick={() => this.playPauseVideo()}>
                  Play/pause
                </button>
              </deckgo-slide-video>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-video-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-video-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-video@latest&#47;dist&#47;deckdeckgo-slide-video&#47;deckdeckgo-slide-video.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-video-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-video">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-video</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-video-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-video-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-video&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-video-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-video&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-video-usage">Usage</h2>
          <p>
            The &quot;Video&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-video/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-slide-video src=&quot;https:&#47;&#47;media.giphy.com&#47;media&#47;vv41HlvfogHAY&#47;giphy.mp4&quot;&gt;{'\n'} &lt;h1
              slot=&quot;title&quot;&gt;A GIF as video&lt;&#47;h1&gt;{'\n'}&lt;&#47;deckgo-slide-video&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-video-slots">Slots</h3>
          <p>
            The slot <code>title</code> and <code>content</code> are optional. The slot <code>content</code> is displayed before the video.
          </p>
          <h2 id="app-slide-video-attributes">Attributes</h2>
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
                <td>autoplay</td>
                <td>boolean</td>
                <td>false</td>
                <td>Automatically start the video when the slide is displayed/reached.</td>
              </tr>
              <tr>
                <td>loop</td>
                <td>boolean</td>
                <td>false</td>
                <td>Loop the video.</td>
              </tr>
              <tr>
                <td>muted</td>
                <td>boolean</td>
                <td>true</td>
                <td>Per default, the video is displayed without sounds.</td>
              </tr>
              <tr>
                <td>playsinline</td>
                <td>boolean</td>
                <td>true</td>
                <td>Per default, the video plays inline.</td>
              </tr>
              <tr>
                <td>type</td>
                <td>string</td>
                <td>&#39;video/mp4&#39;</td>
                <td>The type of video.</td>
              </tr>
              <tr>
                <td>src</td>
                <td>string</td>
                <td></td>
                <td>
                  The source of the video. Could be a video added in your <code>assets</code> or an url.
                </td>
              </tr>
              <tr>
                <td>width</td>
                <td>number</td>
                <td>Per default the video width will be calculated according the content size available.</td>
                <td>Using this option you would be able to define your own width.</td>
              </tr>
              <tr>
                <td>height</td>
                <td>number</td>
                <td>Per default the video height will be calculated according the content size available.</td>
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
          <h2 id="app-slide-video-theming">Theming</h2>
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
          <h2 id="app-slide-video-methods">Methods</h2>
          <p>
            The slide &quot;Video&quot; offers extra methods to play and pause the video clip. These methods are notably used by the [DeckDecGo]&#39;s remote
            control.
          </p>
          <h3 id="app-slide-video-play-the-video">Play the video</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-video&#039;);{'\n'}await slide.play();</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-video-pause-the-video">Pause the video</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-video&#039;);{'\n'}await slide.pause();</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-video-toggle-the-video">Toggle the video</h3>
          <p>Toggle will take care to pause or play the video according its current state.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-video&#039;);{'\n'}await slide.toggle();</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-video-get-the-video">Get the video</h3>
          <p>
            The component does not expose all attributes, if you would like to interact with the video you could get a reference using the following method:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              const slide = deck.getElementsByTagName(&#039;deckgo-slide-video&#039;);{'\n'}const video = await getVideo(); &#47;&#47; resolve an
              &lt;HTMLMediaElement&#47;&gt; element
            </code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
