import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-author',
  styleUrl: 'app-slide-author.scss',
})
export class AppSlideAuthor {
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
          <h1 id="app-slide-author-slide-author">Slide: Author</h1>
          <p>The &quot;Author&quot; slide lets you introduce the author of the presentation.</p>
          <h2 id="app-slide-author-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-author-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-author-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-author-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-author-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-author-framework-integration">Framework integration</a>
                </li>
                <li>
                  <a href="#app-slide-author-social-component">Social component</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-author-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-author-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-slide-author-social-components">Social components</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-author-attributes">Attributes</a>
              <ul>
                <li>
                  <a href="#app-slide-author-example">Example</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-author-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-author-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-author img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
                <h1 slot="title">slot="title"</h1>
                <div slot="author">slot="author"</div>
                <div slot="social-link" style={{fontSize: '0.5rem'}}>
                  <deckgo-social twitter="daviddalbusco">
                    <ion-icon aria-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon>
                  </deckgo-social>
                </div>
                <div slot="social-link" style={{fontSize: '0.5rem'}}>
                  <deckgo-social linkedin="david-dal-busco">
                    <ion-icon aria-label="David on LinkedIn" slot="icon" name="logo-linkedin"></ion-icon>
                  </deckgo-social>
                </div>
              </deckgo-slide-author>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-author-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit this template is included. You don&#39;t need to install it so therefore you should skip the
              &quot;Installation&quot; chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-author-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-author@latest&#47;dist&#47;deckdeckgo-slide-author&#47;deckdeckgo-slide-author.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-author-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-author">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-author</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-author-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-author-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-author&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-author-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-author&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-author-social-component">Social component</h3>
          <p>
            This templates relies on the <code>@deckdeckgo/social</code> component without any explicit dependency. Therefore it should also be installed, see
            its related <a href="/components/social">installation</a> guide.
          </p>
          <h2 id="app-slide-author-usage">Usage</h2>
          <p>
            The &quot;Author&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-author/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-author img-src=&quot;&#47;assets&#47;author.jpeg&quot; img-alt=&quot;My self&quot;&gt;{'\n'} &lt;h1
              slot=&quot;title&quot;&gt;Author&lt;&#47;h1&gt;{'\n'} &lt;div slot=&quot;author&quot;&gt;{'\n'} &lt;h2&gt;David&lt;&#47;h2&gt;{'\n'}{' '}
              &lt;p&gt;Something about me&lt;&#47;p&gt;{'\n'} &lt;&#47;div&gt;{'\n'} &lt;div slot=&quot;social-link&quot;&gt;&lt;deckgo-social
              twitter=&quot;daviddalbusco&quot;&gt;&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'} &lt;&#47;deckgo-slide-author&gt;{'\n'}
              &lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-author-slots">Slots</h3>
          <p>
            Slots for <code>title</code>, <code>author</code> and <code>social-link</code> are optional. It is recommended that the slot <code>author</code> be
            filled as to improve the appearance of the slide.
          </p>
          <p>Notes:</p>
          <ul>
            <li>
              <p>
                The slot <code>title</code> is hidden. If you use the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter, it will be used for the
                navigation model.
              </p>
            </li>
            <li>
              <p>
                You could provide up to six <code>social-link</code> slots. Each of these could be your custom code or you could use the component{' '}
                <code>&lt;deckgo-social/&gt;</code> to easily provide a link to an external URI.
              </p>
            </li>
          </ul>
          <h3 id="app-slide-author-social-components">Social components</h3>
          <p>
            The details of the component <code>&lt;deckgo-social/&gt;</code> is described in the components{' '}
            <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.
          </p>
          <h2 id="app-slide-author-attributes">Attributes</h2>
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
                <td>An image URI, for example a picture of the author. Note: this image will be displayed as a circle.</td>
              </tr>
              <tr>
                <td>img-alt</td>
                <td>string</td>
                <td></td>
                <td>An optional accessibility alt for the image.</td>
              </tr>
              <tr>
                <td>mode</td>
                <td>
                  <code>&quot;circle&quot; | &quot;cover&quot; | &quot;none&quot;</code>
                </td>
                <td>
                  <code>&#39;cover&#39;</code>
                </td>
                <td>
                  The design to be applied to the image. <code>cover</code> fits the image to the start pane, <code>circle</code> displays it in a circle and{' '}
                  <code>none</code> in case you would not like to display an image
                </td>
              </tr>
              <tr>
                <td>custom-background</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you will provide a background for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
              <tr>
                <td>custom-actions</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you will provide actions for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-slide-author-example">Example</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-author img-src=&quot;&#47;assets&#47;author.jpeg&quot;&gt;{'\n'} &lt;div slot=&quot;author&quot;&gt;
              {'\n'} &lt;h2&gt;David&lt;&#47;h2&gt;{'\n'} &lt;p&gt;Something about me&lt;&#47;p&gt;{'\n'} &lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;social-link&quot;&gt;&lt;deckgo-social twitter=&quot;daviddalbusco&quot;&gt;&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;social-link&quot;&gt;&lt;deckgo-social linkedin=&quot;david-dal-busco&quot;&gt;&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;social-link&quot;&gt;&lt;deckgo-social medium=&quot;david.dalbusco&quot;&gt;&lt;&#47;deckgo-social&gt;&lt;&#47;div&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-author&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-author-theming">Theming</h2>
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
                <td>--slide-author-background-start</td>
                <td></td>
                <td>Left pane background</td>
              </tr>
              <tr>
                <td>--slide-author-color-start</td>
                <td></td>
                <td>Left pane color</td>
              </tr>
              <tr>
                <td>--slide-author-background-end</td>
                <td></td>
                <td>Right pane background</td>
              </tr>
              <tr>
                <td>--slide-author-color-end</td>
                <td></td>
                <td>Right pane color</td>
              </tr>
              <tr>
                <td>--slide-author-padding-top</td>
                <td>16px</td>
                <td>Padding top of a slide</td>
              </tr>
              <tr>
                <td>--slide-author-padding-end</td>
                <td>32px</td>
                <td>Padding right of a slide</td>
              </tr>
              <tr>
                <td>--slide-author-padding-bottom</td>
                <td>16px</td>
                <td>Padding bottom of a slide</td>
              </tr>
              <tr>
                <td>--slide-author-padding-start</td>
                <td>32px</td>
                <td>Padding left of a slide</td>
              </tr>
              <tr>
                <td>--slide-padding-start</td>
                <td>32px</td>
                <td>Modify slotted ul and ol padding-inline-start</td>
              </tr>
              <tr>
                <td>--slide-author-align</td>
                <td>inherit</td>
                <td>Modify for example to center if you want to align the content in the middle</td>
              </tr>
              <tr>
                <td>--slide-author-text-align</td>
                <td>inherit</td>
                <td>Modify for example to center if you want to align the text in the middle</td>
              </tr>
              <tr>
                <td>--slide-author-img-size</td>
                <td>80%</td>
                <td>The size of the image of the left pane</td>
              </tr>
              <tr>
                <td>--slide-author-img-border</td>
                <td></td>
                <td>
                  The border of the image of the left pane (only apply if <code>circle</code> mode is specified)
                </td>
              </tr>
              <tr>
                <td>--slide-author-social-padding-top</td>
                <td>32px</td>
                <td>The spacing between the author description and the social links</td>
              </tr>
              <tr>
                <td>--zIndex</td>
                <td>1</td>
                <td>The z-index of the slide</td>
              </tr>
              <tr>
                <td>--slide-author-social-link-padding</td>
                <td>8px</td>
                <td>Padding for the social links</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
