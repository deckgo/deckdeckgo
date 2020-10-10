import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-gif',
})
export class AppComponentsGif {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);

    await this.lazyLoadGifs();
  }

  private lazyLoadGifs(): Promise<void> {
    return new Promise<void>((resolve) => {
      const gifElements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-gif');

      if (gifElements) {
        gifElements.forEach(async (element: HTMLElement) => {
          if (element && 'lazyLoadContent' in element) {
            await (element as any).lazyLoadContent();
          }
        });
      }

      resolve();
    });
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-gif-gif">GIF</h1>
          <p>
            The &quot;GIF&quot; component allows you to easily add a GIF, like those provided by{' '}
            <a href="https://giphy.com" rel="noopener noreferrer">
              Giphy
            </a>
            , in almost any slide of your presentation.
          </p>
          <h2 id="app-components-gif-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-gif-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-gif-installation">Installation</a>
            </li>
            <li>
              <a href="#app-components-gif-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-gif-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-gif-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-components-gif-theming">Theming</a>
            </li>
            <li>
              <a href="#app-components-gif-note">Note</a>
            </li>
          </ul>
          <h2 id="app-components-gif-showcase">Showcase</h2>
          <div>
            <deckgo-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" style={{'--width': '100%'}}>
              <h1 slot="header">Hey</h1>
              <h2 slot="footer">It's a cool gif</h2>
            </deckgo-gif>
          </div>

          <h2 id="app-components-gif-installation">Installation</h2>
          <p>
            This component is part of the &quot;GIF&quot; template. Therefore, if you would like to use it, install the related slide as described in its{' '}
            <a href="/slides/gif">installation</a> chapter.
          </p>
          <blockquote>
            <p>
              If you are using our Starter Kit this template is included. You don&#39;t need to install it so therefore you should skip the
              &quot;Installation&quot; chapter.
            </p>
          </blockquote>
          <h2 id="app-components-gif-usage">Usage</h2>
          <p>
            The &quot;GIF&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-gif/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-gif src=&quot;https:&#47;&#47;media.giphy.com&#47;media&#47;xUA7baWfTjfHGLZc3e&#47;giphy.gif&quot; alt=&quot;My gif&quot;
              fullscreen=&quot;true&quot;&gt;{'\n'} &lt;h1 slot=&quot;header&quot;&gt;Hey&lt;&#47;h1&gt;{'\n'} &lt;h2 slot=&quot;footer&quot;&gt;It&#039;s a
              cool gif&lt;&#47;h2&gt;{'\n'}&lt;&#47;deckgo-slide-gif&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-gif-slots">Slots</h3>
          <p>
            The slots <code>header</code> and <code>footer</code> are both optional. <code>header</code> and <code>footer</code> would be displayed over the
            gif.
          </p>
          <h2 id="app-components-gif-attributes">Attributes</h2>
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
                <td>false</td>
                <td>If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen.</td>
              </tr>
            </tbody>
          </table>
          <h2 id="app-components-gif-theming">Theming</h2>
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
                <td>--width</td>
                <td></td>
                <td>The width of the gif</td>
              </tr>
              <tr>
                <td>--height</td>
                <td></td>
                <td>The height of the gif</td>
              </tr>
              <tr>
                <td>--background</td>
                <td></td>
                <td>
                  The background of the <code>header</code> and <code>footer</code> over the gif
                </td>
              </tr>
              <tr>
                <td>--color</td>
                <td></td>
                <td>
                  The color of the <code>header</code> and <code>footer</code> over the gif
                </td>
              </tr>
              <tr>
                <td>--padding</td>
                <td></td>
                <td>
                  The padding of the <code>header</code> and <code>footer</code> over the gif
                </td>
              </tr>
              <tr>
                <td>--zIndex</td>
                <td>2</td>
                <td>The z-index of the slide</td>
              </tr>
              <tr>
                <td>--border-radius</td>
                <td></td>
                <td>A radius for the border of the gif container</td>
              </tr>
            </tbody>
          </table>
          <h2 id="app-components-gif-note">Note</h2>
          <p>
            Of course, as other images added to a presentation build with <a href="https://deckdeckgo.com">DeckDeckGo</a>, gifs are lazy loaded too.{' '}
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
