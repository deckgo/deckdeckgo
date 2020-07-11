import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-header-footer',
})
export class AppDeckHeaderFooter {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-header-footer-header--footer">Header &amp; Footer</h1>
          <p>
            Header and footer can be added to a <a href="https://deckdeckgo.com">DeckDeckGo</a> deck or to any slides using their dedicated slots{' '}
            <code>header</code> and <code>footer</code>.
          </p>
          <h2 id="app-deck-header-footer-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-deck-header-footer-introduction">Introduction</a>
            </li>
            <li>
              <a href="#app-deck-header-footer-slides">Slides</a>
            </li>
            <li>
              <a href="#app-deck-header-footer-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-deck-header-footer-introduction">Introduction</h2>
          <p>These elements are useful if you wish to display your brand or company logo on each slides.</p>
          <p>
            To achieve this behavior, you can provide a slot <code>header</code> or <code>footer</code> to the deck, these are going to be cloned into each
            slides of your presentation at runtime.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'} &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}
              {'\n'} &lt;div slot=&quot;header&quot;&gt;&lt;img data-src=&quot;.&#47;assets&#47;mylogo.png&quot;&#47;&gt;&lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;footer&quot;&gt;&lt;a href=&quot;https:&#47;&#47;deckdeckgo.com&quot;&gt;DeckDeckGo&lt;&#47;a&gt;&lt;&#47;div&gt;{'\n'}
              &lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-deck-header-footer-slides">Slides</h2>
          <p>
            If you want to display a specific <code>header</code> and <code>footer</code> on a particular slide, you proceed as displayed above but on the slide
            level.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'} &lt;&#47;p&gt;{'\n'}
              {'\n'} &lt;div slot=&quot;header&quot;&gt;&lt;img data-src=&quot;.&#47;assets&#47;mylogo.png&quot;&#47;&gt;&lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;footer&quot;&gt;&lt;a href=&quot;https:&#47;&#47;deckdeckgo.com&quot;&gt;DeckDeckGo&lt;&#47;a&gt;&lt;&#47;div&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <p>
            In case you would like to have <code>header</code> and <code>footer</code> defined for your deck but also assign specific ones on a particular
            slide, use the following attributes:
          </p>
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
                <td>custom-header</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  Set to <code>true</code> to defined a specific header for a slide
                </td>
              </tr>
              <tr>
                <td>custom-footer</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  Set to <code>true</code> to defined a specific footer for a slide
                </td>
              </tr>
            </tbody>
          </table>
          <p>For example:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title custom-header custom-footer&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation
              title&lt;&#47;h1&gt;{'\n'} &lt;p slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'} &lt;&#47;p&gt;{'\n'}
              {'\n'} &lt;div slot=&quot;header&quot;&gt;&lt;img data-src=&quot;.&#47;assets&#47;my-special-logo.png&quot;&#47;&gt;&lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;footer&quot;&gt;&lt;img data-src=&quot;.&#47;assets&#47;my-special-footer.png&quot;&#47;&gt;&lt;&#47;div&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-title&gt;{'\n'}
              {'\n'} &lt;div slot=&quot;header&quot;&gt;&lt;img data-src=&quot;.&#47;assets&#47;mylogo.png&quot;&#47;&gt;&lt;&#47;div&gt;{'\n'} &lt;div
              slot=&quot;footer&quot;&gt;&lt;a href=&quot;https:&#47;&#47;deckdeckgo.com&quot;&gt;DeckDeckGo&lt;&#47;a&gt;&lt;&#47;div&gt;{'\n'}
              &lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-deck-header-footer-theming">Theming</h2>
          <p>
            The following theming options will affect the slot <code>header</code> and <code>footer</code>.
          </p>
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
                <td>--slide-header-footer-z-index</td>
                <td>-1</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-header-max-height</td>
                <td>48px</td>
                <td>
                  On devices smaller as <code>1024px</code> the default value is <code>16px</code>
                </td>
              </tr>
              <tr>
                <td>--slide-header-justify-content</td>
                <td>flex-start</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-header-margin-top</td>
                <td>16px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-header-margin-end</td>
                <td>32px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-header-margin-bottom</td>
                <td>16px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-header-margin-start</td>
                <td>32px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-footer-max-height</td>
                <td>32px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-footer-justify-content</td>
                <td>center</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-footer-margin-top</td>
                <td>16px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-footer-margin-end</td>
                <td>16px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-footer-margin-bottom</td>
                <td>16px</td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-footer-margin-start</td>
                <td>16px</td>
                <td></td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
