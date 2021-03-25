import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-actions',
})
export class AppDeckActions {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-actions-actions">Actions</h1>
          <p>
            A <a href="https://deckdeckgo.com">DeckDeckGo</a> deck also contains an additional slot <code>actions</code>. In the starter kit, this slot is use
            to add a <code>social share</code> action for your presentation.
          </p>
          <h2 id="app-deck-actions-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-deck-actions-introduction">Introduction</a>
            </li>
            <li>
              <a href="#app-deck-actions-theming">Theming</a>
            </li>
            <li>
              <a href="#app-deck-actions-example">Example</a>
            </li>
          </ul>
          <h2 id="app-deck-actions-introduction">Introduction</h2>
          <p>
            The slot <code>actions</code> provided for the deck will be cloned into each slides of your presentation. Therefore this slot will follow the
            swiping.
          </p>
          <p>
            Also worth to notice, this slot will be <code>hidden</code> when the presentation will be displayed full screen.{' '}
          </p>
          <h2 id="app-deck-actions-theming">Theming</h2>
          <p>
            The following theming options will affect the slot <code>actions</code> if set on the <code>deckgo-deck</code> or any slides.
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
                <td>--slide-actions-top</td>
                <td>32px</td>
                <td>Top value</td>
              </tr>
              <tr>
                <td>--slide-actions-end</td>
                <td>32px</td>
                <td>In LTR, right value</td>
              </tr>
              <tr>
                <td>--slide-actions-start</td>
                <td></td>
                <td>In LTR, left value</td>
              </tr>
            </tbody>
          </table>
          <h2 id="app-deck-actions-example">Example</h2>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'} &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'} {'\n'} &lt;a href=&quot;#&quot;
              onclick=&quot;share()&quot; slot=&quot;actions&quot; style=&quot;color: inherit;&quot;&gt;&lt;ion-icon
              name=&quot;share&quot;&gt;&lt;&#47;ion-icon&gt;&lt;&#47;a&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
