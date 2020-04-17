import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-size',
})
export class AppDeckSize {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-size-size">Size</h1>
          <p>
            Per default, the <a href="https://deckdeckgo.com">DeckDeckGo</a> deck will use all the browser <code>window</code> size respectively width and
            height.
          </p>
          <p>
            However, it is possible to include or use <a href="https://deckdeckgo.com">DeckDeckGo</a> in any container, for that purpose you would only need to
            set the attribute <code>embedded</code> to <code>true</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;div style=&quot;width: 500px; height: 400px;&quot;&gt;{'\n'} &lt;deckgo-deck embedded=&quot;true&quot;&gt;{'\n'} &lt;deckgo-slide-title&gt;
              {'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;p slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'}{' '}
              &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'} &lt;&#47;deckgo-deck&gt;{'\n'}&lt;&#47;div&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-deck-size-tips-and-tricks">Tips and tricks</h2>
          <p>
            To detect the size of the container, the <code>offsetParent</code> property is used which returns the nearest ancestor that has a position other
            than static. Therefore, if the size would not be detected, try to set the <code>position</code> of your container to <code>relative</code> or{' '}
            <code>fixed</code>.{' '}
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
