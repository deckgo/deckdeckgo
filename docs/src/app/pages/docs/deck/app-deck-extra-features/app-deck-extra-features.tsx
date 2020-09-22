import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-deck-extra-features',
})
export class AppDeckExtraFeatures {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-deck-extra-features-extra-features">Extra Features</h1>
          <p>
            The <a href="https://deckdeckgo.com">DeckDeckGo</a> deck exposes a couple of extra features which could be added to your presentation too if you
            don&#39;t already use the starter kit.
          </p>
          <h2 id="app-deck-extra-features-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-deck-extra-features-toggle-onoff-the-full-screen-mode">Toggle on/off the full screen mode</a>
            </li>
            <li>
              <a href="#app-deck-extra-features-print-the-presentation">Print the presentation</a>
            </li>
            <li>
              <a href="#app-deck-extra-features-mobile">Mobile</a>
            </li>
            <li>
              <a href="#app-deck-extra-features-init-slide-size">initSlideSize</a>
            </li>
            <li>
              <a href="#app-deck-extra-features-load-background">loadBackground</a>
            </li>
            <li>
              <a href="#app-deck-extra-features-lazy-load-all-content">lazyLoadAllContent</a>
            </li>
          </ul>
          <h2 id="app-deck-extra-features-toggle-onoff-the-full-screen-mode">Toggle on/off the full screen mode</h2>
          <deckgo-highlight-code language="javascript">
            <code slot="code">await deck.toggleFullScreen();</code>
          </deckgo-highlight-code>
          <h2 id="app-deck-extra-features-print-the-presentation">Print the presentation</h2>
          <deckgo-highlight-code language="javascript">
            <code slot="code">await deck.print();</code>
          </deckgo-highlight-code>
          <h2 id="app-deck-extra-features-init-slide-size">Init slide size</h2>
          <p>In case you would like to recalculate the slides&#39; size (width and height)</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">await deck.initSlideSize();</code>
          </deckgo-highlight-code>
          <h2 id="app-deck-extra-features-load-background">Load background</h2>
          <p>If you would dynamically change the deck background element you could forward that changes to also slides using the following method:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">await deck.loadBackground();</code>
          </deckgo-highlight-code>
          <h2 id="app-deck-extra-features-lazy-load-all-content">Lazy load all content</h2>
          <p>To load the content of each and every slides of the deck which would normally be lazy loaded when you would swipe through your slides.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">await deck.lazyLoadAllContent();</code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
