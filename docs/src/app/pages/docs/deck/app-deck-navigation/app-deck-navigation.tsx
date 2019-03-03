import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-deck-navigation'
})
export class AppDeckNavigation {

  @Element() el: HTMLElement;

  constructor(private menuService: MenuService) {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content padding>
        <main><h1 id="app-deck-navigation-navigation">Navigation</h1>
<p>If you use the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit, the navigation methods are already bind out of the box.</p>
<p>However, if you wish to know more about the different options or are using the core of <a href="https://deckdeckgo.com">DeckDeckGo</a>, the <a href="https://deckdeckgo.com">DeckDeckGo</a> deck exposes the following asynchronous methods in case you would like to add navigation features to your project.</p>
<ul>
<li><a href="#app-deck-navigation-introduction">Introduction</a></li>
<li><a href="#app-deck-navigation-go-to-next-slide">Go to next slide</a><ul>
<li><a href="#app-deck-navigation-optional-parameters">Optional parameters</a></li>
</ul>
</li>
<li><a href="#app-deck-navigation-go-to-previous-slide">Go to previous slide</a><ul>
<li><a href="#app-deck-navigation-optional-parameters-1">Optional parameters</a></li>
</ul>
</li>
<li><a href="#app-deck-navigation-go-to-a-specific-slide">Go to a specific slide</a><ul>
<li><a href="#app-deck-navigation-parameters">Parameters</a></li>
</ul>
</li>
<li><a href="#app-deck-navigation-is-the-deck-at-the-begin">Is the deck at the begin</a></li>
<li><a href="#app-deck-navigation-is-the-deck-at-the-end">Is the deck at the end</a></li>
<li><a href="#app-deck-navigation-get-the-index-of-the-current-slide">Get the index of the current slide</a></li>
<li><a href="#app-deck-navigation-get-the-length-of-the-deck">Get the length of the deck</a></li>
</ul>
<h2 id="app-deck-navigation-introduction">Introduction</h2>
<p>In the following examples we are accessing the features available on the deck element <code>&lt;deckgo-deck&gt;</code>.</p>
<p>For example, in Vanilla Javascript, we would get a reference to the deck using the following selector:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">const deck = document.getElementsByTagName(&#039;deckgo-deck&#039;);</code>
    </deckgo-highlight-code><h2 id="app-deck-navigation-go-to-next-slide">Go to next slide</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.slideNext();</code>
    </deckgo-highlight-code><h3 id="app-deck-navigation-optional-parameters">Optional parameters</h3>
<table>
<thead>
<tr>
<th>Parameter</th>
<th>Type</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>slideAnimation</td>
<td>boolean</td>
<td>true</td>
<td>Set to <code>false</code> in case you would not like the inner animation of a slide, like the reveal or code animation for example, to be performed.</td>
</tr>
<tr>
<td>emitEvent</td>
<td>boolean</td>
<td>true</td>
<td>Set to <code>false</code> in case you would not like the events <code>slideNextDidChange</code> and <code>slidePrevDidChange</code> to be fired. Note that to use this parameter, the previous should be set too.</td>
</tr>
</tbody></table>
<p>For example:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.slideNext(false, false);</code>
    </deckgo-highlight-code><h2 id="app-deck-navigation-go-to-previous-slide">Go to previous slide</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.slidePrev();</code>
    </deckgo-highlight-code><h3 id="app-deck-navigation-optional-parameters-1">Optional parameters</h3>
<table>
<thead>
<tr>
<th>Parameter</th>
<th>Type</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>slideAnimation</td>
<td>boolean</td>
<td>true</td>
<td>Set to <code>false</code> in case you would not like the inner animation of a slide, like the reveal or code animation for example, to be performed.</td>
</tr>
<tr>
<td>emitEvent</td>
<td>boolean</td>
<td>true</td>
<td>Set to <code>false</code> in case you would not like the events <code>slideNextDidChange</code> and <code>slidePrevDidChange</code> to be fired. Note that to use this parameter, the previous should be set too.</td>
</tr>
</tbody></table>
<p>For example:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.slidePrev(false, false);</code>
    </deckgo-highlight-code><h2 id="app-deck-navigation-go-to-a-specific-slide">Go to a specific slide</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.slideTo(0); &#47;&#47; parameters: index: number, speed?: number | undefined</code>
    </deckgo-highlight-code><h3 id="app-deck-navigation-parameters">Parameters</h3>
<table>
<thead>
<tr>
<th>Parameter</th>
<th>Type</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>index</td>
<td>number</td>
<td></td>
<td>Slide index of the specific slide.</td>
</tr>
<tr>
<td>speed</td>
<td>number</td>
<td>300</td>
<td>The slide transition speed. Default 300ms.</td>
</tr>
<tr>
<td>emitEvent</td>
<td>boolean</td>
<td>true</td>
<td>In case you would not like to emit the event <code>slideToChange</code>. Note that if you would use this parameter, the above <code>speed</code> parameter must be provided too.</td>
</tr>
</tbody></table>
<h2 id="app-deck-navigation-is-the-deck-at-the-begin">Is the deck at the begin</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.isBeginning(); &#47;&#47; resolve a boolean</code>
    </deckgo-highlight-code><h2 id="app-deck-navigation-is-the-deck-at-the-end">Is the deck at the end</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.isEnd(); &#47;&#47; resolve a boolean</code>
    </deckgo-highlight-code><h2 id="app-deck-navigation-get-the-index-of-the-current-slide">Get the index of the current slide</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.getActiveIndex(); &#47;&#47; resolve a number</code>
    </deckgo-highlight-code><h2 id="app-deck-navigation-get-the-length-of-the-deck">Get the length of the deck</h2>
<deckgo-highlight-code language="javascript">
      <code slot="code">await deck.getLength(); &#47;&#47; resolve a number</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
