import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-lazy-loading'
})
export class AppEditLazyLoading {

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-edit-lazy-loading-lazy-loading">Lazy loading</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> is built for performance and tries to lazy load as much as possible the components of your presentation. Have a look at the Lighthouse score of the <a href="https://deckdeckgo.com">DeckDeckGo</a> website for reference.</p>
<p>In order to lazy load the images of your presentation, you could either use our dead simple component or tweak your images&#39; attributes like the following example below.</p>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> handles the lazy loading of images provided in both forms.</p>
<h2 id="app-edit-lazy-loading-lazy-image-component">Lazy Image component</h2>
<p>Here&#39;s an example of usage of our component:</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-lazy-img img-src=&quot;https:&#47;&#47;deckdeckgo.com&#47;assets&#47;favicon&#47;android-chrome-512x512.png&quot;&gt;{'\n'}&lt;&#47;deckgo-lazy-img&gt;</code>
    </deckgo-highlight-code><h2 id="app-edit-lazy-loading-tweak-image-tag">Tweak Image Tag</h2>
<p>Instead of providing the url of your images as <code>src</code>, which would trigger an instant loading of their content, please provide their url using the attribute <code>data-src</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;img data-src=&quot;https:&#47;&#47;deckdeckgo.com&#47;assets&#47;favicon&#47;android-chrome-512x512.png&quot;&#47;&gt;</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
