import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-running'
})
export class AppRunning {

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
        <main><h1 id="app-running-running">Running</h1>
<p>There are two options to run and showcase your presentation:</p>
<p>You could either publish your deck online and showcase it from there, by accessing it with its online url with your favorite browser. For example by accessing an URL like <a href="https://deckdeckgo.com">https://deckdeckgo.com</a>.</p>
<p>Or you could showcase your deck in your favorite browser by running it locally using the integrated dev server provided by the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit with the default access URL <a href="http://localhost:3000">http://localhost:3000</a>.</p>
<h2 id="app-running-local">Local</h2>
<p>To run your presentation, in a terminal, start the following command to bundle your slides and to keep listening to modifications:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm run start</code>
    </deckgo-highlight-code><p>If you wish to develop your presentation without adding it to the list of available deck of the remote control, run the following command instead:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm run start-no-remote</code>
    </deckgo-highlight-code><blockquote>
<p>Furthermore to serve your deck, both above commands are watching your presentation&#39;s source files for changes and will trigger a new build and reload in case of modifications</p>
</blockquote>
<p><a href="https://github.com/johnpapa/lite-server">lite-server</a> is use as integrated dev server.</p>
<h2 id="app-running-online">Online</h2>
<p>If you are looking to showcase your presentation from an online URL, have a look to the next chapter <a href="/docs/publishing">publishing</a> before deploying your deck on your hosting solution.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
