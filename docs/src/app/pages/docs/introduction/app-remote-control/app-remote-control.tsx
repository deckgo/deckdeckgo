import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-remote-control',
  styleUrl: 'app-remote-control.scss'
})
export class AppRemoteControl {

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
        <main><h1 id="app-remote-control-remote-control">Remote control</h1>
<p>Cherry on the cake 🍒🎂 DeckDeckGo comes with a Progressive Web App to remote control your slides 📱</p>
<h2 id="app-remote-control-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-remote-control-how-does-it-work">How does it works</a></li>
<li><a href="#app-remote-control-give-a-try">Give a try</a></li>
<li><a href="#app-remote-control-control-your-slides">Control your slides</a><ul>
<li><a href="#app-remote-control-features">Features</a></li>
</ul>
</li>
<li><a href="#app-remote-control-time-tracker">Time tracker</a></li>
</ul>
<h2 id="app-remote-control-how-does-it-work">How does it work</h2>
<p>The same Progressive Web App <a href="https://deckdeckgo.app">remote control</a> could be use to control any presentations developed with <a href="https://deckdeckgo.com">DeckDeckGo</a>.</p>
<p>If you are using the starter kit, out of the box, your presentation will signal itself as &quot;controllable&quot; and will therefore be &quot;discoverable&quot;.   </p>
<p>One a presentation would be linked with the <a href="https://deckdeckgo.app">remote control</a>, the communication between these will happens through a real-time channel using <a href="https://webrtc.org/">WebRTC</a>.</p>
<h2 id="app-remote-control-give-a-try">Give a try</h2>
<p>Better than a description, the DeckDeckGo website implement a deck and receiver to let you interact with this application, just give it a try:</p>
<ol>
<li>Open the DeckDeckGo website 👉 <a href="https://deckdeckgo.com">https://deckdeckgo.com</a></li>
<li>Start the <a href="https://deckdeckgo.app">remote control</a> PWA 👉 <a href="https://deckdeckgo.app">https://deckdeckgo.app</a></li>
<li>Have fun 🎉</li>
</ol>
<h2 id="app-remote-control-control-your-slides">Control your slides</h2>
<p>Use the <a href="https://deckdeckgo.app">remote control</a> to take remotely the remotely of your presentation. Useful to interact with present your slides while you are focusing one your talk.</p>
<h3 id="app-remote-control-features">Features</h3>
<ul>
<li>Switch between slides and/or swipe your slides</li>
<li>Play and pause Youtube video</li>
<li>Draw over your slide deck</li>
<li>Highlight elements of your presentation</li>
</ul>
<h3 id="app-remote-control-screenshot">Screenshot</h3>
<p><img src="/assets/img/screenshots/remote-control/deckdeckgo-remote-control-interact.png" alt="The DeckDeckGo remote control" title="The DeckDeckGo remote control"/></p>
<h2 id="app-remote-control-time-tracker">Time tracker</h2>
<p>The <a href="https://deckdeckgo.app">remote control</a> let you track your talk&#39;s time too. This application implements local notifications to inform you when your talk is about to finish or is finished.</p>
<h3 id="app-remote-control-screenshot-1">Screenshot</h3>
<p><img src="/assets/img/screenshots/remote-control/deckdeckgo-remote-control-timer.png" alt="The DeckDeckGo remote control timer" title="The DeckDeckGo remote control timer"/></p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
