import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-remote-control',
  styleUrl: 'app-remote-control.scss'
})
export class AppRemoteControl {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-remote-control-remote-control">Remote control</h1>
          <p>Cherry on the cake 🍒🎂 DeckDeckGo comes with a Progressive Web App to remote control your slides 📱</p>
          <h2 id="app-remote-control-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-remote-control-how-does-it-work">How does it work</a>
            </li>
            <li>
              <a href="#app-remote-control-give-a-try">Give it a try</a>
            </li>
            <li>
              <a href="#app-remote-control-control-your-slides">Control your slides</a>
              <ul>
                <li>
                  <a href="#app-remote-control-features">Features</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-remote-control-time-tracker">Time tracker</a>
            </li>
          </ul>
          <h2 id="app-remote-control-how-does-it-work">How does it work</h2>
          <p>
            The same Progressive Web App <a href="https://deckdeckgo.app">remote control</a> could be used to control any presentations developed with{' '}
            <a href="https://deckdeckgo.com">DeckDeckGo</a>.
          </p>
          <p>
            If you are using the starter kit, out of the box, your presentation will signal itself as &quot;controllable&quot; and will therefore be
            &quot;discoverable&quot;.
          </p>
          <p>
            Once the presentation is linked to the <a href="https://deckdeckgo.app">remote control</a>, the communication between these will happen through a
            real-time channel using <a href="https://webrtc.org/">WebRTC</a>.
          </p>
          <h2 id="app-remote-control-give-it-a-try">Give it a try</h2>
          <p>
            Better than a long description, the DeckDeckGo demo website implements a deck and receiver to let you interact with this application, just give it a
            try:
          </p>
          <ol>
            <li>
              Open the DeckDeckGo demo website 👉 <a href="https://demo.deckdeckgo.com">https://demo.deckdeckgo.com</a>
            </li>
            <li>
              Start the <a href="https://deckdeckgo.app">remote control</a> PWA 👉 <a href="https://deckdeckgo.app">https://deckdeckgo.app</a>
            </li>
            <li>Have fun 🎉</li>
          </ol>
          <h2 id="app-remote-control-control-your-slides">Control your slides</h2>
          <p>
            Use the <a href="https://deckdeckgo.app">remote control</a> to remotely take control of your presentation. It makes it easy to interact with the
            slides while you are focusing on your talk/audience.
          </p>
          <h3 id="app-remote-control-features">Features</h3>
          <ul>
            <li>Switch between slides and/or swipe your slides</li>
            <li>Play and pause YouTube video</li>
            <li>Draw over your slide deck</li>
            <li>Highlight elements of your presentation</li>
          </ul>
          <h3 id="app-remote-control-video">Video</h3>
          <p>Have a look at this video where we demonstrate it!</p>
          <iframe width="560" height="315" src="https://www.youtube.com/embed/3o3oGBTTRSs" frameborder="0"></iframe>

          <p>And this one where we demonstrate how to use it!</p>
          <iframe width="560" height="315" src="https://www.youtube.com/embed/nxjHYBLTw5E" frameborder="0"></iframe>

          <h2 id="app-remote-control-time-tracker">Time tracker</h2>
          <p>
            The <a href="https://deckdeckgo.app">remote control</a> lets you track your talk&#39;s time too. This application implements local notifications to
            inform you when your talk is about to finish or is finished.
          </p>
          <h3 id="app-remote-control-screenshot">Screenshot</h3>
          <p>
            <img
              src="/assets/img/screenshots/remote-control/deckdeckgo-remote-control-timer.png"
              alt="The DeckDeckGo remote control timer"
              title="The DeckDeckGo remote control timer"
            />
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
