import {Component, h} from '@stencil/core';

import {DeckdeckgoHighlightCodeTerminal} from '@deckdeckgo/highlight-code';

@Component({
  tag: 'app-home',
  styleUrl: 'app-home.scss',
})
export class AppHome {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content>
        <main class="ion-padding">
          <section class="hero">
            <app-logo></app-logo>
            <h1>
              The <strong>Progressive Web App</strong> alternative for simple presentations
            </h1>
          </section>

          <section class="overview" padding-top margin-top>
            <p no-margin>
              Create a PWA presentation using Web Components,{' '}
              <a href="https://ionicframework.com" rel="noopener norefferer">
                Ionic
              </a>{' '}
              components and HTML or Markdown 🚀 Cherry on the cake 🍒🎂 DeckDeckGo comes with a <a href="https://deckdeckgo.app">Progressive Web App</a> to
              remote control your slides 📱
            </p>

            <ul>
              <li>
                <ion-icon name="checkmark" padding-end></ion-icon>Progressive Web App
              </li>
              <li>
                <ion-icon name="checkmark" padding-end></ion-icon>Predefined templates
              </li>
              <li>
                <ion-icon name="checkmark" padding-end></ion-icon>Web Components
              </li>
              <li>
                <ion-icon name="checkmark" padding-end></ion-icon>Lazy loaded content
              </li>
              <li>
                <ion-icon name="checkmark" padding-end></ion-icon>HTML or Markdown
              </li>
              <li>
                <ion-icon name="checkmark" padding-end></ion-icon>Free and open source
              </li>
            </ul>
          </section>

          <section class="cta ion-text-center ion-padding">
            <div class="cta__primary">
              <h2>Get started now 🔥</h2>
              <deckgo-highlight-code class="ion-padding" terminal={DeckdeckgoHighlightCodeTerminal.NONE}>
                <code slot="code">$ npm init deckdeckgo</code>
              </deckgo-highlight-code>
            </div>
            <p class="cta__secondary">
              Dive deeper with the <a href="/docs">Getting Started</a> chapter
            </p>
          </section>

          <section class="section">
            <ul>
              <li>
                <ion-icon name="create"></ion-icon>
                <h3>Easy editing</h3>
                <p>HTML or Markdown, templates based, extra components and features, all you need to easily create, showcase and ship your presentation.</p>
              </li>
              <li>
                <ion-icon name="flash" padding-end></ion-icon>
                <h3>Slick and smooth</h3>
                <p>
                  Build for performance and to be SEO friendly, have a look to the Lighthouse score of the DeckDeckGo <a href="https://deckdeckgo.com">demo</a>.
                </p>
              </li>
              <li>
                <ion-icon name="code" padding-end></ion-icon>
                <h3>Modern</h3>
                <p>Presentations are bundled and shipped as Progressive Web Apps and based on Web Components.</p>
              </li>
              <li>
                <ion-icon name="phone-portrait" padding-end></ion-icon>
                <h3>Interact</h3>
                <p>
                  Control you slides or track your talk's time using the DeckDeckGo <a href="https://deckdeckgo.app">remote control PWA</a>.
                </p>
              </li>
            </ul>
          </section>
        </main>

        <app-footer start={true}></app-footer>
      </ion-content>,
    ];
  }
}
