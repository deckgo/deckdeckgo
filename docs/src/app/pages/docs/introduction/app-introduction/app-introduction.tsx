import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-introduction',
})
export class AppIntroduction {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-introduction-getting-started">Getting started</h1>
          <p>
            <a href="https://deckdeckgo.com">DeckDeckGo</a> provides a <a href="https://github.com/deckgo/deckdeckgo/tree/master/cli">CLI</a> and a{' '}
            <a href="https://github.com/deckgo/deckdeckgo-starter">starter kit</a>.
          </p>
          <h2 id="app-introduction-prerequisites">Prerequisites</h2>
          <p>
            <a href="https://deckdeckgo.com">DeckDeckGo</a> requires a recent LTS version of <a href="https://nodejs.org">NodeJS</a> and npm. Make sure
            you&#39;ve installed and/or updated Node before continuing.
          </p>
          <blockquote>
            <p>Note that you will need to use NodeJS 10 or higher</p>
          </blockquote>
          <h2 id="app-introduction-starting-a-new-presentation">Starting a new presentation</h2>
          <p>Run the following command in a terminal to create a new deck:</p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm init deckdeckgo</code>
          </deckgo-highlight-code>
          <p>After running init you will be provided with a prompt which asks you to set the base information for your new deck:</p>
          <deckgo-highlight-code language="bash">
            <code slot="code">
              Cool, let&#039;s kick start a new DeckDeckGo presentation{'\n'}
              {'\n'}? What&#039;s your project name (will be use to create a new folder)? (deckdeckgo){'\n'}
              {'\n'}? What&#039;s your presentation name (max. 45 characters, will be use for meta tags and manifest information)? (DeckDeckGo){'\n'}
              {'\n'}? What&#039;s your presentation about (its description)? (Create a lightweight presentation using Web Components 🚀){'\n'}
              {'\n'}? What&#039;s your name (will be use for the author meta information)? (David)
            </code>
          </deckgo-highlight-code>
          <p>Once your presentation created, navigate to your new project&#39;s folder to start editing your slides and content for your talk 😉</p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
