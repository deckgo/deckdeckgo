import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-fonts',
})
export class AppEditFonts {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-edit-fonts-fonts">Fonts</h1>
          <p>
            As any web application, the fonts of your presentation could be easily styled, but <a href="https://deckdeckgo.com">DeckDeckGo</a> goes one step
            further by downloading and installing <strong>automatically</strong> any{' '}
            <a href="https://fonts.google.com" rel="noopener noreferrer">
              Google Fonts
            </a>{' '}
            you would like to use during the setup process (if you are using the starter kit).
          </p>
          <h2 id="app-edit-fonts-using-automatically-any-google-fonts">Using automatically any Google Fonts</h2>
          <p>
            After you have kick-started your presentation running <code>npm init deckdeckgo</code> in a terminal and provided the information as described in
            the <a href="https://docs.deckdeckgo.com/docs/introduction">Getting started</a> chapter, the <a href="https://deckdeckgo.com">DeckDeckGo</a> CLI
            will ask you if you are looking to use Google Fonts, and subsequently will try gathering more information of the required font.
          </p>
          <p>
            Once all the information is accumulated, it will automatically download the fonts, add them to your presentation and even update the settings in the
            CSS stylesheets for you.
          </p>
          <h2 id="app-edit-fonts-example">Example</h2>
          <p>
            The following is an example of the process if you would like to install the Google Font <code>Lato</code>:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              ? Do you want to use a Google Font for your presentation? (y&#47;N) Y{'\n'}
              {'\n'}⠼ Fetching fonts list...{'\n'}
              {'\n'}? Search a Google font (min. 3 characters)? ato{'\n'}
              {'\n'}? Select the font (Use arrow keys){'\n'} Atomic Age{'\n'}❯ Lato{'\n'} Search again{'\n'} Skip{'\n'}
              {'\n'}? Select charsets (Press &lt;space&gt; to select, &lt;a&gt; to toggle all, &lt;i&gt; to invert selection){'\n'}❯◉ latin{'\n'} ◯ latin-ext
              {'\n'}
              {'\n'}? Select styles (Press &lt;space&gt; to select, &lt;a&gt; to toggle all, &lt;i&gt; to invert selection){'\n'} ◯ 100{'\n'} ◯ 100italic{'\n'}{' '}
              ◯ 300{'\n'} ◯ 300italic{'\n'} ❯◉ regular{'\n'} ◯ italic{'\n'} ◯ 700{'\n'} (Move up and down to reveal more choices){'\n'}
              {'\n'}⠼ Downloading font{'\n'}⠼ Writing to CSS files...{'\n'}
            </code>
          </deckgo-highlight-code>
          <h2 id="app-edit-fonts-video">Video</h2>
          <p>Have a look at this video where we show how to do it!</p>
          <iframe width="560" height="315" src="https://www.youtube.com/embed/S6qL7JbxJ70" frameborder="0"></iframe>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
