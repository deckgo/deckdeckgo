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
            According the window or container height in which the deck is provided, <a href="https://deckdeckgo.com">DeckDeckGo</a> will calculate a responsive
            base font size for the presentation.
          </p>
          <p>
            If you wish to overwrite this automatic setting, you can provide a custom size by applying a CSS4 variable <code>--slide-font-size</code> on the
            main <code>&lt;deckgo-deck/&gt;</code> element.
          </p>
          <h2 id="app-edit-fonts-google-fonts">Google Fonts</h2>
          <p>
            While using the developer kit, the <a href="https://deckdeckgo.com">DeckDeckGo</a> CLI will prompt you about the optional use of{' '}
            <a href="https://fonts.google.com">Google Fonts</a>, and subsequently will try gathering more information of the required font.
          </p>
          <p>
            Once all the information accumulated, it will automatically download the fonts, add them to your presentation and even update the settings in the
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
