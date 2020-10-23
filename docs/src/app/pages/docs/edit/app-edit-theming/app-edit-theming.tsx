import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-theming',
})
export class AppEditTheming {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-edit-theming-theming">Theming</h1>
          <p>
            Theming a <a href="https://deckdeckgo.com">DeckDeckGo</a> presentation is quick and easy.
          </p>
          <h2 id="app-edit-theming-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-edit-theming-theme">Theme</a>
            </li>
            <li>
              <a href="#app-edit-theming-custom-styles">Custom styles</a>
            </li>
            <li>
              <a href="#app-edit-theming-notes">Notes</a>
            </li>
          </ul>
          <h2 id="app-edit-theming-theme">Theme</h2>
          <p>
            To create the theme for your deck when you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit, use the{' '}
            <a href="https://ionicframework.com/docs/theming/color-generator">Ionic Color Generator</a> to pick the colors of your choice.
          </p>
          <p>
            Once done, <code>copy</code> the generated CSS variables and <code>paste</code> them into the <code>src/css/variables.css</code> file of your
            presentation.
          </p>
          <p>
            Finally, edit the <code>index.html</code> and replace the color of the <code>body</code> background.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              body &#123;{'\n'} background: #3880ff;{'\n'}&#125;
            </code>
          </deckgo-highlight-code>
          <blockquote>
            <p>
              This background color is defined directly in the index.html file because it is the one which will be applied until your presentation is loaded
            </p>
          </blockquote>
          <h2 id="app-edit-theming-custom-styles">Custom styles</h2>
          <p>
            <a href="https://deckdeckgo.com">DeckDeckGo</a> also offers various theming options which could be set using CSS variables and which are described
            in their respective slides&#39; templates, see chapter <a href="/slides/concept">concept</a> to begin with.
          </p>
          <h2 id="app-edit-theming-notes">Notes</h2>
          <p>
            If you think we missed or need further theming options, don&#39;t hesitate to open an issue and/or submit a PR, it would be my pleasure to add more
            theming flexibility and options.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
