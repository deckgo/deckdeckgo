import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-opensource',
  styleUrl: 'app-opensource.scss',
})
export class AppAbout {
  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding">
          <h1>Open Source</h1>

          <p>
            DeckDeckGo is <strong>open source</strong>. This art of licencing is the DNA of the project. Beside its primary goal, we hope that this platform
            will help us become better programmers as we are betting on the web and modern technologies using it as a wonderful training. We also hope that by
            sharing our code it will be beneficial for the community.
          </p>

          <h2>Licence</h2>

          <p>
            The platform and its applications are licensed under the AGPL v3 (or later) licence. Several separate components are licensed under MIT licence. The
            licence displayed in each projects (see their README.md) is decisive.
          </p>

          <h2>Repo</h2>

          <p>
            The open source code of DeckDeckGo is available on&nbsp;
            <a href="http://github.com/deckgo/deckdeckgo" rel="noopener noreferrer">
              GitHub&nbsp;
              <ion-icon name="logo-github" area-label="Github"></ion-icon>
            </a>
          </p>
        </main>
      </ion-content>,
    ];
  }
}
