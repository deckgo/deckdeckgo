import {Component, h, State} from '@stencil/core';

import {AssetsService} from '../../../../services/core/assets/assets.service';

@Component({
  tag: 'app-team',
  styleUrl: 'app-team.scss'
})
export class AppTeam {
  @State()
  private assets: Assets | undefined = undefined;

  async componentWillLoad() {
    this.assets = await AssetsService.getInstance().assets();
  }

  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding">
          <h1>Team</h1>

          <div class="team">
            <div>
              {this.assets !== undefined && this.assets.team ? (
                <app-avatar src={this.assets.team.david} role="presentation" aria-label="David Dal Busco"></app-avatar>
              ) : (
                undefined
              )}

              <h2>David Dal Busco</h2>

              <p text-center>
                David is a freelancer by day and the creator of DeckDeckGo by night. He's also the organiser of the Ionic Meetup Zürich and used to play in a
                band called VanRonMaiden, which was probably the coolest band ever but no one will ever know 🤣
              </p>

              <div class="social-links">
                <a href="https://twitter.com/daviddalbusco">
                  <ion-icon name="logo-twitter" area-label="Twitter"></ion-icon>
                </a>

                <a href="https://daviddalbusco.com">
                  <ion-icon name="globe" area-label="Personal blog and website"></ion-icon>
                </a>

                <a href="https://dev.to/daviddalbusco">
                  <ion-icon src="./assets/icons/dev.svg" area-label="Dev"></ion-icon>
                </a>

                <a href="https://medium.com/@david.dalbusco">
                  <ion-icon src="./assets/icons/medium.svg" area-label="Medium"></ion-icon>
                </a>

                <a href="http://github.com/peterpeterparker">
                  <ion-icon name="logo-github" area-label="Github"></ion-icon>
                </a>
              </div>
            </div>

            <div>
              {this.assets !== undefined && this.assets.team ? (
                <app-avatar src={this.assets.team.nicolas} role="presentation" aria-label="Nicolas Mattia"></app-avatar>
              ) : (
                undefined
              )}

              <h2>Nicolas Mattia</h2>

              <p class="ion-text-center">Nicolas ... has a bio as soon as he'll send me a PR 😉</p>

              <div class="social-links">
                <a href="https://twitter.com/nasmattia">
                  <ion-icon name="logo-twitter" area-label="Twitter"></ion-icon>
                </a>

                <a href="https://nmattia.com">
                  <ion-icon name="globe" area-label="Personal blog and website"></ion-icon>
                </a>

                <a href="https://github.com/nmattia">
                  <ion-icon name="logo-github" area-label="Github"></ion-icon>
                </a>
              </div>
            </div>
          </div>
        </main>
      </ion-content>
    ];
  }
}
