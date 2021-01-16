import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-slide-more',
})
export class AppSlideMore {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-slide-more-more">More</h1>
          <p>More templates are available thanks to the wonderful work of the community.</p>
          <p>
            Developers and maintainers contributions are gathered in a <a href="https://github.com/deckgo-community">GitHub org</a>. Find there more templates
            and documentations about their usages.
          </p>
          <p>
            If you want to add your contributions to the community, <a href="https://deckdeckgo.com/contact">get in touch</a>.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
