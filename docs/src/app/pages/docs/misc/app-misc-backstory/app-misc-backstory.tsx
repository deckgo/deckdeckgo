import {Component, Element} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-misc-backstory'
})
export class AppMiscBackstory {

  @Element() el: HTMLElement;

  constructor(private menuService: MenuService) {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content padding>
        <main><h1 id="app-misc-backstory-backstory">Backstory</h1>
<p>I had the opportunity to talk about Web Components and Ionic. While I was developing my presentation it came to my mind that I was not really following what I was about to present, that&#39;s why I wrapped up together <a href="https://deckdeckgo.com">DeckDeckGo</a>, this new tool to create a Progressive Web App alternative for simple presentations using Web Components.</p>
<p>I hope my little project will help you to create a slick presentation 🚀</p>
<p>To infinity and beyond 🖖</p>
<p><a href="https://twitter.com/daviddalbusco">David</a></p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
