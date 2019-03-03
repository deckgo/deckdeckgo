import {Component, Element} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-misc-collections'
})
export class AppMiscCollections {

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
        <main><h1 id="app-misc-collections-collections">Collections</h1>
<p>A collection of presentations and talks where <a href="https://deckdeckgo.com">DeckDeckGo</a> was used:</p>
<table>
<thead>
<tr>
<th>Title</th>
<th>Date and event</th>
<th>Author</th>
<th>Repo</th>
<th>Available online</th>
</tr>
</thead>
<tbody><tr>
<td>Pantallas táctiles</td>
<td>2019/01/23 <a href="https://goo.gl/maps/h7897TMAL792">Universitat Jaume I</a>, Castellón de la Plana (Spain)</td>
<td><a href="https://github.com/Dellos7">David López Castellote</a></td>
<td><a href="https://github.com/Dellos7/sap126-pantallas-tactiles">sap126-pantallas-tactiles</a></td>
<td><a href="https://dellos7.github.io/presentaciones/sap126-pantallas-tactiles/">https://bit.ly/2FFqXWX</a></td>
</tr>
<tr>
<td>Schaffhausen civictech prototype?</td>
<td>2018/12/08</td>
<td><a href="https://github.com/sansan88">Sandro Scalco</a></td>
<td></td>
<td><a href="https://schaffhausen.io">https://schaffhausen.io</a></td>
</tr>
<tr>
<td>Ionic v4 and web components</td>
<td>2018/10/26 <a href="https://www.meetup.com/fr-FR/Web-Zurich/events/255699446">Web Zürich October</a>, Zürich</td>
<td><a href="https://github.com/peterpeterparker">David Dal Busco</a></td>
<td><a href="https://github.com/peterpeterparker/webzueri">Web Zueri</a></td>
<td></td>
</tr>
<tr>
<td>Ionic v4, web components, shadow dom and beyond</td>
<td>2018/10/16 <a href="https://www.meetup.com/fr-FR/Pantalks-tech-non-tech-talks-Panter-AG-Zurich/events/255430094/">Pantalks</a>, Zürich</td>
<td><a href="https://github.com/peterpeterparker">David Dal Busco</a></td>
<td><a href="https://github.com/peterpeterparker/ionicv4-and-beyond">Ionicv4-and-beyond</a></td>
<td></td>
</tr>
</tbody></table>
<h2 id="app-misc-collections-send-me-your-slides">Send me your slides</h2>
<p>If you would publish online a presentation or talk you would have built with <a href="https://deckdeckgo.com">DeckDeckGo</a>, reach me out, I would be super duper happy to add it to the list of talks and presentations ❤️</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
