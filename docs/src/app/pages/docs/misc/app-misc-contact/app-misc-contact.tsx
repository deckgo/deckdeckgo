import {Component, Element, h} from '@stencil/core';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-misc-contact'
})
export class AppMiscContact {

  @Element() el: HTMLElement;

  private menuService: MenuService;

  constructor() {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-misc-contact-contact">Contact</h1>
<p>We would love to hear from you, ping us 😃</p>
<p>Email: <a href="mailto:hello@deckdeckgo.com">hello@deckdeckgo.com</a></p>
<p>Twitter: <a href="https://twitter.com/deckdeckgo">@deckdeckgo</a></p>
<p>On join us on our dedicated <a href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY">Slack</a> channel</p>
<p>If you would like to contribute, that would be really awesome! For feature requests, issues or even better Pull Requests, find us on <a href="https://github.com/deckgo/deckdeckgo">GitHub</a></p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
