import {Component, Element, h, Host} from '@stencil/core';

@Component({
  tag: 'app-landing',
  styleUrl: 'app-landing.scss',
  shadow: false
})
export class AppLanding {
  @Element() el: HTMLElement;

  render() {
    return (
      <Host>
        <section class="header">
          <app-landing-deck></app-landing-deck>
        </section>

        <app-landing-content></app-landing-content>

        <app-landing-footer></app-landing-footer>
      </Host>
    );
  }
}
