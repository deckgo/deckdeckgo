import {Component, Prop, h} from '@stencil/core';

@Component({
  tag: 'app-footer',
  styleUrl: 'app-footer.scss',
  shadow: true,
})
export class AppFooter {
  @Prop() start: boolean = false;

  render() {
    const containerClass: string = !this.start ? 'container container-end' : 'container';

    return (
      <footer>
        <div class={containerClass}>
          {this.renderStart()}
          <div class="end">
            <a href="https://twitter.com/deckdeckgo">
              <ion-icon name="logo-twitter"></ion-icon>
            </a>
            <a href="https://github.com/deckgo">
              <ion-icon name="logo-github"></ion-icon>
            </a>
          </div>
        </div>
      </footer>
    );
  }

  private renderStart() {
    if (this.start) {
      return (
        <div class="start">
          <app-menu-footer></app-menu-footer>
        </div>
      );
    } else {
      return undefined;
    }
  }
}
