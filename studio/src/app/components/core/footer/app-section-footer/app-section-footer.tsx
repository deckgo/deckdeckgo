import {Component, h, Prop} from '@stencil/core';

@Component({
  tag: 'app-section-footer',
  styleUrl: 'app-section-footer.scss',
  shadow: false,
})
export class AppSectionFooter {
  @Prop()
  action: boolean = true;

  render() {
    return (
      <footer>
        <img src={`/assets/img/landing/wave-remote.svg`} role="presentation" loading="lazy" class="wave-section" />

        {this.renderAction()}

        <section class="ion-padding links">
          <app-footer display="landing"></app-footer>
        </section>
      </footer>
    );
  }

  private renderAction() {
    if (!this.action) {
      return undefined;
    }

    return (
      <section class="ion-padding ion-text-center">
        <h3>Start now.</h3>

        <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="primary">
          <ion-label>Write a presentation</ion-label>
        </ion-button>
      </section>
    );
  }
}
