import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-enterprise',
  styleUrl: 'app-enterprise.scss',
})
export class AppEnterprise {
  private featuresRef!: HTMLElement;
  private formRef!: HTMLElement;

  private scrollTo(ref: HTMLElement | undefined) {
    if (!ref) {
      return;
    }

    ref.scrollIntoView({
      behavior: 'smooth',
    });
  }

  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content>
        <section class="hero">
          <div class="content">
            <div>
              <h1>Professional presentation building, custom tailored to your brand.</h1>
              <h3 style={{'font-weight': '300'}}>
                Easily apply your corporate identity and design across all your company’s presentations. Secure distribution and maintenance.
              </h3>

              <ion-button class="ion-margin-top" shape="round" onClick={() => this.scrollTo(this.formRef)} mode="md" color="tertiary">
                <ion-label style={{'text-transform': 'none'}}>Get in touch</ion-label>
              </ion-button>
            </div>
          </div>

          <img class="wave" src={`/assets/img/landing/wave-start.svg`} role="presentation" />
        </section>

        <section class="intro">
          <div class="content">
            <blockquote>
              <span>❝</span>
              <p>Did you see Bob’s presentation? He used the wrong logo, again.</p>
            </blockquote>

            <h2>Templates and options that match your CI/CD.</h2>

            <div>
              <p>Unlike any other editor for presentations, DeckDeckGo assists you in applying your corporate identity to your company's presentations.</p>

              <p>The design, colors, fonts, styles and any other options you are defining in your templates are those your collaborators are using.</p>
            </div>

            <div>
              <p>Together with your designers, we create unbreakable slides for your design systems.</p>

              <p>Moreover, rolling out new design and logo has never been so easy and DeckDeckGo can offer so much more.</p>

              <button type="button" class="app-button" onClick={() => this.scrollTo(this.featuresRef)}>
                Discover more features <ion-icon src="/assets/icons/ionicons/arrow-forward.svg"></ion-icon>
              </button>
            </div>
          </div>
        </section>

        <div class="separator intro">
          <img src={`/assets/img/landing/wave-introducing.svg`} role="presentation" loading="lazy" />
        </div>

        <app-features ref={(el) => (this.featuresRef = el as HTMLElement)}></app-features>

        <div class="separator features">
          <img src={`/assets/img/landing/wave-audience.svg`} role="presentation" loading="lazy" />
        </div>

        <app-contact-form ref={(el) => (this.formRef = el as HTMLElement)}></app-contact-form>

        <app-section-footer action={false}></app-section-footer>
      </ion-content>,
    ];
  }
}
