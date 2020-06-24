import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-enterprise',
  styleUrl: 'app-enterprise.scss',
})
export class AppEnterprise {
  private featuresRef!: HTMLElement;

  private scrollToFeatures() {
    if (!this.featuresRef) {
      return;
    }

    this.featuresRef.scrollIntoView({
      behavior: 'smooth',
    });
  }

  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content>
        <main>
          <section class="hero">
            <div>
              <h1>The unbreakable slides for your corporate communication.</h1>
              <h3 style={{'font-weight': '300'}}>Distribute and keep up-to-date your corporate identity and design across all your company's presentations.</h3>

              <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="tertiary">
                <ion-label style={{'text-transform': 'none'}}>Get in touch</ion-label>
              </ion-button>
            </div>

            <img class="wave" src={`/assets/img/landing/wave-start.svg`} role="presentation" />
          </section>

          <section class="intro">
            <blockquote>
              <span>❝</span>
              <p>Did you had a look to Bob's last presentation? He used our old logo and made it green, again.</p>
            </blockquote>

            <h2>Templates and options that match your CI/CD.</h2>

            <div>
              <p>
                Unlike any other editor for presentations, DeckDeckGo assists you in applying your corporate identity to your company's presentations. The
                templates and set of options you are defining are those your collaborators are using.
              </p>
            </div>

            <div>
              <p>Together with your designers, we create unbreakable slides.</p>

              <p>Moreover, rolling out new design and logo has never been so easy and DeckDeckGo can offer so much more.</p>

              <button type="button" class="app-button" onClick={() => this.scrollToFeatures()}>
                Discover more features <ion-icon src="/assets/icons/ionicons/arrow-forward.svg"></ion-icon>
              </button>
            </div>
          </section>

          <img class="separator intro" src={`/assets/img/landing/wave-introducing.svg`} role="presentation" />

          <app-features ref={(el) => (this.featuresRef = el as HTMLElement)}></app-features>
        </main>
      </ion-content>,
    ];
  }
}
