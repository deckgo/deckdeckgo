import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-landing-footer',
    styleUrl: 'app-landing-footer.scss',
    shadow: false
})
export class AppLandingFooter {


    render() {
        return <footer>
            <deckgo-lazy-img svg-src={`/assets/img/landing/wave-remote.svg`} aria-label="Section introducing separator"></deckgo-lazy-img>

            <main>
                <section class="ion-padding ion-text-center">
                    <h3>Start now.</h3>

                    <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="primary">
                        <ion-label>Write a presentation</ion-label>
                    </ion-button>
                </section>

                <section class="ion-padding ion-margin-top">
                    <app-footer display="landing"></app-footer>
                </section>
            </main>

        </footer>
    }
}
