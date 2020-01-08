import {Component, Element, h, Host} from '@stencil/core';

@Component({
    tag: 'app-landing',
    styleUrl: 'app-landing.scss',
    shadow: false
})
export class AppLanding {

    @Element() el: HTMLElement;

    render() {

        return <Host>
            <section class="header">
                <app-landing-deck></app-landing-deck>
            </section>

            <app-landing-content></app-landing-content>

            <footer>
                <deckgo-lazy-img svg-src={`/assets/img/landing/wave-remote.svg`} aria-label="Section introducing separator"></deckgo-lazy-img>

                <main>
                    <section class="ion-padding ion-text-center">
                        <h3>Start now.</h3>

                        <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="primary">
                            <ion-label>Write a presentation</ion-label>
                        </ion-button>
                    </section>
                </main>


            </footer>
        </Host>
    }
}
