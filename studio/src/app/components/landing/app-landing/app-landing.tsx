import {Component, Element, h, Host, State} from '@stencil/core';

import {isIOS} from '@deckdeckgo/utils';

@Component({
    tag: 'app-landing',
    styleUrl: 'app-landing.scss',
    shadow: false
})
export class AppLanding {

    @Element() el: HTMLElement;

    @State()
    private iOS: boolean = isIOS();

    render() {
        return <Host>
            <section class={this.iOS ? 'header ios' : 'header'}>
                <div class="deck">
                    <deckgo-deck embedded={true}>
                        <deckgo-slide-title style={{'--background': 'var(--ion-color-primary)', '--color': 'white'}}>
                            <h1 slot="title">Make more than presentations</h1>
                            <div slot="content" style={{'margin-bottom': '48px'}}>
                                <h3>Create, present and share decks. Interact with your audience.</h3>

                                <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="light">
                                    <ion-label style={{'text-transform': 'none'}}>Get started with DeckDeckGo</ion-label>
                                </ion-button>
                            </div>
                        </deckgo-slide-title>

                        <deckgo-slide-title>
                            <h2 slot="title">Edit your slides anywhere. Display them everywhere.</h2>
                            <div slot="content" style={{'display': 'flex', 'justify-content': 'center', 'align-items': 'center', 'max-width': '100%'}}>
                                <ion-icon class="ion-padding" src="/assets/img/landing/mobile-light.svg" style={{'font-size': '4rem'}}></ion-icon>
                                <ion-icon class="ion-padding" src="/assets/img/landing/tablet-light.svg" style={{'font-size': '6rem'}}></ion-icon>
                                <ion-icon class="ion-padding" src="/assets/img/landing/desktop-light.svg" style={{'font-size': '6.6rem'}}></ion-icon>
                                <ion-icon class="ion-padding" src="/assets/img/landing/projector.svg" style={{'font-size': '6.6rem'}}></ion-icon>
                            </div>
                        </deckgo-slide-title>
                    </deckgo-deck>
                </div>

                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1440 320">
                    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1440 320">
                        <path fill="#ffffff" fill-opacity="1" d="M0,224L80,229.3C160,235,320,245,480,224C640,203,800,149,960,149.3C1120,149,1280,203,1360,229.3L1440,256L1440,320L1360,320C1280,320,1120,320,960,320C800,320,640,320,480,320C320,320,160,320,80,320L0,320Z"></path>
                    </svg>
                </svg>
            </section>

            <section>
                hello
            </section>
        </Host>
    }

}
