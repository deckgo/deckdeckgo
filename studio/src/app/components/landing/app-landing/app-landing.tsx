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
                <deckgo-deck embedded={true}>
                    <deckgo-slide-title style={{'--background': 'var(--ion-color-primary)', '--color': 'white'}}>
                        <h1 slot="title">Make more than presentations</h1>
                        <div slot="content" style={{'margin-bottom': '48px'}}>
                            <h3>Create, present and share decks. Interact with your audience.</h3>

                            <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="light">
                                <ion-label style={{'text-transform': 'none'}}>Get started with DeckDeckGo</ion-label>
                            </ion-button>
                        </div>
                        <div slot="background">
                            <img src="/assets/img/landing/wave-start.svg"/>
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
                        <div slot="background">
                            <img src="/assets/img/landing/wave-end.svg"/>
                        </div>
                    </deckgo-slide-title>
                </deckgo-deck>
            </section>

            <section>
                hello
            </section>
        </Host>
    }

}
