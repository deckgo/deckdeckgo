import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-about',
    styleUrl: 'app-about.scss'
})
export class AppAbout {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                <main class="ion-padding">
                    <h1>About DeckDeckGo</h1>

                    <h3 class="ion-padding-top">Edit anywhere, display everywhere</h3>

                    <p class="ion-padding-top">DeckDeckGo aims to be the open source web editor for presentations. It enables anyone with any type of devices (desktop, mobile or tablets) to easily create, present and share presentations for free.</p>

                    <p>What makes it different 🤔? Every presentations published with DeckDeckGo are standalone <strong>Progressive Web Apps</strong> 🚀</p>

                    <p>Moreover, it is also an online community for sharing presentations, slides and talks about your interests and ideas.</p>

                    <p>DeckDeckGo was created with passion in 2019 in Zürich🇨🇭</p>

                </main>

            </ion-content>
        ];
    }

}
