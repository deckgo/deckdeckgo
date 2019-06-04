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

                    <ion-grid>
                        <ion-row>
                            <ion-col size="12" size-md="6">
                                <h3 class="padding-top">Edit anywhere, display everywhere</h3>

                                <p class="padding-top">DeckDeckGo aims to be the open source web editor for presentations.</p>

                                <p>What makes it different 🤔? Every presentations published with DeckDeckGo are standalone <strong>Progressive Web Apps</strong> 🚀</p>

                                <p>Moreover, it is also an online community for sharing presentations, slides and talks about your interests and ideas.</p>

                                <p>DeckDeckGo was created with passion in Zürich🇨🇭in 2019</p>
                            </ion-col>

                            <ion-col class="demo">
                                <app-demo></app-demo>
                            </ion-col>
                        </ion-row>
                    </ion-grid>

                </main>

            </ion-content>
        ];
    }

}
