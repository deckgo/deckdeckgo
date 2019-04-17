import {Component} from '@stencil/core';

@Component({
    tag: 'app-about',
    styleUrl: 'app-about.scss'
})
export class AppAbout {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                <main padding>
                    <h1>About DeckDeckGo</h1>

                    <ion-grid>
                        <ion-row>
                            <ion-col size="12" size-md="6">
                                <h3 padding-top>Edit anywhere, display everywhere</h3>

                                <p padding-top>DeckDeckGo aims to be the open source editor for PWA presentations.</p>

                                <p>What does that mean 🤔? It means that every presentations you write and publish with DeckDeckGo are <strong>apps</strong> too 🤪</p>

                                <p>It is also an online community for sharing presentations, slides and talks about your interests and ideas.</p>

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
