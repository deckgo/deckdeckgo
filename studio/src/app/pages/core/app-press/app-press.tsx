import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-press',
    styleUrl: 'app-press.scss'
})
export class AppPress {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                <main class="ion-padding">
                    <h1>Press</h1>

                    <p>We don't provide any press kit yet but, as we are open source and open to any contributions, we would be happy to get your help to create such material 😄</p>

                    <p>That being said, you could download our logo as a <a href="https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg" target="_blank">svg</a> and could <ion-router-link href="/contact" routerDirection="forward">contact</ion-router-link> us for any inquiries.</p>
                </main>
            </ion-content>
        ];
    }

}

