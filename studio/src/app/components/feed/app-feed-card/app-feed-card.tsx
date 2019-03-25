import {Component} from '@stencil/core';

@Component({
    tag: 'app-feed-card',
    styleUrl: 'app-feed-card.scss',
    shadow: false
})
export class AppFeedCard {

    render() {
        return <ion-card>
            <ion-card-content>
                <div class="summary">

                    <ion-card-header>
                        <ion-card-title class="ion-text-uppercase">Card Title</ion-card-title>

                        <ion-card-subtitle class="ion-text-lowercase">
                            <div class="chips"><ion-label>Javascript&nbsp;</ion-label></div>
                            <div class="chips"><ion-label>Typescript&nbsp;</ion-label></div>
                            <div class="chips"><ion-label>Ionic&nbsp;</ion-label></div>
                        </ion-card-subtitle>
                    </ion-card-header>

                    <p padding-start padding-end class="content ion-text-lowercase">Keep close to Nature's heart... and break clear away, once in
                        awhile,
                        and climb a mountain or spend a week in the woods. Wash your spirit clean.
                    </p>

                    <p class="author" padding>
                        <ion-label>David Dal Busco | Mars 9</ion-label>
                    </p>
                </div>
                <div class="preview">
                    <img src="./assets/img/deckdeckgo-logo.svg"/>
                </div>
            </ion-card-content>
        </ion-card>
    }

}
