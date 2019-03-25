import {Component} from '@stencil/core';

@Component({
    tag: 'app-feed',
    styleUrl: 'app-feed.scss',
    shadow: false
})
export class AppFeed {

    render() {
        return [
            <ion-card>

                <div class="deck-container">
                    <deckgo-deck embedded={true} pager={false}>
                        <deckgo-slide-title>
                            <h1 slot="title">DeckDeckGo</h1>
                            <div no-shadow slot="content ion-text-lowercase">
                                <h3>The Progressive Web App alternative for simple presentations 🚀</h3>
                            </div>
                        </deckgo-slide-title>

                        <deckgo-slide-split>
                            <h1 slot="title">Progressive Web App</h1>
                            <h3 slot="start">Publish your presentation as a</h3>
                            <div slot="end"><strong>Progressive Web App</strong></div>
                        </deckgo-slide-split>

                        <deckgo-slide-youtube src="https://www.youtube.com/watch?v=3o3oGBTTRSs">
                            <h2 slot="title">Remote Control</h2>
                        </deckgo-slide-youtube>
                    </deckgo-deck>
                </div>

                <hr class="deck-divider" margin></hr>

                <ion-card-content class="first-card" margin-start margin-end>

                    <ion-card-header>
                        <ion-card-title class="ion-text-uppercase">Card Title</ion-card-title>
                        <ion-card-subtitle class="ion-text-lowercase">
                            <div class="chips"><ion-label>Javascript&nbsp;</ion-label></div>
                            <div class="chips"><ion-label>Typescript&nbsp;</ion-label></div>
                            <div class="chips"><ion-label>Ionic&nbsp;</ion-label></div>
                        </ion-card-subtitle>
                    </ion-card-header>

                    <p class="content ion-text-lowercase">Keep close to Nature's heart... and break clear away, once in
                        awhile,
                        and climb a mountain or spend a week in the woods. Wash your spirit clean.
                    </p>

                    <p class="author" padding-top padding-bottom>
                        <ion-label>David Dal Busco | Mars 9</ion-label>
                    </p>
                </ion-card-content>
            </ion-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
        ];
    }
}
