import {Component} from '@stencil/core';

@Component({
    tag: 'app-feed-card',
    styleUrl: 'app-feed-card.scss',
    shadow: false
})
export class AppFeedCard {

    render() {

        return <ion-card>
            <app-feed-card-content></app-feed-card-content>
        </ion-card>
    }

}
