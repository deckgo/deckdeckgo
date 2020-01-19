import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-discover'
})
export class AppDiscover {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">
                <app-feed></app-feed>
            </ion-content>
        ];
    }

}
