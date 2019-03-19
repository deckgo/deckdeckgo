import {Component} from '@stencil/core';

@Component({
    tag: 'app-popular',
    styleUrl: 'app-popular.scss',
    shadow: false
})
export class AppPopular {

    render() {
        return [
            <h1 padding>Popular</h1>,
            <ion-card>
                <ion-item href="#" class="activated">
                    <ion-icon name="wifi" slot="start"></ion-icon>
                    <ion-label>Card Link Item 1 .activated</ion-label>
                </ion-item>

                <ion-item href="#">
                    <ion-icon name="wine" slot="start"></ion-icon>
                    <ion-label>Card Link Item 2</ion-label>
                </ion-item>

                <ion-item class="activated">
                    <ion-icon name="warning" slot="start"></ion-icon>
                    <ion-label>Card Button Item 1 .activated</ion-label>
                </ion-item>

                <ion-item>
                    <ion-icon name="walk" slot="start"></ion-icon>
                    <ion-label>Card Button Item 2</ion-label>
                </ion-item>
            </ion-card>

        ];
    }

}
