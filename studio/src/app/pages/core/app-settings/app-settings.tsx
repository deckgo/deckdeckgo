import {Component} from '@stencil/core';

@Component({
    tag: 'app-settings',
    styleUrl: 'app-settings.scss'
})
export class AppHome {

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content class="ion-padding fullscreen-padding">
                <main padding>

                    <h1>Hello</h1>


                </main>
            </ion-content>
        ];
    }
}
