import {Component} from '@stencil/core';

@Component({
    tag: 'app-about',
    styleUrl: 'app-about.scss'
})
export class AppAbout {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content padding>

                <main>
                    Hello
                </main>

            </ion-content>
        ];
    }

}
