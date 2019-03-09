import {Component} from '@stencil/core';

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content padding>

                <main>

                    Hello World

                </main>

            </ion-content>
        ];
    }
}
