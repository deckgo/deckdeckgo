import { Component } from '@stencil/core';

@Component({
    tag: 'app-tabs'
})
export class AppTabs {

    render() {
        return [
            <ion-tabs>
                <ion-tab tab="tab-home">
                    <ion-nav></ion-nav>
                </ion-tab>
                <ion-tab tab="tab-timer">
                    <ion-nav></ion-nav>
                </ion-tab>
                <ion-tab tab="tab-about">
                    <ion-nav></ion-nav>
                </ion-tab>

                <ion-tab-bar slot="bottom" color="light">
                    <ion-tab-button tab="tab-home">
                        <ion-label>Remote</ion-label>
                        <ion-icon name="tv"></ion-icon>
                    </ion-tab-button>
                    <ion-tab-button tab="tab-timer">
                        <ion-label>Timer</ion-label>
                        <ion-icon name="timer"></ion-icon>
                    </ion-tab-button>
                    <ion-tab-button tab="tab-about">
                        <ion-label>About</ion-label>
                        <ion-icon name="information-circle-outline"></ion-icon>
                    </ion-tab-button>
                </ion-tab-bar>
            </ion-tabs>
        ];
    }

}
