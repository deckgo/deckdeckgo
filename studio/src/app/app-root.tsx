import {Component, Prop} from '@stencil/core';

@Component({
    tag: 'app-root',
    styleUrl: 'app-root.scss'
})
export class AppRoot {

    @Prop({connect: 'ion-menu-controller'}) lazyMenuController!: HTMLIonMenuControllerElement;

    render() {
        return ([
            <ion-app>
                <ion-router useHash={false}>
                    <ion-route url="/" component="app-home"/>
                </ion-router>

                <ion-split-pane when="lg">
                    <ion-menu side="start" type="push" swipeGesture={false}>
                        <app-navigation logo={true} menuToggle={false}></app-navigation>
                        <ion-content>
                            <ion-menu-toggle autoHide={false}>
                                <ion-list>
                                    <ion-item href="/" routerDirection="forward">
                                        <ion-label>Home</ion-label>
                                    </ion-item>
                                </ion-list>
                            </ion-menu-toggle>
                        </ion-content>
                    </ion-menu>

                    <ion-nav main/>
                </ion-split-pane>
            </ion-app>,
            <ion-menu-controller></ion-menu-controller>
        ]);
    }
}
