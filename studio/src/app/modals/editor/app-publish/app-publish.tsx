import {Component, Element, Listen, Prop, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';

import {AuthService} from '../../../services/auth/auth.service';

@Component({
    tag: 'app-publish',
    styleUrl: 'app-publish.scss'
})
export class AppPublish {

    @Element() el: HTMLElement;

    private authService: AuthService;

    @Prop()
    caption: string;

    @Prop()
    description: string;

    @State()
    private author: string;

    @State()
    private today: Date = new Date();

    constructor() {
        this.authService = AuthService.getInstance();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
            this.author = authUser ? authUser.name : '';
        });
    }

    @Listen('window:popstate')
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Ready to publish?</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <p>Edit the title and summary of your presentation and add or change tags (up to 5) to make your presentation more inviting to readers</p>

                <app-feed-card compact={false} miniature={false} editable={true} author={this.author} publication={this.today} caption={this.caption} description={this.description}></app-feed-card>

                <div class="ion-padding ion-text-center">
                    <ion-button shape="round" color="primary">
                        <ion-label class="ion-text-uppercase">Publish now</ion-label>
                    </ion-button>
                </div>
            </ion-content>
        ];
    }


}
