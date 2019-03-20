import {Component, State} from '@stencil/core';
import {AuthService} from '../../../services/auth/auth.service';
import {Subscription} from 'rxjs';
import {User} from '../../../models/user';


@Component({
    tag: 'app-menu-user',
    styleUrl: 'app-menu-user.scss',
    shadow: false
})
export class AppMenuUser {

    private authService: AuthService;
    private subscription: Subscription;

    @State()
    private user: User;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.authService.watch().subscribe((user: User) => {
            this.user = user;
        });
    }

    componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    render() {
        return <ion-list>

            {this.renderUser()}

            <ion-item-divider>
                <ion-label>Presentations</ion-label>
                <ion-button size="small" slot="end" shape="round" margin-end href="/editor" routerDirection="forward">
                    <ion-icon name="book" slot="start"></ion-icon>
                    <ion-label>New</ion-label>
                </ion-button>
            </ion-item-divider>

            <ion-item href="/editor" routerDirection="forward">
                <ion-icon name="book" slot="start"></ion-icon>
                <ion-label>Presentation A</ion-label>
            </ion-item>

            <ion-item href="/editor" routerDirection="forward">
                <ion-icon name="book" slot="start"></ion-icon>
                <ion-label>Presentation B</ion-label>
            </ion-item>
        </ion-list>;
    }

    private renderUser() {
        if (this.user) {
            return <ion-item class="user">
                <app-avatar slot="start" src={this.user.photo_url}></app-avatar>
                <ion-label>{this.user.name}</ion-label>
            </ion-item>;
        } else {
            return <ion-item class="user"></ion-item>;
        }
    }

}
