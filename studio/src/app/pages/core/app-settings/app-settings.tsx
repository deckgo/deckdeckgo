import {Component, Listen, State} from '@stencil/core';
import {filter, take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';
import {User} from '../../../models/user';

import {UserService} from '../../../services/api/user/user.service';
import {AuthService} from '../../../services/api/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';

@Component({
    tag: 'app-settings',
    styleUrl: 'app-settings.scss'
})
export class AppHome {

    @State()
    private authUser: AuthUser;

    @State()
    private user: User;

    @State()
    private valid: boolean = true;

    private authService: AuthService;
    private userService: UserService;

    private navService: NavService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.userService= UserService.getInstance();
        this.navService = NavService.getInstance();
    }

    componentWillLoad() {
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined),
            take(1)).subscribe(async (authUser: AuthUser) => {
                this.authUser = authUser;
        });

        this.userService.watch().pipe(
            filter((user: User) => user !== null && user !== undefined),
            take(1)).subscribe(async (user: User) => {
                this.user = user;
        });
    }

    @Listen('keydown.enter')
    handleEnterKey() {
        // TODO submit
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    private handleSubmit(e: Event) {
        e.preventDefault();

        console.log(e);
        // send data to our backend
    }

    private handleUsernameInput($event: CustomEvent<KeyboardEvent>) {
        this.user.username = ($event.target as InputTargetEvent).value;
    }

    private validateUsernameInput() {
        this.valid = this.user && this.user.username && this.user.username !== null && this.user.username !== undefined && this.user.username.length >= 3 && this.user.username.length <= 32;
    }

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content class="ion-padding fullscreen-padding">
                <main padding>
                    {this.renderGuardedContent()}
                </main>
            </ion-content>
        ];
    }

    private renderGuardedContent() {
        if (!this.authUser) {
            return this.renderNotLoggedInContent();
        } else{
            return this.renderUserContent();
        }
    }

    private renderNotLoggedInContent() {
        return [
            <h1>Oh, hi! Good to have you.</h1>,
            <p class="ion-padding-top"><ion-anchor onClick={() => this.signIn()}>Sign in</ion-anchor> to access your settings.</p>
        ]
    }

    private renderUserContent() {
        return [
            <h1>Settings</h1>,
            <form onSubmit={(e: Event) => this.handleSubmit(e)}>
                <ion-list>
                    {this.renderUsername()}
                </ion-list>

                {this.renderSubmitForm()}
            </form>
        ]
    }

    private renderUsername() {
        if (this.user) {
            return [<ion-item class="item-title">
                <ion-label>Username</ion-label>
                </ion-item>,
                <ion-item>
                    <ion-input value={this.user.username} debounce={500} minlength={3} maxlength={32} required={true} input-mode="text"
                           onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleUsernameInput(e)}
                           onIonChange={() => this.validateUsernameInput()}></ion-input>
            </ion-item>];
        } else {
            return undefined;
        }
    }

    private renderSubmitForm() {
        if (this.user) {
            return <ion-button type="submit" disabled={!this.valid} color="primary" shape="round" class="ion-margin-top">
                <ion-label>Submit</ion-label>
            </ion-button>
        } else {
            return undefined;
        }
    }
}
