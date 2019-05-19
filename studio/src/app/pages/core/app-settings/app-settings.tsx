import {Component, Listen, State} from '@stencil/core';
import {filter, take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';
import {User} from '../../../models/user';

import {UserService} from '../../../services/api/user/user.service';
import {AuthService} from '../../../services/api/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {ErrorService} from '../../../services/core/error/error.service';

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

    private errorService: ErrorService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.userService= UserService.getInstance();
        this.navService = NavService.getInstance();
        this.errorService = ErrorService.getInstance();
    }

    componentWillLoad() {
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined && !authUser.anonymous),
            take(1)).subscribe(async (authUser: AuthUser) => {
                this.authUser = authUser;
        });

        this.userService.watch().pipe(
            filter((user: User) => user !== null && user !== undefined && !user.anonymous),
            take(1)).subscribe(async (user: User) => {
                this.user = user;
        });
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    @Listen('keydown.enter')
    async handleEnterKey() {
        await this.save();
    }

    private async handleSubmit(e: Event) {
        e.preventDefault();

        await this.save();
    }

    private handleUsernameInput($event: CustomEvent<KeyboardEvent>) {
        this.user.username = ($event.target as InputTargetEvent).value;
    }

    private validateUsernameInput() {
        this.valid = this.user && this.user.username && this.user.username !== null && this.user.username !== undefined && this.user.username.length >= 3 && this.user.username.length <= 32;
    }

    private save(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.valid) {
                resolve();
                return;
            }

            try {
                // TODO: Doesn't work yet, end up with a 405
                await this.userService.query(this.user, this.authUser.token, 'PUT');
            } catch (err) {
                this.errorService.error('Your changes couldn\'t be saved');
            }

            resolve();
        });
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
                    {this.renderName()}
                    {this.renderUsername()}
                </ion-list>

                {this.renderSubmitForm()}
            </form>
        ]
    }

    private renderName() {
        if (this.authUser) {
            return [<ion-item class="item-title">
                <ion-label>Name</ion-label>
            </ion-item>,
                <ion-item>
                    <ion-input value={this.authUser.name} required={true} input-mode="text" disabled={true}></ion-input>
                </ion-item>];
        } else {
            return undefined;
        }
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
