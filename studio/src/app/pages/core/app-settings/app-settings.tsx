import {Component, Listen, State, h} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';
import {filter, take} from 'rxjs/operators';

import firebase from '@firebase/app';
import '@firebase/auth';

import {ApiUser} from '../../../models/api/api.user';
import {AuthUser} from '../../../models/auth/auth.user';

import {UserUtils} from '../../../utils/core/user-utils';
import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {ApiUserService} from '../../../services/api/user/api.user.service';
import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {ErrorService} from '../../../services/core/error/error.service';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import {UserService} from '../../../services/data/user/user.service';

@Component({
    tag: 'app-settings',
    styleUrl: 'app-settings.scss'
})
export class AppHome {

    @State()
    private authUser: AuthUser;

    @State()
    private apiUser: ApiUser;

    @State()
    private valid: boolean = true;

    private authService: AuthService;
    private apiUserService: ApiUserService;
    private userService: UserService;

    private navService: NavService;

    private errorService: ErrorService;

    private imageHistoryService: ImageHistoryService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.apiUserService = ApiUserService.getInstance();
        this.navService = NavService.getInstance();
        this.errorService = ErrorService.getInstance();
        this.imageHistoryService = ImageHistoryService.getInstance();
        this.userService = UserService.getInstance();
    }

    componentWillLoad() {
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined && !authUser.anonymous),
            take(1)).subscribe(async (authUser: AuthUser) => {
            this.authUser = authUser;
        });

        this.apiUserService.watch().pipe(
            filter((apiUser: ApiUser) => apiUser !== null && apiUser !== undefined && !apiUser.anonymous),
            take(1)).subscribe(async (apiUser: ApiUser) => {
            this.apiUser = apiUser;
        });
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    @Listen('keydown')
    async handleEnterKey($event: KeyboardEvent) {
        if ($event && $event.key === 'Enter') {
            await this.save();
        }
    }

    private async handleSubmit(e: Event) {
        e.preventDefault();

        await this.save();
    }

    private handleUsernameInput($event: CustomEvent<KeyboardEvent>) {
        this.apiUser.username = ($event.target as InputTargetEvent).value;
    }

    private validateUsernameInput() {
        this.valid = this.apiUser && UserUtils.validUsername(this.apiUser.username);
    }

    private save(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.valid) {
                resolve();
                return;
            }

            try {
                await this.apiUserService.put(this.apiUser, this.authUser.token, this.apiUser.id);
            } catch (err) {
                this.errorService.error('Your changes couldn\'t be saved');
            }

            resolve();
        });
    }

    private async presentConfirmDelete() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-user-delete',
            componentProps: {
                username: this.apiUser.username
            }
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                await this.deleteUser();
            }
        });

        await modal.present();
    }

    private deleteUser(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                const loading: HTMLIonLoadingElement = await IonControllerUtils.createLoading({});

                await loading.present();

                // TODO: Delete decks and slides?

                await this.apiUserService.delete(this.apiUser.id, this.authUser.token);

                await this.userService.delete(this.authUser.uid);

                const firebaseUser: firebase.User = firebase.auth().currentUser;

                if (firebaseUser) {
                    await firebaseUser.delete();
                }

                await this.imageHistoryService.clear();

                this.navService.navigate({
                    url: '/',
                    direction: NavDirection.ROOT
                });

                await loading.dismiss();

                resolve();
            } catch (err) {
                this.errorService.error('Your user couldn\'t be deleted, contact us per email')
            }
        });
    }

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content class="ion-padding fullscreen-padding">
                <main class="ion-padding">
                    {this.renderGuardedContent()}
                    {this.renderDangerZone()}
                </main>
            </ion-content>
        ];
    }

    private renderGuardedContent() {
        if (!this.authUser) {
            return this.renderNotLoggedInContent();
        } else {
            return this.renderUserContent();
        }
    }

    private renderNotLoggedInContent() {
        return [
            <h1>Oh, hi! Good to have you.</h1>,
            <p class="ion-padding-top">
                <ion-anchor onClick={() => this.signIn()}>Sign in</ion-anchor>
                to access your settings.</p>
        ]
    }

    private renderUserContent() {
        return [
            <h1>Settings</h1>,
            <form onSubmit={(e: Event) => this.handleSubmit(e)}>
                <ion-list class="inputs-list">
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
        if (this.apiUser) {
            return [<ion-item class="item-title">
                <ion-label>Username</ion-label>
            </ion-item>,
                <ion-item>
                    <ion-input value={this.apiUser.username} debounce={500} minlength={3} maxlength={32} required={true}
                               input-mode="text"
                               onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleUsernameInput(e)}
                               onIonChange={() => this.validateUsernameInput()}></ion-input>
                </ion-item>];
        } else {
            return undefined;
        }
    }

    private renderSubmitForm() {
        if (this.apiUser) {
            return <ion-button type="submit" disabled={!this.valid} color="primary" shape="round">
                <ion-label>Submit</ion-label>
            </ion-button>
        } else {
            return undefined;
        }
    }

    private renderDangerZone() {
        if (this.apiUser && this.authUser) {
            return [<h1 class="ion-padding-top ion-margin-top">Danger Zone</h1>,
                <p>Once you delete your user, there is no going back. Please be certain.</p>,
                <ion-button color="danger" shape="round" fill="outline" onClick={() => this.presentConfirmDelete()}>
                    <ion-label>Delete my user</ion-label>
                </ion-button>
            ]
        } else {
            return undefined;
        }
    }
}
