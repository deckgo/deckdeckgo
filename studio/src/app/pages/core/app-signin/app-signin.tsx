import {Component, Element, Prop, Watch} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {filter, take} from 'rxjs/operators';

import {del, get, set} from 'idb-keyval';

import {User} from '../../../models/user';
import {AuthUser} from '../../../models/auth-user';

import {Utils} from '../../../utils/utils';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {NavDirection, NavService} from '../../../services/nav/nav.service';
import {MergeService} from '../../../services/merge/merge.service';
import {UserService} from '../../../services/user/user.service';
import {AuthService} from '../../../services/auth/auth.service';

interface MergeInformation {
    deckId: string;
    userId: string;
    userToken: string;
}

@Component({
    tag: 'app-signin',
    styleUrl: 'app-signin.scss'
})
export class AppSignIn {

    @Element() el: HTMLElement;

    @Prop()
    redirect: string;

    @Prop()
    redirectId: string;

    private navService: NavService;

    private mergeService: MergeService;
    private userService: UserService;
    private authService: AuthService;

    private firebaseUser: firebase.User;

    constructor() {
        this.navService = NavService.getInstance();
        this.mergeService = MergeService.getInstance();
        this.userService = UserService.getInstance();
        this.authService = AuthService.getInstance();
    }

    async componentDidLoad() {
        await this.setupFirebaseUI();
    }

    async componentDidUnload() {
        const ui = firebaseui.auth.AuthUI.getInstance();
        if (ui) {
            await ui.delete();
        }
    }

    @Watch('redirect')
    async watchRedirect() {
        await this.saveRedirect();
    }

    async setupFirebaseUI() {
        await Utils.injectJS(
            'firebase-ui-script',
            'https://cdn.firebase.com/libs/firebaseui/3.5.2/firebaseui.js'
        );
        await Utils.injectCSS(
            'firebase-ui-css',
            'https://cdn.firebase.com/libs/firebaseui/3.5.2/firebaseui.css'
        );

        const appUrl: string = EnvironmentConfigService.getInstance().get('appUrl');

        const signInOptions = [];

        signInOptions.push(firebase.auth.GoogleAuthProvider.PROVIDER_ID);
        signInOptions.push(firebase.auth.EmailAuthProvider.PROVIDER_ID);

        this.firebaseUser = firebase.auth().currentUser;

        const uiConfig = {
            signInFlow: 'redirect',
            signInSuccessUrl: appUrl,
            signInOptions: signInOptions,
            // tosUrl and privacyPolicyUrl accept either url string or a callback
            // function.
            // Terms of service url/callback.
            tosUrl: appUrl + '/terms',
            // Privacy policy url/callback.
            privacyPolicyUrl: appUrl + '/privacy',
            credentialHelper: firebaseui.auth.CredentialHelper.GOOGLE_YOLO,
            autoUpgradeAnonymousUsers: true,
            callbacks: {
                signInSuccessWithAuthResult: async (_authResult, _redirectUrl) => {
                    await this.navigateRedirect();

                    return true;
                },
                // signInFailure callback must be provided to handle merge conflicts which
                // occur when an existing credential is linked to an anonymous user.
                signInFailure: this.onSignInFailure
            }
        };

        window['firebase'] = firebase;

        await this.saveRedirect();

        const ui = firebaseui.auth.AuthUI.getInstance() || new firebaseui.auth.AuthUI(firebase.auth());

        if (!ui.isPendingRedirect()) {
            ui.reset();
        }

        // The start method will wait until the DOM is loaded.
        ui.start('#firebaseui-auth-container', uiConfig);
    }

    onSignInFailure = (error): Promise<void> => {
        return new Promise<void>(async (resolve) => {
            // For merge conflicts, the error.code will be
            // 'firebaseui/anonymous-upgrade-merge-conflict'.
            if (error.code != 'firebaseui/anonymous-upgrade-merge-conflict') {
                resolve();
                return;
            }
            // The credential the user tried to sign in with.
            const cred = error.credential;

            const mergeInfo: MergeInformation = await get('deckdeckgo_redirect_info');

            await this.userService.signOut();

            this.userService.watch().pipe(
                filter((user: User) => user !== null && user !== undefined),
                take(1)).subscribe(async (user: User) => {

                if (user && mergeInfo) {
                    // Merge deck to new user
                    await this.mergeService.mergeDeck(mergeInfo.deckId, mergeInfo.userToken, user.id);

                    // Delete previous anonymous user from our backend
                    await this.userService.delete(mergeInfo.userId, mergeInfo.userToken);

                    // Delete previous anonymous user from Firebase
                    if (this.firebaseUser) {
                        await this.firebaseUser.delete();
                    }

                    if (user.anonymous) {
                        // In case that would happen, if user merge in was created with the anonymous flag, set it to false now
                        user.anonymous = false;

                        this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                            if (authUser) {
                                await this.userService.query(user, authUser.token, 'PUT');
                            }

                            await this.navigateRedirect();

                            resolve();
                        });
                    } else {
                        await this.navigateRedirect();

                        resolve();
                    }
                } else {
                    await this.navigateRedirect();

                    resolve();
                }
            });

            await firebase.auth().signInAndRetrieveDataWithCredential(cred);

            resolve();
        });
    };

    private saveRedirect(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            await set('deckdeckgo_redirect', this.redirect ? this.redirect : '/');

            this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                this.userService.watch().pipe(take(1)).subscribe(async (user: User) => {
                    await set('deckdeckgo_redirect_info', {
                        deckId: this.mergeService.deckId,
                        userId: user.id,
                        userToken: authUser.token
                    });

                    resolve();
                });
            });
        })
    }

    private async navigateRedirect() {
        const redirectUrl: string = await get('deckdeckgo_redirect');
        const mergeInfo: MergeInformation = await get('deckdeckgo_redirect_info');

        await del('deckdeckgo_redirect');
        await del('deckdeckgo_redirect_info');

        // TODO: That's ugly
        const url: string = !redirectUrl || redirectUrl.trim() === '' || redirectUrl.trim() === '/' ? '/' : '/' + redirectUrl + (!mergeInfo || !mergeInfo.deckId || mergeInfo.deckId.trim() === '' || mergeInfo.deckId.trim() === '/' ? '' : '/' + mergeInfo.deckId);

        // Do not push a new page but reload as we might later face a DOM with contains two firebaseui which would not work
        this.navService.navigate({
            url: url,
            direction: NavDirection.ROOT
        });

    }

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content padding>
                <main padding>
                    {this.renderMsg()}

                    <div id="firebaseui-auth-container"></div>

                    <p class="ion-text-center ion-padding-start ion-padding-end">
                        <small>DeckDeckGo is free and open source 🖖</small>
                    </p>
                </main>
            </ion-content>
        ];
    }

    private renderMsg() {
        if (this.redirect === 'editor') {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Good to have you.</h1>,
                <p class="ion-text-center ion-padding">Sign in to extend your deck, to publish your presentation and to
                    get soon a personalized feed of recommendations.</p>
            ]
        } else {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Welcome back.</h1>,
                <p class="ion-text-center ion-padding">Sign in to publish your presentation and to get soon a
                    personalized feed of recommendations.</p>
            ]
        }
    }
}
