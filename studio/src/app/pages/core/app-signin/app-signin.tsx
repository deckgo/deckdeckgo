import {Component, Element, Prop, State, Watch, h} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {forkJoin} from 'rxjs';
import {filter, take} from 'rxjs/operators';

import {del, get, set} from 'idb-keyval';

import {AuthUser} from '../../../models/auth/auth.user';
import {Deck} from '../../../models/data/deck';

import {Utils} from '../../../utils/core/utils';
import {EnvironmentDeckDeckGoConfig} from '../../../services/core/environment/environment-config';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {AuthService} from '../../../services/auth/auth.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {UserService} from '../../../services/data/user/user.service';
import {DeckService} from '../../../services/data/deck/deck.service';

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

    @State()
    private signInInProgress: boolean = false;

    private navService: NavService;

    private userService: UserService;
    private authService: AuthService;
    private deckService: DeckService;

    private firebaseUser: firebase.User;

    private deckEditorService: DeckEditorService;

    constructor() {
        this.navService = NavService.getInstance();
        this.deckService = DeckService.getInstance();
        this.userService = UserService.getInstance();
        this.authService = AuthService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
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
        this.signInInProgress = false;

        await Utils.injectJS(
            'firebase-ui-script',
            'https://cdn.firebase.com/libs/firebaseui/4.0.0/firebaseui.js'
        );
        await Utils.injectCSS(
            'firebase-ui-css',
            'https://cdn.firebase.com/libs/firebaseui/4.0.0/firebaseui.css'
        );

        const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        const appUrl: string = deckDeckGoConfig.appUrl;

        const redirectUrl: string = await get<string>('deckdeckgo_redirect');
        const mergeInfo: MergeInformation = await get<MergeInformation>('deckdeckgo_redirect_info');

        const signInOptions = [];

        signInOptions.push(firebase.auth.GoogleAuthProvider.PROVIDER_ID);
        signInOptions.push(firebase.auth.GithubAuthProvider.PROVIDER_ID);
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
                signInSuccessWithAuthResult: (_authResult, _redirectUrl) => {
                    this.signInInProgress = true;

                    // HACK: so signInSuccessWithAuthResult doesn't like promises, so we save the navigation information before and run the redirect not asynchronously
                    // Ultimately I would like to transfer here the userService.updateMergedUser if async would be supported
                    this.navigateRoot(redirectUrl, mergeInfo);

                    return false;
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

            this.signInInProgress = true;

            // The credential the user tried to sign in with.
            const cred = error.credential;

            const mergeInfo: MergeInformation = await get<MergeInformation>('deckdeckgo_redirect_info');

            if (!mergeInfo || !mergeInfo.deckId || !mergeInfo.userToken || !mergeInfo.userId) {
                // Should not happens but at least  don't get stuck
                await firebase.auth().signInWithCredential(cred);

                await this.navigateRedirect();

                resolve();
                return;
            }

            this.authService.watch().pipe(
                filter((authUser: AuthUser) => authUser !== null && authUser !== undefined && authUser.uid && authUser.uid !== mergeInfo.userId),
                take(1)).subscribe(async (authUser: AuthUser) => {

                // Merge deck to new user
                await this.deckService.mergeDeck(mergeInfo.deckId, authUser.uid);

                // Delete previous anonymous user from the database
                await this.userService.delete(mergeInfo.userId);

                // Delete previous anonymous user from Firebase
                if (this.firebaseUser) {
                    await this.firebaseUser.delete();
                }

                await this.navigateRedirect();

                resolve();
            });

            await firebase.auth().signInWithCredential(cred);

            resolve();
        });
    };

    private saveRedirect(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const mergeInfo: MergeInformation = await get<MergeInformation>('deckdeckgo_redirect_info');

            if (mergeInfo && mergeInfo.userId && mergeInfo.userToken) {
                resolve();
                return;
            }

            await set('deckdeckgo_redirect', this.redirect ? this.redirect : '/');

            const observables = [];
            observables.push(this.authService.watch().pipe(take(1)));
            observables.push(this.deckEditorService.watch().pipe(take(1)));

            forkJoin(observables).subscribe(async ([authUser, deck]: [AuthUser, Deck]) => {
                await set('deckdeckgo_redirect_info', {
                    deckId: deck ? deck.id : null,
                    userId: authUser ? authUser.uid : null,
                    userToken: authUser ? authUser.token : null,
                    anonymous: authUser ? authUser.anonymous : true
                });

                resolve();
            });
        })
    }

    private async navigateRedirect() {
        const redirectUrl: string = await get<string>('deckdeckgo_redirect');
        const mergeInfo: MergeInformation = await get<MergeInformation>('deckdeckgo_redirect_info');

        await del('deckdeckgo_redirect');
        await del('deckdeckgo_redirect_info');

        this.navigateRoot(redirectUrl, mergeInfo);
    }

    private navigateRoot(redirectUrl: string, mergeInfo: MergeInformation) {
        // TODO: That's ugly
        const url: string = !redirectUrl || redirectUrl.trim() === '' || redirectUrl.trim() === '/' ? '/' : '/' + redirectUrl + (!mergeInfo || !mergeInfo.deckId || mergeInfo.deckId.trim() === '' || mergeInfo.deckId.trim() === '/' ? '' : '/' + mergeInfo.deckId);

        // Do not push a new page but reload as we might later face a DOM with contains two firebaseui which would not work
        this.navService.navigate({
            url: url,
            direction: NavDirection.ROOT
        });
    }

    async navigateBack() {
        this.navService.navigate({
            direction: NavDirection.BACK
        });
    }

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content class="ion-padding fullscreen-padding">
                <main class="ion-padding">
                    {this.renderBackButton()}

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
                <p class="ion-text-center ion-padding">Sign in to unleash all features of the editor like adding more slides, uploading and using your own images, using the author template or being able to share your presentation as an app.</p>
            ]
        } else {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Welcome back.</h1>,
                <p class="ion-text-center ion-padding">Sign in to unleash all features of the editor and to be able to share your presentation as an app.</p>
            ]
        }
    }

    private renderBackButton() {
        if (this.signInInProgress) {
            return undefined;
        } else {
            return <ion-buttons class="back">
                <ion-button onClick={() => this.navigateBack()} color="primary">
                    <ion-icon name="close"></ion-icon>
                </ion-button>
            </ion-buttons>;
        }
    }
}
