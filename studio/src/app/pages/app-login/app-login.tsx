import {Component, Element, Prop, Watch} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {Utils} from '../../utils/utils';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {NavService} from '../../services/nav/nav.service';

@Component({
    tag: 'app-login',
    styleUrl: 'app-login.scss'
})
export class AppLogin {

    @Element() el: HTMLElement;

    @Prop()
    redirect: string;

    private navService: NavService;

    constructor() {
        this.navService = NavService.getInstance();
    }

    async componentDidLoad() {
        await this.setupFirebaseUI();
    }

    @Watch('redirect')
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
        let redirectUrl: string = appUrl;

        if (this.redirect) {
            redirectUrl += '/' + this.redirect;
        }

        const signInOptions = [];

        signInOptions.push(firebase.auth.GoogleAuthProvider.PROVIDER_ID);
        signInOptions.push(firebase.auth.EmailAuthProvider.PROVIDER_ID);

        const uiConfig = {
            signInFlow: 'redirect',
            signInSuccessUrl: redirectUrl,
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
                // signInFailure callback must be provided to handle merge conflicts which
                // occur when an existing credential is linked to an anonymous user.
                signInFailure: this.onSignInFailure
            }
        };

        window['firebase'] = firebase;

        // Initialize the FirebaseUI Widget using Firebase.
        const ui = firebaseui.auth.AuthUI.getInstance() || new firebaseui.auth.AuthUI(firebase.auth());

        if (firebaseui.auth.AuthUI.getInstance()) {
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
            // Copy data from anonymous user to permanent user and delete anonymous
            // user.
            // ...
            // Finish sign-in after data is copied.

            // TODO: What to do, copy or not? merge or not merge?

            await firebase.auth().signInAndRetrieveDataWithCredential(cred);

            await this.navService.navigate(this.redirect ? '/' + this.redirect : '/');

            resolve();
        });
    };

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content padding>
                <main padding>
                    {this.renderMsg()}

                    <div id="firebaseui-auth-container"></div>

                    <p class="ion-text-center ion-padding-start ion-padding-end"><small>DeckDeckGo is free and open source 🖖</small></p>
                </main>
            </ion-content>
        ];
    }

    private renderMsg() {
        if (this.redirect === 'editor') {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Good to have you.</h1>,
                <p class="ion-text-center ion-padding">Sign in to extend your deck, to publish your presentation and to get soon a personalized feed of recommendations.</p>
            ]
        } else {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Welcome back.</h1>,
                <p class="ion-text-center ion-padding">Sign in to publish your presentation and to get soon a personalized feed of recommendations.</p>
            ]
        }
    }
}
