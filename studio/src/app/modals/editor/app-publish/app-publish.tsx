import {Component, Element, Listen, Prop, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';


import {Deck} from '../../../models/deck';

import {AuthService} from '../../../services/auth/auth.service';
import {DeckEditorService} from '../../../services/deck/deck-editor.service';
import {DeckService} from '../../../services/deck/deck.service';
import {ErrorService} from '../../../services/error/error.service';

@Component({
    tag: 'app-publish',
    styleUrl: 'app-publish.scss'
})
export class AppPublish {

    @Element() el: HTMLElement;

    private authService: AuthService;

    @Prop()
    description: string;

    @State()
    private caption: string;

    @State()
    private author: string;

    @State()
    private today: Date = new Date();

    @State()
    private disablePublish: boolean = false;

    private deckEditorService: DeckEditorService;
    private deckService: DeckService;

    private errorService: ErrorService;

    constructor() {
        this.authService = AuthService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();
        this.deckService = DeckService.getInstance();

        this.errorService = ErrorService.getInstance();
    }

    async componentWillLoad() {
        this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
            if (deck) {
                this.caption = deck.name;
            }
        });
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

    @Listen('editCaption')
    async onEditCaption(e: CustomEvent) {
        await this.updateDeck(e.detail);
    }

    private updateDeck(title: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!title || title === undefined || title === '') {
                resolve();
                return;
            }

            this.disablePublish = true;

            try {
                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck || !deck.id) {
                        this.disablePublish = false;
                        resolve();
                        return;
                    }

                    deck.name = title;

                    const updatedDeck: Deck = await this.deckService.put(deck);
                    this.deckEditorService.next(updatedDeck);

                    this.disablePublish = false;
                });
            } catch (err) {
                this.disablePublish = false;
                this.errorService.error(err);
            }

            resolve();
        });
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
            <ion-content class="ion-padding">
                <p>Edit the title and summary of your presentation and add or change tags (up to 5) to make your
                    presentation more inviting to readers</p>

                <app-feed-card compact={false} miniature={false} editable={true} author={this.author}
                               publication={this.today} caption={this.caption}
                               description={this.description}></app-feed-card>

                <div class="ion-padding ion-text-center">
                    <ion-button shape="round" color="primary" disabled={this.disablePublish}>
                        <ion-label class="ion-text-uppercase">Publish now</ion-label>
                    </ion-button>
                </div>
            </ion-content>
        ];
    }


}
