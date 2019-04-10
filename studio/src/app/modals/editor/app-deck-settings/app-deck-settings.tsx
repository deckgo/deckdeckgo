import {Component, Listen, Element, State} from '@stencil/core';

import {Deck} from '../../../models/deck';

import {DeckService} from '../../../services/deck/deck.service';
import {DeckEditorService} from '../../../services/deck/deck-editor.service';
import {ErrorService} from '../../../services/error/error.service';
import {take} from 'rxjs/operators';

@Component({
    tag: 'app-deck-settings',
    styleUrl: 'app-deck-settings.scss'
})
export class AppDeckSettings {

    @Element() el: HTMLElement;

    @State()
    private presentationName: string;

    private deckEditorService: DeckEditorService;
    private deckService: DeckService;

    private errorService: ErrorService;

    @State()
    private disableSave: boolean = false;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
        this.deckService = DeckService.getInstance();
        this.errorService = ErrorService.getInstance();
    }

    componentWillLoad() {
        this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
            this.presentationName = deck ? deck.name : null;
        });
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private presentationNameChange($event: CustomEvent) {
        this.presentationName = $event.detail.value;

        this.disableSave = !this.hasPresentationName();
    }

    private hasPresentationName(): boolean {
        return this.presentationName !== undefined && this.presentationName != null && this.presentationName !== '';
    }

    private save(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.disableSave = true;

            if (!this.hasPresentationName() || this.presentationName.length < 5) {
                this.errorService.error('Presentation name should be at least 5 characters and max 40');

                this.disableSave = false;
                resolve();
                return;
            }

            try {
                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck || !deck.id) {
                        this.disableSave = false;
                        resolve();
                        return;
                    }

                    deck.name = this.presentationName;

                    const updatedDeck: Deck = await this.deckService.put(deck);
                    this.deckEditorService.next(updatedDeck);

                    await this.closeModal();
                });
            } catch (err) {
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
                    <ion-title class="ion-text-uppercase">Settings</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <ion-item>
                    <ion-label position="stacked">Presentation name</ion-label>
                    <ion-input value={this.presentationName} color="primary" debounce={100} inputmode="text" minlength={5} maxlength={40} required={true}
                        onIonChange={(e: CustomEvent) => this.presentationNameChange(e)}></ion-input>
                </ion-item>

                <div class="ion-padding ion-text-center">
                    <ion-button shape="round" color="primary" disabled={this.disableSave} onClick={() => {this.save()}}>
                        <ion-label class="ion-text-uppercase">Save</ion-label>
                    </ion-button>
                </div>
            </ion-content>
        ];
    }

}
