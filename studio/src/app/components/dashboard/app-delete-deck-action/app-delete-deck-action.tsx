import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';
import {loadingController, modalController, OverlayEventDetail} from '@ionic/core';

import {Deck} from '../../../models/data/deck';

import {DeckService} from '../../../services/data/deck/deck.service';

@Component({
    tag: 'app-delete-deck-action',
    styleUrl: 'app-delete-deck-action.scss',
    shadow: true
})
export class AppDeleteDeckAction {

    @Prop() deck: Deck;

    private deckService: DeckService;

    @Event() deckDeleted: EventEmitter<string>;

    constructor() {
        this.deckService = DeckService.getInstance();
    }

    private async presentConfirmDelete($event: UIEvent) {
        $event.stopPropagation();

        if (!this.deck || !this.deck.data) {
            return;
        }

        const modal: HTMLIonModalElement = await modalController.create({
            component: 'app-deck-delete',
            componentProps: {
                deckName: this.deck.data.name,
                published: this.deck.data.meta && this.deck.data.meta.published
            }
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                await this.deleteDeck();
            }
        });

        await modal.present();
    }

    private deleteDeck(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.deck || !this.deck.id || this.deck.id === undefined || this.deck.id === '') {
                resolve();
                return;
            }

            const loading: HTMLIonLoadingElement = await loadingController.create({});

            await loading.present();

            await this.deckService.delete(this.deck.id);

            this.deckDeleted.emit(this.deck.id);

            await loading.dismiss();

            resolve();
        });
    }

    render() {
        return <a onClick={($event: UIEvent) => this.presentConfirmDelete($event)} title="Delete presentation">
            <ion-icon name="trash"></ion-icon>
        </a>
    }

}
