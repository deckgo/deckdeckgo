import {Component, Event, EventEmitter, h, Prop, Host, State} from '@stencil/core';
import {loadingController, modalController, OverlayEventDetail} from '@ionic/core';

import {Deck} from '../../../models/data/deck';

import {DeckService} from '../../../services/data/deck/deck.service';
import {DeckCloneResult, DeckDashboardService} from '../../../services/dashboard/deck/deck-dashboard.service';
import {ErrorService} from '../../../services/core/error/error.service';

@Component({
    tag: 'app-dashboard-deck-actions',
    styleUrl: 'app-dashboard-deck-actions.scss',
    shadow: true
})
export class AppDashboardDeckActions {

    @Prop() deck: Deck;

    private deckService: DeckService;
    private deckDashboardService: DeckDashboardService;

    private errorService: ErrorService;

    @Event() deckDeleted: EventEmitter<string>;
    @Event() deckCloned: EventEmitter<DeckCloneResult>;

    @State()
    private actionInProgress: boolean = false;

    constructor() {
        this.deckService = DeckService.getInstance();
        this.deckDashboardService = DeckDashboardService.getInstance();

        this.errorService = ErrorService.getInstance();
    }

    private async presentConfirmDelete($event: UIEvent) {
        $event.stopPropagation();

        if (this.actionInProgress) {
            return;
        }

        const disabled: boolean = this.deck && this.deck.data && this.deck.data.clone !== undefined;

        if (disabled) {
            return;
        }

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

            this.actionInProgress = true;

            const loading: HTMLIonLoadingElement = await loadingController.create({});

            await loading.present();

            try {
                await this.deckService.delete(this.deck.id);

                this.deckDeleted.emit(this.deck.id);
            } catch (err) {
                this.errorService.error(err);
            }

            await loading.dismiss();

            this.actionInProgress = false;

            resolve();
        });
    }

    private async cloneDeck($event: UIEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            $event.stopPropagation();

            if (this.actionInProgress) {
                resolve();
                return;
            }

            const disabled: boolean = this.deck && this.deck.data && this.deck.data.clone !== undefined;

            if (disabled) {
                resolve();
                return;
            }

            if (!this.deck || !this.deck.id || this.deck.id === undefined || this.deck.id === '') {
                resolve();
                return;
            }

            if (!this.deck.data) {
                resolve();
                return;
            }

            this.actionInProgress = true;

            const loading: HTMLIonLoadingElement = await loadingController.create({});

            await loading.present();

            try {
                const clone: DeckCloneResult = await this.deckDashboardService.clone(this.deck);

                this.deckCloned.emit(clone);
            } catch (err) {
                this.errorService.error(err);
            }

            await loading.dismiss();

            this.actionInProgress = false;

            resolve();
        });
    }

    render() {
        const disabled: boolean = this.deck && this.deck.data && this.deck.data.clone !== undefined;

        return <Host>
            <a onClick={($event: UIEvent) => this.cloneDeck($event)} title="Copy presentation" class={this.actionInProgress || disabled ? 'disabled' : undefined}>
                <ion-icon name="copy"></ion-icon>
            </a>

            <a onClick={($event: UIEvent) => this.presentConfirmDelete($event)} title="Delete presentation" class={this.actionInProgress || disabled ? 'disabled' : undefined}>
                <ion-icon name="trash"></ion-icon>
            </a>
        </Host>
    }

}
