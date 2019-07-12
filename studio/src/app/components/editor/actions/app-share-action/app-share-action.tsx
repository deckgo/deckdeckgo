import {Component, Element, Event, EventEmitter, h} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {take} from 'rxjs/operators';

import {Deck} from '../../../../models/data/deck';

import {ShareOption} from '../../../../popovers/editor/app-share-options/share-option';

import {IonControllerUtils} from '../../../../utils/core/ion-controller-utils';

import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-share-action'
})
export class AppShareAction {

    @Element() el: HTMLElement;

    private deckEditorService: DeckEditorService;

    @Event() private actionPublish: EventEmitter<void>;

    @Event() private openShare: EventEmitter<void>;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
    }

    private share($event: UIEvent): Promise<void> {
        return new Promise<void>((resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                const deckPublished: boolean = deck && deck.data && deck.data.meta && deck.data.meta.published;

                if (deckPublished) {
                    await this.openShareOptions($event);
                } else {
                    this.actionPublish.emit();
                }

                resolve();
            });
        });
    }

    async openShareOptions($event: UIEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-share-options',
            event: $event,
            mode: 'ios'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                if (detail.data.action === ShareOption.SHARE) {
                    await this.openShare.emit();
                } else if (detail.data.action === ShareOption.PUBLISH) {
                    this.actionPublish.emit();
                }
            }
        });

        await popover.present();
    }

    render() {
        return <ion-tab-button onClick={($event: UIEvent) => this.share($event)} color="primary" mode="md">
            <ion-icon name="share"></ion-icon>
            <ion-label>Share</ion-label>
        </ion-tab-button>
    }
}
