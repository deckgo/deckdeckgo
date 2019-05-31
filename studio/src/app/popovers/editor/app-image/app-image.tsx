import {Component, Element, Prop, State} from '@stencil/core';

import {ImageAction} from './image-action';

import {ControllerUtils} from '../../../utils/core/controller-utils';

import {PhotoHistoryService} from '../../../services/editor/photo-history/photo-history.service';

@Component({
        tag: 'app-image',
    styleUrl: 'app-image.scss'
})
export class AppImage {

    @Element() el: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    private applyToAllDeck: boolean = false;

    private photoHistoryService: PhotoHistoryService;

    @State()
    private photosHistoryOdd: UnsplashPhoto[];

    @State()
    private photosHistoryEven: UnsplashPhoto[];

    constructor() {
        this.photoHistoryService = PhotoHistoryService.getInstance();
    }

    async componentWillLoad() {
        await this.initPhotoHistory();
    }

    private initPhotoHistory(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const photosHistory: UnsplashPhoto[] = await this.photoHistoryService.get();

            if (!photosHistory || photosHistory.length <= 0) {
                resolve();
                return;
            }

            this.photosHistoryEven = [...photosHistory.filter((_a, i) => i % 2)];
            this.photosHistoryOdd = [...photosHistory.filter((_a, i) => !(i % 2))];

            resolve();
        });
    }

    private async closePopover(action: ImageAction, photo?: UnsplashPhoto) {
        const data = {
            action: action
        };

        if (this.deckOrSlide) {
            data['applyToAllDeck'] = this.applyToAllDeck;
        }

        if (photo) {
            data['photo'] = photo;
        }

        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss(data);
    }

    private selectPhotoFromHistory($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            await this.closePopover(ImageAction.ADD_PHOTO, $event.detail);

            resolve();
        });
    }

    private selectApplyToAllDeck($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToAllDeck = $event.detail.value;
        }
    }

    private async presentHistoryInfo() {
        const alert: HTMLIonAlertElement = await ControllerUtils.createAlert({
            message: 'The editor keeps track of the last 10 images you would have use in any of your presentations.<br/><br/>Select one to add it again quickly.',
            buttons: ['Ok']
        });

        return await alert.present();
    }

    render() {
        return [<div class="ion-padding"><h2>{this.deckOrSlide ? 'Background' : 'Image'}</h2></div>,
            <ion-list>
                {this.renderDeckOrSlide()}

                <ion-item class="ion-margin-top action-button">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_PHOTOS)} color="primary">
                        <ion-label class="ion-text-uppercase">Add a stock photo</ion-label>
                    </ion-button>
                </ion-item>

                {this.renderDeleteAction()}

                <ion-item-divider class="ion-padding-top">
                    <ion-label>History</ion-label>
                    <button slot="end" class="info" onClick={() => this.presentHistoryInfo()}>
                        <ion-icon name="help"></ion-icon>
                    </button>
                </ion-item-divider>

                {this.renderPhotosHistory()}
            </ion-list>
        ];
    }

    private renderDeckOrSlide() {
        if (!this.deckOrSlide) {
            return undefined;
        } else{
            return [
                <ion-item-divider class="ion-padding-top"><ion-label>Apply change to</ion-label></ion-item-divider>,
                <ion-radio-group onIonChange={($event) => this.selectApplyToAllDeck($event)}>
                    <ion-item>
                        <ion-label>Just this slide</ion-label>
                        <ion-radio slot="start" value={false} checked mode="md"></ion-radio>
                    </ion-item>
                    <ion-item>
                        <ion-label>The all deck</ion-label>
                        <ion-radio slot="start" value={true} mode="md"></ion-radio>
                    </ion-item>
                </ion-radio-group>
            ]
        }
    }

    private renderDeleteAction() {
        if (!this.deckOrSlide) {
            return undefined;
        } else {
            return <ion-item class="action-button">
                <ion-button shape="round" onClick={() => this.closePopover(ImageAction.DELETE_PHOTO)} color="medium" fill="outline">
                    <ion-label class="ion-text-uppercase">Delete background</ion-label>
                </ion-button>
            </ion-item>;
        }
    }

    private renderPhotosHistory() {
        if (!this.photosHistoryOdd && !this.photosHistoryEven) {
            return <ion-item class="history-empty">
                <ion-label class="ion-text-wrap"><small>You have not used any photos so far</small></ion-label>
            </ion-item>
        } else {
            return <div class="history-photos ion-padding">
                <app-stock-photos photosOdd={this.photosHistoryOdd} photosEven={this.photosHistoryEven} onSelectPhoto={($event: CustomEvent) => this.selectPhotoFromHistory($event)}></app-stock-photos>
            </div>
        }
    }
}
