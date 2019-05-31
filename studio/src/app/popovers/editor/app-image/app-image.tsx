import {Component, Element, Prop, State} from '@stencil/core';

import {ImageAction} from './image-action';

import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

@Component({
        tag: 'app-image',
    styleUrl: 'app-image.scss'
})
export class AppImage {

    @Element() el: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    private applyToAllDeck: boolean = false;

    private imageHistoryService: ImageHistoryService;

    @State()
    private imagesHistoryOdd: (UnsplashPhoto | TenorGif)[];

    @State()
    private imagesHistoryEven: (UnsplashPhoto | TenorGif)[];

    constructor() {
        this.imageHistoryService = ImageHistoryService.getInstance();
    }

    async componentWillLoad() {
        await this.initImagesHistory();
    }

    private initImagesHistory(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const imagesHistory: (UnsplashPhoto | TenorGif)[] = await this.imageHistoryService.get();

            if (!imagesHistory || imagesHistory.length <= 0) {
                resolve();
                return;
            }

            this.imagesHistoryEven = [...imagesHistory.filter((_a, i) => i % 2)];
            this.imagesHistoryOdd = [...imagesHistory.filter((_a, i) => !(i % 2))];

            resolve();
        });
    }

    private async closePopover(action: ImageAction, image?: UnsplashPhoto | TenorGif) {
        const data = {
            action: action
        };

        if (this.deckOrSlide) {
            data['applyToAllDeck'] = this.applyToAllDeck;
        }

        if (image) {
            data['image'] = image;
        }

        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss(data);
    }

    private selectImageFromHistory($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            await this.closePopover(ImageAction.ADD_IMAGE, $event.detail);

            resolve();
        });
    }

    private selectApplyToAllDeck($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToAllDeck = $event.detail.value;
        }
    }

    private async presentHistoryInfo() {
        const alert: HTMLIonAlertElement = await IonControllerUtils.createAlert({
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

                <ion-item class="action-button">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_GIFS)} color="primary">
                        <ion-label class="ion-text-uppercase">Add a gif</ion-label>
                    </ion-button>
                </ion-item>

                {this.renderDeleteAction()}

                <ion-item-divider class="ion-padding-top">
                    <ion-label>History</ion-label>
                    <button slot="end" class="info" onClick={() => this.presentHistoryInfo()}>
                        <ion-icon name="help"></ion-icon>
                    </button>
                </ion-item-divider>

                {this.renderImagesHistory()}
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
                <ion-button shape="round" onClick={() => this.closePopover(ImageAction.DELETE_BACKGROUND)} color="medium" fill="outline">
                    <ion-label class="ion-text-uppercase">Delete background</ion-label>
                </ion-button>
            </ion-item>;
        }
    }

    private renderImagesHistory() {
        if (!this.imagesHistoryOdd && !this.imagesHistoryEven) {
            return <ion-item class="history-empty">
                <ion-label class="ion-text-wrap"><small>You have not used any photos so far</small></ion-label>
            </ion-item>
        } else {
            return <div class="history-photos ion-padding">
                <app-image-columns imagesOdd={this.imagesHistoryOdd} imagesEven={this.imagesHistoryEven} onSelectImage={($event: CustomEvent) => this.selectImageFromHistory($event)}></app-image-columns>
            </div>
        }
    }
}
