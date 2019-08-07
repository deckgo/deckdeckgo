import {Component, Element, Prop, State, h, EventEmitter} from '@stencil/core';

import {ImageAction} from './image-action';

import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

enum ImageSize {
    SMALL = '25%',
    MEDIUM = '50%',
    LARGE = '75%',
    ORIGINAL = '100%'
}

enum ImageAlignment {
    START = 'flex-start',
    CENTER = 'center',
    END = 'flex-end'
}

@Component({
    tag: 'app-image',
    styleUrl: 'app-image.scss'
})
export class AppImage {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    @Prop()
    imgDidChange: EventEmitter<HTMLElement>;

    private applyToAllDeck: boolean = false;

    private imageHistoryService: ImageHistoryService;

    @State()
    private imagesHistoryOdd: (UnsplashPhoto | TenorGif | StorageFile)[];

    @State()
    private imagesHistoryEven: (UnsplashPhoto | TenorGif | StorageFile)[];

    @State()
    private currentImageSize: ImageSize;

    @State()
    private currentImageAlignment: ImageAlignment;

    constructor() {
        this.imageHistoryService = ImageHistoryService.getInstance();
    }

    async componentWillLoad() {
        await this.initImagesHistory();

        this.currentImageSize = await this.initImageSize();
        this.currentImageAlignment = await this.initImageAlignment();
    }

    private initImagesHistory(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const imagesHistory: (UnsplashPhoto | TenorGif | StorageFile)[] = await this.imageHistoryService.get();

            if (!imagesHistory || imagesHistory.length <= 0) {
                resolve();
                return;
            }

            this.imagesHistoryEven = [...imagesHistory.filter((_a, i) => i % 2)];
            this.imagesHistoryOdd = [...imagesHistory.filter((_a, i) => !(i % 2))];

            resolve();
        });
    }

    private initImageSize(): Promise<ImageSize> {
        return new Promise<ImageSize>((resolve) => {
            if (!this.selectedElement || !this.selectedElement.style) {
                resolve(null);
                return;
            }

            if (this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height') === '25%') {
                resolve(ImageSize.SMALL);
            } else if (this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height') === '50%') {
                resolve(ImageSize.MEDIUM);
            } else if (this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height') === '75%') {
                resolve(ImageSize.LARGE);
            } else {
                resolve(ImageSize.ORIGINAL);
            }
        });
    }

    private initImageAlignment(): Promise<ImageAlignment> {
        return new Promise<ImageAlignment>(async (resolve) => {
            if (!this.selectedElement || !this.selectedElement.style) {
                resolve(null);
                return;
            }

            if (this.selectedElement.style.getPropertyValue('justify-content') === 'center') {
                resolve(ImageAlignment.CENTER);
            } else if (this.selectedElement.style.getPropertyValue('justify-content') === 'flex-end') {
                resolve(ImageAlignment.END);
            } else if (this.selectedElement.style.getPropertyValue('justify-content') === 'flex-start') {
                resolve(ImageAlignment.START);
            } else {
                const result: ImageAlignment = await this.findSlideDefaultAlignment();
                resolve(result);
            }
        });
    }

    private findSlideDefaultAlignment(): Promise<ImageAlignment> {
        return new Promise<ImageAlignment>((resolve) => {
            const parent: HTMLElement = this.selectedElement.parentElement;

            if (parent && parent.nodeName && parent.nodeName.toLowerCase().indexOf('deckgo-slide') > -1) {
                const container: HTMLElement = parent.shadowRoot.querySelector('.deckgo-slide');
                if (container) {
                    const style: CSSStyleDeclaration = window.getComputedStyle(container);

                    if (style && style.alignItems === 'center') {
                        resolve(ImageAlignment.CENTER);
                        return;
                    }
                }
            }

            resolve(ImageAlignment.START);
        });
    }

    private async closePopoverWithoutResults() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async closePopover(action: ImageAction, image?: UnsplashPhoto | TenorGif | StorageFile) {
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
        if ($event) {
            this.applyToAllDeck = $event.detail;
        }
    }

    private async presentHistoryInfo() {
        const alert: HTMLIonAlertElement = await IonControllerUtils.createAlert({
            message: 'The editor keeps track of the last 10 images you would have use in any of your presentations.<br/><br/>Select one to add it again quickly.',
            buttons: ['Ok']
        });

        return await alert.present();
    }

    private toggleImageSize($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            this.currentImageSize = $event.detail.value;

            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (this.currentImageSize === ImageSize.ORIGINAL) {
                this.selectedElement.style.removeProperty('--deckgo-lazy-img-height');
            } else {
                this.selectedElement.style.setProperty('--deckgo-lazy-img-height', this.currentImageSize);
            }

            this.imgDidChange.emit(this.selectedElement);

            resolve();
        });
    }

    private toggleImageAlignment($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            this.currentImageAlignment = $event.detail.value;

            if (!this.selectedElement) {
                resolve();
                return;
            }

            this.selectedElement.style.setProperty('display', 'inline-flex');
            this.selectedElement.style.setProperty('justify-content', this.currentImageAlignment);

            this.imgDidChange.emit(this.selectedElement);

            resolve();
        });
    }

    render() {
        return [<ion-toolbar>
            <h2>{this.deckOrSlide ? 'Background' : 'Image'}</h2>
            <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <ion-list>
                <app-deck-or-slide deckOrSlide={this.deckOrSlide}
                                   onApplyTo={($event: CustomEvent) => this.selectApplyToAllDeck($event)}></app-deck-or-slide>

                {this.renderImageSize()}
                {this.renderImageAlignment()}

                <ion-item class="action-button action-button-margin">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_PHOTOS)}
                                color="primary">
                        <ion-label class="ion-text-uppercase">Stock photo</ion-label>
                    </ion-button>
                </ion-item>

                <ion-item class="action-button">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_GIFS)}
                                color="secondary">
                        <ion-label class="ion-text-uppercase">Gif</ion-label>
                    </ion-button>
                </ion-item>

                <ion-item class="action-button">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_CUSTOM)}
                                color="tertiary">
                        <ion-label class="ion-text-uppercase">Your images</ion-label>
                    </ion-button>
                </ion-item>

                {this.renderDeleteAction()}

                <ion-item-divider class="ion-padding-top ion-margin-top">
                    <ion-label>History</ion-label>
                    <button slot="end" class="info" onClick={() => this.presentHistoryInfo()}>
                        <ion-icon name="help"></ion-icon>
                    </button>
                </ion-item-divider>

                {this.renderImagesHistory()}
            </ion-list>
        ];
    }

    private renderDeleteAction() {
        if (!this.deckOrSlide) {
            return undefined;
        } else {
            return <ion-item class="action-button ion-margin-bottom">
                <ion-button shape="round" onClick={() => this.closePopover(ImageAction.DELETE_BACKGROUND)}
                            fill="outline" class="delete">
                    <ion-label class="ion-text-uppercase">Delete background</ion-label>
                </ion-button>
            </ion-item>;
        }
    }

    private renderImagesHistory() {
        if (!this.imagesHistoryOdd && !this.imagesHistoryEven) {
            return <ion-item class="history-empty">
                <ion-label class="ion-text-wrap"><small>You have not used any images so far.</small></ion-label>
            </ion-item>
        } else {
            return <div class="history-photos ion-padding">
                <app-image-columns imagesOdd={this.imagesHistoryOdd} imagesEven={this.imagesHistoryEven}
                                   onSelectImage={($event: CustomEvent) => this.selectImageFromHistory($event)}></app-image-columns>
            </div>
        }
    }

    private renderImageSize() {
        if (this.deckOrSlide) {
            return undefined;
        } else {
            return [
                <ion-item-divider class="ion-padding-top">
                    <ion-label>Size</ion-label>
                </ion-item-divider>,

                <ion-item class="select">
                    <ion-label>Size</ion-label>

                    <ion-select value={this.currentImageSize} placeholder="Select an image size"
                                onIonChange={(e: CustomEvent) => this.toggleImageSize(e)}
                                class="ion-padding-start ion-padding-end">
                        <ion-select-option value={ImageSize.SMALL}>Small</ion-select-option>
                        <ion-select-option value={ImageSize.MEDIUM}>Medium</ion-select-option>
                        <ion-select-option value={ImageSize.LARGE}>Large</ion-select-option>
                        <ion-select-option value={ImageSize.ORIGINAL}>Original</ion-select-option>
                    </ion-select>
                </ion-item>
            ]
        }
    }

    private renderImageAlignment() {
        if (this.deckOrSlide) {
            return undefined;
        } else {
            return [
                <ion-item-divider class="ion-padding-top">
                    <ion-label>Alignment</ion-label>
                </ion-item-divider>,

                <ion-item class="select">
                    <ion-label>Alignment</ion-label>

                    <ion-select value={this.currentImageAlignment} placeholder="Align the image"
                                onIonChange={(e: CustomEvent) => this.toggleImageAlignment(e)}
                                class="ion-padding-start ion-padding-end">
                        <ion-select-option value={ImageAlignment.START}>Start</ion-select-option>
                        <ion-select-option value={ImageAlignment.CENTER}>Center</ion-select-option>
                        <ion-select-option value={ImageAlignment.END}>End</ion-select-option>
                    </ion-select>
                </ion-item>
            ]
        }
    }
}
