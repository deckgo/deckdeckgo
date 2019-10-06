import {Component, Element, EventEmitter, h, Prop, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {Deck} from '../../../models/data/deck';

import {QRCodeUtils} from '../../../utils/editor/qrcode.utils';
import {ImageAction} from '../app-image/image-action';

import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-edit-slide',
    styleUrl: 'app-edit-slide.scss'
})
export class AppEditSlide {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    qrCode: boolean;

    @Prop()
    slideDidChange: EventEmitter<HTMLElement>;

    @State()
    private customQRCode: boolean = false;

    @State()
    private customContent: string = undefined;

    private deckEditorService: DeckEditorService;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
    }

    componentWillLoad() {
        this.customQRCode = this.selectedElement && this.selectedElement.hasAttribute('custom-qrcode') && this.selectedElement.getAttribute('custom-qrcode') === 'true';

        this.customContent = this.customQRCode && this.selectedElement ? this.selectedElement.getAttribute('content') : undefined;
    }

    private async closePopoverWithoutResults() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async closePopover(action: ImageAction) {
        const data = {
            action: action
        };

        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss(data);
    }

    private async onRadioCustomLink($event: CustomEvent) {
        if ($event && $event.detail && $event.detail.value !== this.customQRCode) {
            this.customQRCode = $event.detail.value;

            await this.setDefaultQRCodeLink();
        }
    }

    private handleInput($event: CustomEvent<KeyboardEvent>) {
        this.customContent = ($event.target as InputTargetEvent).value;
    }

    private onInputCustomUrlChange(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            const valid: boolean = await this.validateCustomUrl();

            if (!valid) {
                resolve();
                return;
            }

            this.selectedElement.setAttribute('content', this.customContent);
            this.selectedElement.setAttribute('custom-qrcode', `${true}`);

            this.slideDidChange.emit(this.selectedElement);

            resolve();
        });
    }

    private validateCustomUrl(): Promise<boolean> {
        return new Promise<boolean>((resolve) => {
            try {
                if (!this.customContent) {
                    resolve(false);
                    return;
                }

                new URL(this.customContent);

                // https://stackoverflow.com/a/14582229/5404186
                const pattern: RegExp = new RegExp('^(https?:\\/\\/)?'+ // protocol
                    '((([a-z\\d]([a-z\\d-]*[a-z\\d])*)\\.?)+[a-z]{2,}|'+ // domain name
                    '((\\d{1,3}\\.){3}\\d{1,3}))'+ // OR ip (v4) address
                    '(\\:\\d+)?(\\/[-a-z\\d%_.~+]*)*'+ // port and path
                    '(\\?[;&a-z\\d%_.~+=-]*)?'+ // query string
                    '(\\#[-a-z\\d_]*)?$','i'); // fragment locator

                resolve(pattern.test(this.customContent));
            } catch (err) {
                resolve(false);
            }
        });
    }

    private setDefaultQRCodeLink(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (this.customQRCode) {
                resolve();
                return;
            }

            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                this.selectedElement.setAttribute('content', QRCodeUtils.getPresentationUrl(deck));

                if (this.selectedElement.hasAttribute('custom-qrcode')) {
                    this.selectedElement.setAttribute('custom-qrcode', `${false}`);
                }

                this.customContent = undefined;

                this.slideDidChange.emit(this.selectedElement);

                resolve();
            });
        });
    }

    render() {
        return [<ion-toolbar>
            <h2>Slide options</h2>
            <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <ion-list>
                {this.renderOptions()}
            </ion-list>
        ]
    }

    private renderOptions() {
        if (this.qrCode) {
            return <ion-radio-group onIonChange={($event) => this.onRadioCustomLink($event)}>
                <ion-item-divider class="ion-padding-top">
                    <ion-label>Scanning the QR code lead to</ion-label>
                </ion-item-divider>

                <ion-item>
                    <ion-label>Your presentation</ion-label>
                    <ion-radio slot="start" value={false} mode="md" checked={!this.customQRCode}></ion-radio>
                </ion-item>
                <ion-item>
                    <ion-label>A custom url</ion-label>
                    <ion-radio slot="start" value={true} mode="md" checked={this.customQRCode}></ion-radio>
                </ion-item>

                <ion-item class="with-padding">
                    <ion-input value={this.customContent} placeholder="https://..." debounce={500} disabled={!this.customQRCode}
                               onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
                               onIonChange={() => this.onInputCustomUrlChange()}></ion-input>
                </ion-item>

                <ion-item-divider class="ion-padding-top">
                    <ion-label>QR code logo</ion-label>
                </ion-item-divider>

                <ion-item class="action-button">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_CUSTOM_LOGO)}
                                color="tertiary">
                        <ion-label class="ion-text-uppercase">Your image</ion-label>
                    </ion-button>
                </ion-item>

                <ion-item class="action-button ion-margin-bottom">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.DELETE_LOGO)}
                                fill="outline" class="delete">
                        <ion-label class="ion-text-uppercase">Delete image</ion-label>
                    </ion-button>
                </ion-item>
            </ion-radio-group>;
        } else {
            return undefined;
        }
    }
}
