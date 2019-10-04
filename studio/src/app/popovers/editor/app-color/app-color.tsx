import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {isIPad} from '@deckdeckgo/utils';

import {TargetElement} from '../../../utils/editor/target-element';

interface InitStyleColor {
    rgb: string | null;
    opacity: number | null;
}

enum ApplyColorType {
    TEXT,
    BACKGROUND,
    QR_CODE
}

@Component({
    tag: 'app-color',
    styleUrl: 'app-color.scss'
})
export class AppColor {

    @Element() el: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    @Prop()
    selectedElement: HTMLElement;

    @Event() colorDidChange: EventEmitter<boolean>;

    @State()
    private applyToTargetElement: TargetElement = TargetElement.SLIDE;

    @State()
    private applyColorType: ApplyColorType = ApplyColorType.TEXT;

    @State()
    private color: string;

    @State()
    private colorOpacity: number = 100;

    @State()
    private moreColors: boolean = true;

    @State()
    private qrCode: boolean = false;

    async componentWillLoad() {
        await this.initCurrentColors();

        this.qrCode = this.deckOrSlide && this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-qrcode'.toUpperCase();

        this.moreColors = !isIPad();
    }

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async selectApplyToTargetElement($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToTargetElement = $event.detail;

            if (this.applyToTargetElement === TargetElement.QR_CODE && this.applyColorType === ApplyColorType.TEXT) {
                this.applyColorType = ApplyColorType.QR_CODE;
            } else if (this.applyToTargetElement !== TargetElement.QR_CODE && this.applyColorType === ApplyColorType.QR_CODE) {
                this.applyColorType = ApplyColorType.TEXT;
            }

            await this.initCurrentColors();
        }
    }

    private async selectApplyType($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyColorType = $event.detail.value;

            await this.initCurrentColors();
        }
    }

    private async selectColor($event: CustomEvent) {
        if (!this.selectedElement || !$event || !$event.detail) {
            return;
        }

        this.color = $event.detail.rgb;

        await this.applyColor();
    }

    private async applyColor() {
        if (this.applyColorType === ApplyColorType.QR_CODE) {
            await this.applyQRCodeColor();
        } else if (this.applyColorType === ApplyColorType.BACKGROUND) {
            await this.applyBackground();
        } else {
            await this.applyTextColor();
        }
    }

    private resetColor(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            const applyToQRCode: boolean = this.qrCode && this.applyToTargetElement === TargetElement.QR_CODE;

            if (this.applyColorType === ApplyColorType.QR_CODE) {
                this.selectedElement.style.removeProperty('--deckgo-qrcode-color-fill');
            } else if (this.applyColorType === ApplyColorType.BACKGROUND) {
                if (applyToQRCode) {
                    this.selectedElement.style.removeProperty('--deckgo-qrcode-background-fill');
                } else {
                    this.selectedElement.style.removeProperty('--background');
                    this.selectedElement.style.removeProperty('--slide-split-background-start');
                    this.selectedElement.style.removeProperty('--slide-split-background-end');
                    this.selectedElement.style.removeProperty('background');
                }
            } else {
                this.selectedElement.style.removeProperty('--color');
                this.selectedElement.style.removeProperty('color');
            }

            this.color = null;
            this.colorOpacity = 100;

            this.colorDidChange.emit(this.isApplyToAllDeck() && !applyToQRCode);

            resolve();
        });
    }

    private applyTextColor(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.color) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.color},${this.transformOpacity()})`;

            if (this.deckOrSlide) {
                const element: HTMLElement = this.isApplyToAllDeck() ? this.selectedElement.parentElement : this.selectedElement;

                element.style.setProperty('--color', selectedColor);
            } else {
                this.selectedElement.style.color = selectedColor;
            }

            this.colorDidChange.emit(this.isApplyToAllDeck());

            resolve();
        });
    }

    private applyQRCodeColor(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.color) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.color},${this.transformOpacity()})`;

            this.selectedElement.style.setProperty('--deckgo-qrcode-color-fill', selectedColor);

            this.colorDidChange.emit(false);

            resolve();
        });
    }

    private isApplyToAllDeck(): boolean {
        return this.applyToTargetElement === TargetElement.DECK;
    }

    private transformOpacity(): number {
        return this.colorOpacity === 0 ? 0 : this.colorOpacity / 100;
    }

    private applyBackground(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.color) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.color},${this.transformOpacity()})`;

            const applyToQRCode: boolean = this.qrCode && this.applyToTargetElement === TargetElement.QR_CODE;

            if (this.deckOrSlide) {
                if (applyToQRCode) {
                    this.selectedElement.style.setProperty('--deckgo-qrcode-background-fill', selectedColor);
                } else {
                    const element: HTMLElement = this.isApplyToAllDeck() ? this.selectedElement.parentElement : this.selectedElement;

                    element.style.setProperty('--background', selectedColor);
                }
            } else if (this.selectedElement.parentElement && this.selectedElement.parentElement.nodeName && this.selectedElement.parentElement.nodeName.toLowerCase() === 'deckgo-slide-split') {
                const element: HTMLElement = this.selectedElement.parentElement;

                if (this.selectedElement.getAttribute('slot') === 'start') {
                    element.style.setProperty('--slide-split-background-start', selectedColor);
                } else if (this.selectedElement.getAttribute('slot') === 'end') {
                    element.style.setProperty('--slide-split-background-end', selectedColor);
                } else {
                    this.selectedElement.style.background = selectedColor;
                }
            } else {
                this.selectedElement.style.background = selectedColor;
            }

            this.colorDidChange.emit(this.isApplyToAllDeck() && !applyToQRCode);

            resolve();
        });
    }

    private async initCurrentColors() {
        if (!this.selectedElement) {
            return;
        }

        const element: HTMLElement = this.isApplyToAllDeck() ? this.selectedElement.parentElement : this.selectedElement;

        if (!element) {
            return;
        }

        let styleColor: InitStyleColor;

        if (this.applyColorType === ApplyColorType.QR_CODE) {
            styleColor = await this.splitColor(element.style.getPropertyValue('--deckgo-qrcode-color-fill'));
        } else if (this.applyColorType === ApplyColorType.BACKGROUND) {
            if (this.applyToTargetElement === TargetElement.QR_CODE) {
                styleColor = await this.splitColor(element.style.getPropertyValue('--deckgo-qrcode-background-fill'));
            } else {
                styleColor = await this.splitColor(element.style.getPropertyValue('--background') ? element.style.getPropertyValue('--background') : element.style.background);
            }
        } else {
            styleColor = await this.splitColor(element.style.getPropertyValue('--color') ? element.style.getPropertyValue('--color') : element.style.color);
        }

        this.color = styleColor.rgb;
        this.colorOpacity = styleColor.opacity;
    }

    private splitColor(styleColor: string): Promise<InitStyleColor> {
        return new Promise<InitStyleColor>((resolve) => {
            if (styleColor && styleColor !== undefined) {
                const rgbs: RegExpMatchArray | null = styleColor.match(/[.?\d]+/g);

                if (rgbs && rgbs.length >= 3) {
                    resolve({
                        rgb: `${rgbs[0]}, ${rgbs[1]}, ${rgbs[2]}`,
                        opacity: rgbs.length > 3 ? parseFloat(rgbs[3]) * 100 : 100
                    });

                    return;
                }
            }

            resolve({
                rgb: null,
                opacity: 100
            });
        });
    }

    private updateOpacity($event: CustomEvent<RangeChangeEventDetail>): Promise<void> {
        return new Promise<void>(async (resolve) => {

            if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
                resolve();
                return;
            }

            $event.stopPropagation();

            const opacity: number = $event.detail.value as number;

            this.colorOpacity = opacity;

            await this.applyColor();

            resolve();
        });
    }

    render() {
        return [<ion-toolbar>
            <h2>Color</h2>
            <ion-router-link slot="end" onClick={() => this.closePopover()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <ion-list>
                <app-select-target-element deckOrSlide={this.deckOrSlide} qrCode={this.qrCode}
                                           onApplyTo={($event: CustomEvent) => this.selectApplyToTargetElement($event)}></app-select-target-element>

                {this.renderApplyColorTo()}

                <ion-item-divider class="ion-padding-top">
                    <ion-label>Opacity</ion-label>
                </ion-item-divider>

                <ion-item class="item-opacity">
                    <ion-range color="primary" min={0} max={100} disabled={!this.color || this.color === undefined}
                               value={this.colorOpacity} mode="md"
                               onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity(e)}></ion-range>
                </ion-item>
            </ion-list>,
            <deckgo-color class="ion-padding-start ion-padding-end ion-padding-bottom" more={this.moreColors}
                          onColorChange={($event: CustomEvent) => this.selectColor($event)} color-rgb={this.color}>
                <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>,
            <ion-item class="action-button ion-margin-bottom">
                <ion-button shape="round" onClick={() => this.resetColor()}
                            fill="outline" class="delete">
                    <ion-label class="ion-text-uppercase">{this.resetLabelContent()}</ion-label>
                </ion-button>
            </ion-item>
        ]
    }

    private renderApplyColorTo() {
        return <ion-radio-group onIonChange={($event) => this.selectApplyType($event)}>
            <ion-item-divider class="ion-padding-top">
                <ion-label>Apply color to</ion-label>
            </ion-item-divider>

            {this.renderApplyColorToTextOrQRCode()}
            <ion-item>
                <ion-label>Background</ion-label>
                <ion-radio slot="start" value={ApplyColorType.BACKGROUND} mode="md"></ion-radio>
            </ion-item>
        </ion-radio-group>;
    }

    private renderApplyColorToTextOrQRCode() {
        if (this.applyToTargetElement === TargetElement.QR_CODE) {
            return <ion-item>
                <ion-label>Fill</ion-label>
                <ion-radio slot="start" value={ApplyColorType.QR_CODE} checked mode="md"></ion-radio>
            </ion-item>;
        } else {
            return <ion-item>
                <ion-label>Text</ion-label>
                <ion-radio slot="start" value={ApplyColorType.TEXT} checked mode="md"></ion-radio>
            </ion-item>;
        }
    }

    private resetLabelContent() {
        if (this.applyColorType === ApplyColorType.QR_CODE) {
            return "Reset fill color";
        } else if (this.applyColorType === ApplyColorType.BACKGROUND) {
            return "Reset background";
        } else {
            return "Reset color";
        }
    }
}
