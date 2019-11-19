import {Component, Element, Event, EventEmitter, h, Method, Prop, State, Watch} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';

enum ApplyColorType {
    TEXT,
    BACKGROUND
}

@Component({
    tag: 'app-color-deck-slide'
})
export class AppColorDeckSlide {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    moreColors: boolean = true;

    @Prop()
    applyToAllDeck: boolean = false;

    @Prop()
    deckOrSlide: boolean = false;

    @State()
    private color: string;

    @State()
    private colorOpacity: number = 100;

    @State()
    private applyColorType: ApplyColorType = ApplyColorType.TEXT;

    @Event() colorChange: EventEmitter<boolean>;

    @State()
    private colorText: string;

    @State()
    private colorBackground: string;

    async componentWillLoad() {
        await this.initCurrentColors();
    }

    @Watch('applyToAllDeck')
    async onApplyToAllDeckChange() {
        await this.initCurrentColors();
    }

    @Method()
    async initCurrentColors() {
        if (!this.selectedElement) {
            return;
        }

        const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

        if (!element) {
            return;
        }

        let styleColor: InitStyleColor;

        if (this.applyColorType === ApplyColorType.BACKGROUND) {
            styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--background') ? element.style.getPropertyValue('--background') : element.style.background);
        } else {
            styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--color') ? element.style.getPropertyValue('--color') : element.style.color);
        }

        this.color = styleColor.rgb;
        this.colorOpacity = styleColor.opacity;

        await this.initCurrentRadioButtonsColors();
    }

    private async initCurrentRadioButtonsColors() {
        const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

        if (!element) {
            return;
        }

        const styleColorBackground: InitStyleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--background') ? element.style.getPropertyValue('--background') : element.style.background);;
        const styleColorText: InitStyleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--color') ? element.style.getPropertyValue('--color') : element.style.color);

        this.colorText = styleColorText.rgb === '255, 255, 255' ? 'var(--ion-color-medium-rgb)' : styleColorText.rgb;
        this.colorBackground = styleColorBackground.rgb === '255, 255, 255' ? 'var(--ion-color-medium-rgb)' : styleColorBackground.rgb;
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

        await this.initCurrentRadioButtonsColors();
    }

    private async applyColor() {
        if (this.applyColorType === ApplyColorType.BACKGROUND) {
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

            const element: HTMLElement = this.deckOrSlide ? (this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement) : this.selectedElement;

            if (this.applyColorType === ApplyColorType.BACKGROUND) {
                element.style.removeProperty('--background');
                element.style.removeProperty('--slide-split-background-start');
                element.style.removeProperty('--slide-split-background-end');
                element.style.removeProperty('background');
            } else {
                element.style.removeProperty('--color');
                element.style.removeProperty('color');
            }

            this.color = null;
            this.colorOpacity = 100;

            this.colorChange.emit(this.applyToAllDeck);

            resolve();
        });
    }

    private applyTextColor(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.color) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

            if (this.deckOrSlide) {
                const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

                element.style.setProperty('--color', selectedColor);
            } else {
                this.selectedElement.style.color = selectedColor;
            }

            this.colorChange.emit(this.applyToAllDeck);

            resolve();
        });
    }

    private applyBackground(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.color) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

            if (this.deckOrSlide) {
                const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

                element.style.setProperty('--background', selectedColor);
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

            this.colorChange.emit(this.applyToAllDeck);

            resolve();
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
        return [
            <ion-list>
                <ion-radio-group onIonChange={($event) => this.selectApplyType($event)}>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>Apply color to</ion-label>
                    </ion-item-divider>

                    <ion-item>
                        <ion-label>Text</ion-label>
                        <ion-radio slot="start" value={ApplyColorType.TEXT} checked mode="md" style={{'--color': `rgb(${this.colorText})`, '--color-checked': `rgb(${this.colorText})`}}></ion-radio>
                    </ion-item>

                    <ion-item>
                        <ion-label>Background</ion-label>
                        <ion-radio slot="start" value={ApplyColorType.BACKGROUND} mode="md" style={{'--color': `rgb(${this.colorBackground})`, '--color-checked': `rgb(${this.colorBackground})`}}></ion-radio>
                    </ion-item>
                </ion-radio-group>
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

    private resetLabelContent() {
        if (this.applyColorType === ApplyColorType.BACKGROUND) {
            return "Reset background";
        } else {
            return "Reset color";
        }
    }
}
