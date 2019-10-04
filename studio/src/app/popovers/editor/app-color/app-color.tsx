import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {isIPad} from '@deckdeckgo/utils';
import {ColorType} from '../../../utils/editor/color-type';

interface InitStyleColor {
    rgb: string | null;
    opacity: number | null;
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

    private applyToAllDeck: boolean = false;

    @State()
    private applyToText: boolean = true; // true = text, false = background

    @State()
    private color: string;

    @State()
    private background: string;

    @State()
    private colorOpacity: number = 100;

    @State()
    private backgroundOpacity: number = 100;

    @State()
    private moreColors: boolean = true;

    async componentWillLoad() {
        await this.initCurrentColors(this.selectedElement);

        this.moreColors = !isIPad();
    }

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async selectApplyToAllDeck($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToAllDeck = $event.detail === ColorType.DECK;

            if (this.deckOrSlide) {
                await this.initCurrentColors(this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement);
            }
        }
    }

    private selectApplyToText($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToText = $event.detail.value;
        }
    }

    private async selectColor($event: CustomEvent) {
        if (!this.selectedElement || !$event || !$event.detail) {
            return;
        }

        if (this.applyToText) {
            this.color = $event.detail.rgb;
        } else {
            this.background = $event.detail.rgb;
        }

        await this.applyColor();
    }

    private async applyColor() {
        if (this.applyToText) {
            await this.applyTextColor();
        } else {
            await this.applyBackground();
        }
    }

    private resetColor(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (this.applyToText) {
                this.selectedElement.style.removeProperty('--color');
                this.selectedElement.style.removeProperty('color');

                this.color = null;
                this.colorOpacity = 100;
            } else {
                this.selectedElement.style.removeProperty('--background');
                this.selectedElement.style.removeProperty('--slide-split-background-start');
                this.selectedElement.style.removeProperty('--slide-split-background-end');
                this.selectedElement.style.removeProperty('background');

                this.background = null;
                this.backgroundOpacity = 100;
            }

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
                const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

                element.style.setProperty('--color', selectedColor);
            } else {
                this.selectedElement.style.color = selectedColor;
            }

            this.colorDidChange.emit(this.applyToAllDeck);

            resolve();
        });
    }

    private transformOpacity(): number {
        return this.applyToText ? (this.colorOpacity === 0 ? 0 : this.colorOpacity / 100) : (this.backgroundOpacity === 0 ? 0 : this.backgroundOpacity / 100);
    }

    private applyBackground(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.background) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.background},${this.transformOpacity()})`;

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

            this.colorDidChange.emit(this.applyToAllDeck);

            resolve();
        });
    }

    private async initCurrentColors(element: HTMLElement) {
        if (!element) {
            return;
        }

        const styleColor: InitStyleColor = await this.splitColor(element.style.getPropertyValue('--color') ? element.style.getPropertyValue('--color') : element.style.color);
        this.color = styleColor.rgb;
        this.colorOpacity = styleColor.opacity;

        const styleBackground: InitStyleColor = await this.splitColor(element.style.getPropertyValue('--background') ? element.style.getPropertyValue('--background') : element.style.background);
        this.background = styleBackground.rgb;
        this.backgroundOpacity = styleBackground.opacity;
    }

    private splitColor(styleColor: string): Promise<InitStyleColor> {
        return new Promise<InitStyleColor>((resolve) => {
            if (styleColor && styleColor !== undefined) {
                const rgbs: RegExpMatchArray | null = styleColor.match(/[.?\d]+/g);

                if (rgbs && rgbs.length >= 3) {
                    resolve({
                        rgb: `${rgbs[0]}, ${rgbs[1]}, ${rgbs[2]}`,
                        opacity: rgbs.length > 3 ? parseFloat(rgbs[3]) * 100 : null
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

            if (this.applyToText) {
                this.colorOpacity = opacity;
            } else {
                this.backgroundOpacity = opacity;
            }

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
                <app-deck-or-slide deckOrSlide={this.deckOrSlide}
                                   onApplyTo={($event: CustomEvent) => this.selectApplyToAllDeck($event)}></app-deck-or-slide>

                <ion-radio-group onIonChange={($event) => this.selectApplyToText($event)}>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>Apply color to</ion-label>
                    </ion-item-divider>

                    <ion-item>
                        <ion-label>Text</ion-label>
                        <ion-radio slot="start" value={true} checked mode="md"></ion-radio>
                    </ion-item>
                    <ion-item>
                        <ion-label>Background</ion-label>
                        <ion-radio slot="start" value={false} mode="md"></ion-radio>
                    </ion-item>
                </ion-radio-group>

                <ion-item-divider class="ion-padding-top">
                    <ion-label>Opacity</ion-label>
                </ion-item-divider>

                <ion-item class="item-opacity">
                    <ion-range color="primary" min={0} max={100} disabled={this.applyToText ? !this.color || this.color === undefined : !this.background || this.background === undefined}
                               value={this.applyToText ? this.colorOpacity : this.backgroundOpacity} mode="md"
                               onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity(e)}></ion-range>
                </ion-item>
            </ion-list>,
            <deckgo-color class="ion-padding-start ion-padding-end ion-padding-bottom" more={this.moreColors} onColorChange={($event: CustomEvent) => this.selectColor($event)} color-rgb={this.applyToText ? this.color : this.background}>
                <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>,
            <ion-item class="action-button ion-margin-bottom">
                <ion-button shape="round" onClick={() => this.resetColor()}
                            fill="outline" class="delete">
                    <ion-label class="ion-text-uppercase">Reset {this.applyToText ? 'text color' : 'background'}</ion-label>
                </ion-button>
            </ion-item>
        ]
    }

}
