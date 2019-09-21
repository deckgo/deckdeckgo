import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {isIPad} from '@deckdeckgo/utils';

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
    private moreColors: boolean = true;

    async componentWillLoad() {
        await this.initCurrentColors(this.selectedElement);

        this.moreColors = !isIPad();
    }

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async selectApplyToAllDeck($event: CustomEvent) {
        if ($event) {
            this.applyToAllDeck = $event.detail;

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

        const selectedColor: string = $event.detail.hex;

        if (this.applyToText) {
            await this.selectTextColor(selectedColor);
            this.color = selectedColor;
        } else {
            await this.selectBackground(selectedColor);
            this.background = selectedColor;
        }
    }

    private selectTextColor(color: string): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !color) {
                resolve();
                return;
            }

            if (this.deckOrSlide) {
                const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

                element.style.setProperty('--color', color);
            } else {
                this.selectedElement.style.color = color;
            }

            this.colorDidChange.emit(this.applyToAllDeck);

            resolve();
        });
    }

    private selectBackground(color: string): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !color) {
                resolve();
                return;
            }

            if (this.deckOrSlide) {
                const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

                element.style.setProperty('--background', color);
            } else if (this.selectedElement.parentElement && this.selectedElement.parentElement.nodeName && this.selectedElement.parentElement.nodeName.toLowerCase() === 'deckgo-slide-split') {
                const element: HTMLElement = this.selectedElement.parentElement;

                if (this.selectedElement.getAttribute('slot') === 'start') {
                    element.style.setProperty('--slide-split-background-start', color);
                } else if (this.selectedElement.getAttribute('slot') === 'end') {
                    element.style.setProperty('--slide-split-background-end', color);
                } else {
                    this.selectedElement.style.background = color;
                }
            } else {
                this.selectedElement.style.background = color;
            }

            this.colorDidChange.emit(this.applyToAllDeck);

            resolve();
        });
    }

    private async initCurrentColors(element: HTMLElement) {
        if (!element) {
            return;
        }

        this.color = await this.rgb2hex(element.style.getPropertyValue('--color') ? element.style.getPropertyValue('--color') : element.style.color);
        this.background = await this.rgb2hex(element.style.getPropertyValue('--background') ? element.style.getPropertyValue('--background') : element.style.background);
    }

    // https://css-tricks.com/converting-color-spaces-in-javascript/
    private rgb2hex(rgb: string): Promise<string> {
        return new Promise<string>((resolve) => {
            if (!rgb || rgb === undefined || rgb === '' || rgb.indexOf('rgb') <= -1) {
                resolve(rgb);
                return;
            }

            const separator: string = rgb.indexOf(",") > -1 ? "," : " ";

            const rgbs: string[] = rgb.substr(4).split(")")[0].split(separator);

            const r: string = (+rgbs[0]).toString(16);
            const g: string = (+rgbs[1]).toString(16);
            const b: string = (+rgbs[2]).toString(16);

            resolve(`#${r.length == 1 ? '0' + r : r}${g.length == 1 ? '0' + g : g}${b.length == 1 ? '0' + b : b}`);
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
            </ion-list>,
            <deckgo-color class="ion-padding" more={this.moreColors} onColorChange={($event: CustomEvent) => this.selectColor($event)} color-hex={this.applyToText ? this.color : this.background}>
                <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>
        ]
    }

}
