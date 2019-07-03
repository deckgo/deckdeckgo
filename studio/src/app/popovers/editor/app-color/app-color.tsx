import {Component, Element, Event, EventEmitter, h, Prop} from '@stencil/core';

@Component({
    tag: 'app-color',
    styleUrl: 'app-color.scss'
})
export class AppColor {

    @Element() el: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    @Prop()
    color: string;

    @Prop()
    background: string;

    @Prop()
    selectedElement: HTMLElement;

    @Event() colorDidChange: EventEmitter<HTMLElement>;

    private applyToAllDeck: boolean = false;

    async componentDidLoad() {
        await this.colorPickerListener(true);
        await this.backgroundPickerListener(true);
    }

    async componentDidUnload() {
        await this.colorPickerListener(false);
        await this.backgroundPickerListener(false);
    }

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async selectApplyToAllDeck($event: CustomEvent) {
        if ($event) {
            this.applyToAllDeck = $event.detail;
        }
    }

    // Color

    private colorPickerListener(bind: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            const colorPicker: HTMLInputElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            if (bind) {
                colorPicker.addEventListener('change', this.selectColor, false);
            } else {
                colorPicker.removeEventListener('change', this.selectColor, true);
            }


            resolve();
        });
    }

    private openColorPicker(): Promise<void> {
        return new Promise<void>((resolve) => {
            const colorPicker: HTMLInputElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            colorPicker.click();

            resolve();
        });
    }

    private selectColor = async ($event) => {
        if (!this.selectedElement) {
            return;
        }

        this.color = $event.target.value;

        if (this.deckOrSlide) {
            const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

            element.style.setProperty('--color', $event.target.value);
        } else {
            this.selectedElement.style.color = $event.target.value;
        }

        await this.colorDidChange.emit(this.selectedElement);
    };

    // Background

    private backgroundPickerListener(bind: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            const backgroundPicker: HTMLInputElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            if (bind) {
                backgroundPicker.addEventListener('change', this.selectBackground, false);
            } else {
                backgroundPicker.removeEventListener('change', this.selectBackground, true);
            }


            resolve();
        });
    }

    private openBackgroundPicker(): Promise<void> {
        return new Promise<void>((resolve) => {
            const backgroundPicker: HTMLInputElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            backgroundPicker.click();

            resolve();
        });
    }

    private selectBackground = async ($event) => {
        if (!this.selectedElement) {
            return;
        }

        this.background = $event.target.value;

        if (this.deckOrSlide) {
            const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

            element.style.setProperty('--background', $event.target.value);
        } else if (this.selectedElement.parentElement && this.selectedElement.parentElement.nodeName && this.selectedElement.parentElement.nodeName.toLowerCase() === 'deckgo-slide-split') {
            const element: HTMLElement = this.selectedElement.parentElement;

            if (this.selectedElement.getAttribute('slot') === 'start') {
                element.style.setProperty('--slide-split-background-start', $event.target.value);
            } else if (this.selectedElement.getAttribute('slot') === 'end') {
                element.style.setProperty('--slide-split-background-end', $event.target.value);
            } else {
                this.selectedElement.style.background = $event.target.value;
            }
        } else {
            this.selectedElement.style.background = $event.target.value;
        }

        await this.colorDidChange.emit(this.selectedElement);
    };

    render() {
        return [<ion-toolbar class="ion-margin ion-padding-end">
                <h2>Color</h2>
                <ion-router-link slot="end" onClick={() => this.closePopover()}><ion-icon name="close"></ion-icon></ion-router-link>
            </ion-toolbar>,
            <ion-list>
                <app-deck-or-slide deckOrSlide={this.deckOrSlide} onApplyTo={($event: CustomEvent) => this.selectApplyToAllDeck($event)}></app-deck-or-slide>

                <ion-item class="ion-margin-top action-button">
                    <ion-button shape="round" onClick={() => this.openColorPicker()} color="primary">
                        <ion-label class="ion-text-uppercase">Text color</ion-label>
                    </ion-button>
                </ion-item>

                <ion-item class="action-button">
                    <ion-button shape="round" onClick={() => this.openBackgroundPicker()} color="secondary">
                        <ion-label class="ion-text-uppercase">Background color</ion-label>
                    </ion-button>
                </ion-item>
            </ion-list>,
            <input type="color" name="color-picker" value={this.color}></input>,
            <input type="color" name="background-picker" value={this.background}></input>
        ]
    }

}
