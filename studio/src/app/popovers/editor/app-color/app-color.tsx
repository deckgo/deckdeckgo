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

    @Event() colorDidChange: EventEmitter<boolean>;

    private applyToAllDeck: boolean = false;

    private applyToText: boolean = true; // true = text, false = background

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async selectApplyToAllDeck($event: CustomEvent) {
        if ($event) {
            this.applyToAllDeck = $event.detail;
        }
    }

    private selectApplyToText($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToText = $event.detail.value;
        }
    }

    private selectColor = async ($event) => {
        if (!this.selectedElement || !$event || !$event.detail) {
            return;
        }

        const selectedColor: string = $event.detail;

        if (this.applyToText) {
            await this.selectTextColor(selectedColor);
        } else {
            await this.selectBackground(selectedColor);
        }
    };

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
            <deckgo-color class="ion-padding" onSelected={($event) => this.selectColor($event)}>
                <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>
        ]
    }

}
