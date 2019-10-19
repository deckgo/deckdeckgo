import {Component, Element, Event, EventEmitter, h, Method, Prop, State} from '@stencil/core';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';

enum ApplyColorAxisType {
    TEXT,
    AXIS,
    GRID
}

@Component({
    tag: 'app-color-chart'
})
export class AppColorDeckSlide {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    moreColors: boolean = true;

    @State()
    private applyColorAxisType: ApplyColorAxisType = ApplyColorAxisType.TEXT;

    @State()
    private colorAxis: string;

    @Event() colorChange: EventEmitter<boolean>;

    async componentWillLoad() {
        await this.initCurrentColors();
    }

    @Method()
    async initCurrentColors() {
        if (!this.selectedElement) {
            return;
        }

        const element: HTMLElement = this.selectedElement;

        if (!element) {
            return;
        }

        await this.initColorAxis(element);
    }

    private async initColorAxis(element: HTMLElement) {
        let styleColor: InitStyleColor;

        if (this.applyColorAxisType === ApplyColorAxisType.AXIS) {
            styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-chart-axis-color'));
        } else {
            styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-chart-text-color'));
        }

        this.colorAxis = styleColor.rgb;
    }

    private toggleFontSize($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            this.applyColorAxisType = $event.detail.value;
        });
    }

    private async selectColorAxis($event: CustomEvent) {
        if (!this.selectedElement || !$event || !$event.detail) {
            return;
        }

        this.colorAxis = $event.detail.rgb;

        await this.applyColor();
    }

    private applyColor(): Promise<void> {
        return new Promise<void>((resolve) => {

            if (!this.selectedElement || !this.colorAxis) {
                resolve();
                return;
            }

            const selectedColor: string = `rgba(${this.colorAxis},1)`;

            if (this.applyColorAxisType === ApplyColorAxisType.AXIS) {
                this.selectedElement.style.setProperty('--deckgo-chart-axis-color', selectedColor);
            } else if (this.applyColorAxisType === ApplyColorAxisType.GRID) {
                this.selectedElement.style.setProperty('--deckgo-chart-grid-stroke', selectedColor);
            } else {
                this.selectedElement.style.setProperty('--deckgo-chart-text-color', selectedColor);
            }

            this.colorChange.emit(false);

            resolve();
        });
    }

    render() {
        return [
            <ion-item-divider class="ion-padding-top">
                <ion-label>Apply color to</ion-label>
            </ion-item-divider>,

            <ion-item class="select">
                <ion-label>Apply color to</ion-label>

                <ion-select value={this.applyColorAxisType} placeholder="Apply color to"
                            onIonChange={(e: CustomEvent) => this.toggleFontSize(e)}
                            class="ion-padding-start ion-padding-end">
                    <ion-select-option value={ApplyColorAxisType.TEXT}>Text</ion-select-option>
                    <ion-select-option value={ApplyColorAxisType.AXIS}>Axis</ion-select-option>
                    <ion-select-option value={ApplyColorAxisType.GRID}>Grid</ion-select-option>
                </ion-select>
            </ion-item>,

            <deckgo-color class="ion-padding-start ion-padding-end ion-padding-bottom" more={this.moreColors}
                          onColorChange={($event: CustomEvent) => this.selectColorAxis($event)} color-rgb={this.colorAxis}>
                <ion-icon name="more" ios="md-mode" md="md-more" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>
        ]
    }
}
