import { Component, Event, EventEmitter, h, Prop, State } from '@stencil/core';

import { RangeChangeEventDetail } from '@ionic/core';

@Component({
    tag: 'app-border-radius',
    styleUrl: 'app-align.scss',
})
export class AppAlign {
    @Prop()
    selectedElement: HTMLElement;

    @State()
    private borderRadius: number = 0;

    @State()
    private maxBorderRadius: number = 0;

    @Event() borderRadiusChange: EventEmitter<void>;

    async componentWillLoad() {
        if (this.selectedElement) {
            this.maxBorderRadius = this.selectedElement.offsetHeight / 2;
        }
    }

    private async updateBorderRadius($event: CustomEvent): Promise<void> {
        if (!this.selectedElement || !$event || !$event.detail) {
            return;
        }

        this.selectedElement.style.borderRadius = `${$event.detail.value}px`;
        this.borderRadius = $event.detail.value;

        this.borderRadiusChange.emit();
    }

    render() {
        if (this.borderRadius === undefined) {
            return undefined;
        }

        return (
            <app-expansion-panel>
                <ion-label slot="title">Border radius</ion-label>
                <ion-list>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>
                            Border radius  <small>{this.borderRadius}px</small>
                        </ion-label>
                    </ion-item-divider>
                    <ion-item class="item-opacity">
                        <ion-range
                            color="primary"
                            min={0}
                            max={this.maxBorderRadius}
                            value={this.borderRadius}
                            mode="md"
                            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event)}></ion-range>
                    </ion-item>
                </ion-list>
            </app-expansion-panel>
        );
    }
}
