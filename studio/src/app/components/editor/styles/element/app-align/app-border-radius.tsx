import { Component, Event, EventEmitter, h, Prop, State } from '@stencil/core';

import { RangeChangeEventDetail } from '@ionic/core';

@Component({
    tag: 'app-border-radius',
    styleUrl: 'app-align.scss',
})
export class BorderRadius {
    @Prop()
    selectedElement: HTMLElement;

    @State()
    private borderRadiuses: Map<string, number> = new Map(
        [
            ['General', 0],
            ['TopLeft', 0],
            ['TopRight', 0],
            ['BottomLeft', 0],
            ['BottomRight', 0],
        ]
    );

    @State()
    private maxBorderRadius: number = 0;

    @Event() borderRadiusChange: EventEmitter<void>;

    async componentWillLoad() {
        if (this.selectedElement) {
            this.maxBorderRadius = this.selectedElement.offsetHeight / 2;
        }
    }

    private async updateBorderRadius($event: CustomEvent, corner: string = ''): Promise<void> {
        if (!this.selectedElement || !$event || !$event.detail) {
            return;
        }

        this.borderRadiuses.set(corner, $event.detail.value);
        if (corner === 'General') {
            this.selectedElement.style.borderRadius = `${$event.detail.value}px`;
        } else {
            this.selectedElement.style[`border${corner}Radius`] = `${$event.detail.value}px`;
        }

        this.borderRadiusChange.emit();
    }

    render() {
        return (
            <app-expansion-panel>
                <ion-label slot="title">Border radius</ion-label>
                <ion-list>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>
                            Border radius <small>{this.borderRadiuses.get('General')}px</small>
                        </ion-label>
                    </ion-item-divider>
                    <ion-item class="item-opacity">
                        <ion-range
                            color="primary"
                            min={0}
                            max={this.maxBorderRadius}
                            value={this.borderRadiuses.get('General')}
                            mode="md"
                            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, 'General')}></ion-range>
                    </ion-item>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>
                            Top left <small>{this.borderRadiuses.get('TopLeft')}px</small>
                        </ion-label>
                    </ion-item-divider>
                    <ion-item class="item-opacity">
                        <ion-label slot="title">Top left</ion-label>
                        <ion-range
                            color="primary"
                            min={0}
                            max={this.maxBorderRadius}
                            value={this.borderRadiuses.get('TopLeft')}
                            mode="md"
                            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, 'TopLeft')}></ion-range>
                    </ion-item>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>
                            Top right <small>{this.borderRadiuses.get('TopRight')}px</small>
                        </ion-label>
                    </ion-item-divider>
                    <ion-item class="item-opacity">
                        <ion-label slot="title">Top right</ion-label>
                        <ion-range
                            color="primary"
                            min={0}
                            max={this.maxBorderRadius}
                            value={this.borderRadiuses.get('TopRight')}
                            mode="md"
                            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, 'TopRight')}></ion-range>
                    </ion-item>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>
                            Bottom left <small>{this.borderRadiuses.get('BottomLeft')}px</small>
                        </ion-label>
                    </ion-item-divider>
                    <ion-item class="item-opacity">
                        <ion-label slot="title">Bottom left</ion-label>
                        <ion-range
                            color="primary"
                            min={0}
                            max={this.maxBorderRadius}
                            value={this.borderRadiuses.get('BottomLeft')}
                            mode="md"
                            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, 'BottomLeft')}></ion-range>
                    </ion-item>
                    <ion-item-divider class="ion-padding-top">
                        <ion-label>
                            Bottom right <small>{this.borderRadiuses.get('BottomRight')}px</small>
                        </ion-label>
                    </ion-item-divider>
                    <ion-item class="item-opacity">
                        <ion-label slot="title">Bottom right</ion-label>
                        <ion-range
                            color="primary"
                            min={0}
                            max={this.maxBorderRadius}
                            value={this.borderRadiuses.get('BottomRight')}
                            mode="md"
                            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, 'BottomRight')}></ion-range>
                    </ion-item>
                </ion-list>
            </app-expansion-panel>
        );
    }
}
