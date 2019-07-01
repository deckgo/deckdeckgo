import {Component, Element, Prop, State, h} from '@stencil/core';

import {SlotType} from '../../../utils/editor/create-slides.utils';

@Component({
    tag: 'app-slot-type',
    styleUrl: 'app-slot-type.scss'
})
export class AppSlideAdd {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @State()
    private currentType: SlotType;

    componentWillLoad() {
        if (this.selectedElement && this.selectedElement.nodeName && this.selectedElement.nodeName !== '') {
            this.currentType = this.selectedElement.nodeName && this.selectedElement.nodeName.toLowerCase() === SlotType.CODE ? SlotType.CODE : SlotType[this.selectedElement.nodeName];
        }
    }

    private async closePopover(type?: SlotType) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            type: this.currentType !== type ? type : null
        });
    }

    render() {
        return [<ion-toolbar class="ion-margin ion-padding-end">
                <h2>Toggle section</h2>
                <ion-anchor slot="end" onClick={() => this.closePopover()}><ion-icon name="close"></ion-icon></ion-anchor>
            </ion-toolbar>,

            <ion-list>
                <a onClick={() => this.closePopover(SlotType.H1)} class={this.currentType === SlotType.H1 ? "current" : ""}><ion-item><h1>Huge title</h1></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.H2)} class={this.currentType === SlotType.H2 ? "current" : ""}><ion-item><h2>Large title</h2></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.H3)} class={this.currentType === SlotType.H3 ? "current" : ""}><ion-item><h3>Small title</h3></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.SECTION)} class={this.currentType === SlotType.SECTION ? "current" : ""}><ion-item><p>Paragraph</p></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.CODE)} class={this.currentType === SlotType.CODE ? "current" : ""}><ion-item><p>Code</p></ion-item></a>
            </ion-list>
        ]
    }
}
