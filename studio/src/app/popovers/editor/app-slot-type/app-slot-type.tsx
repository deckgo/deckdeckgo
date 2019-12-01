import {Component, Element, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../utils/editor/slot-type';
import {SlotUtils} from '../../../utils/editor/slot.utils';

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

    @State()
    private onlyTextTypes: boolean = false;

    async componentWillLoad() {
        if (this.selectedElement) {
            if (SlotUtils.isNodeRevealList(this.selectedElement)) {
                await this.initCurrentTypeList();
            } else {
                await this.initCurrentType();
            }
        }

        this.onlyTextTypes = this.selectedElement && this.selectedElement.parentElement && this.selectedElement.parentElement.nodeName && this.selectedElement.parentElement.nodeName.toLowerCase() === 'deckgo-slide-poll';
    }

    private initCurrentType(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const element: HTMLElement = SlotUtils.isNodeReveal(this.selectedElement) ? this.selectedElement.firstElementChild as HTMLElement : this.selectedElement;

            if (element.nodeName && element.nodeName !== '') {
                this.currentType = await this.initSlotType(element.nodeName.toLowerCase());
            }

            resolve();
        });
    }

    private initCurrentTypeList(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.currentType = this.selectedElement.getAttribute('list-tag') === SlotType.UL ? SlotType.UL : SlotType.OL;

            resolve();
        });
    }

    private initSlotType(type: string): Promise<SlotType> {
        return new Promise<SlotType>((resolve) => {
            const templateKey: string = Object.keys(SlotType).find((key: string) => {
                return type === SlotType[key];
            });

            resolve(SlotType[templateKey]);
        });
    }

    private async closePopover(type?: SlotType) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            type: this.currentType !== type ? type : null
        });
    }

    render() {
        return [<ion-toolbar>
                <h2>Toggle section</h2>
                <ion-router-link slot="end" onClick={() => this.closePopover()}><ion-icon name="close"></ion-icon></ion-router-link>
            </ion-toolbar>,

            <ion-list>
                <a onClick={() => this.closePopover(SlotType.H1)} class={this.currentType === SlotType.H1 ? "current" : ""}><ion-item><h1>Huge title</h1></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.H2)} class={this.currentType === SlotType.H2 ? "current" : ""}><ion-item><h2>Large title</h2></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.H3)} class={this.currentType === SlotType.H3 ? "current" : ""}><ion-item><h3>Small title</h3></ion-item></a>
                <a onClick={() => this.closePopover(SlotType.SECTION)} class={this.currentType === SlotType.SECTION ? "current" : ""}><ion-item><p>Paragraph</p></ion-item></a>
                {this.renderComplexTypes()}
            </ion-list>
        ]
    }

    private renderComplexTypes() {
        if (this.onlyTextTypes) {
            return undefined;
        }

        return [
            <a onClick={() => this.closePopover(SlotType.OL)} class={this.currentType === SlotType.OL ? "current" : ""}><ion-item><p>List</p></ion-item></a>,
            <a onClick={() => this.closePopover(SlotType.IMG)} class={this.currentType === SlotType.IMG ? "current" : ""}><ion-item><p>Image</p></ion-item></a>,
            <a onClick={() => this.closePopover(SlotType.CODE)} class={this.currentType === SlotType.CODE ? "current" : ""}><ion-item><p>Code</p></ion-item></a>
        ]
    }
}
