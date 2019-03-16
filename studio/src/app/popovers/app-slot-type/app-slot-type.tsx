import {Component, Element, Prop, State} from '@stencil/core';

import {DeckdeckgoSlotType} from '../../utils/deckdeckgo-slot-type';

@Component({
    tag: 'app-slot-type',
    styleUrl: 'app-slot-type.scss'
})
export class AppSlideAdd {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @State()
    private currentType: DeckdeckgoSlotType;

    componentWillLoad() {
        if (this.selectedElement && this.selectedElement.nodeName && this.selectedElement.nodeName !== '') {
            this.currentType = DeckdeckgoSlotType[this.selectedElement.nodeName];
        }
    }

    async closePopover(type: DeckdeckgoSlotType) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            type: type
        });
    }

    render() {
        return <div padding>
            <a onClick={() => this.closePopover(DeckdeckgoSlotType.H1)} class={this.currentType === DeckdeckgoSlotType.H1 ? "current" : ""}><h1>Title</h1></a>
            <a onClick={() => this.closePopover(DeckdeckgoSlotType.H2)} class={this.currentType === DeckdeckgoSlotType.H2 ? "current" : ""}><h2>Title</h2></a>
            <a onClick={() => this.closePopover(DeckdeckgoSlotType.H3)} class={this.currentType === DeckdeckgoSlotType.H3 ? "current" : ""}><h3>Title</h3></a>
            <a onClick={() => this.closePopover(DeckdeckgoSlotType.P)} class={this.currentType === DeckdeckgoSlotType.P ? "current" : ""}><p>Paragraph</p></a>
        </div>
    }
}
