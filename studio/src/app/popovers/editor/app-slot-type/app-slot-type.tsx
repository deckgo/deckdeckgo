import {Component, Element, Prop, State} from '@stencil/core';

import {SlotType} from '../../../utils/editor-utils';

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
            this.currentType = SlotType[this.selectedElement.nodeName];
        }
    }

    async closePopover(type: SlotType) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            type: type
        });
    }

    render() {
        return <div padding>
            <a onClick={() => this.closePopover(SlotType.H1)} class={this.currentType === SlotType.H1 ? "current" : ""}><h1>Title</h1></a>
            <a onClick={() => this.closePopover(SlotType.H2)} class={this.currentType === SlotType.H2 ? "current" : ""}><h2>Title</h2></a>
            <a onClick={() => this.closePopover(SlotType.H3)} class={this.currentType === SlotType.H3 ? "current" : ""}><h3>Title</h3></a>
            <a onClick={() => this.closePopover(SlotType.DIV)} class={this.currentType === SlotType.DIV ? "current" : ""}><p>Paragraph</p></a>
        </div>
    }
}
