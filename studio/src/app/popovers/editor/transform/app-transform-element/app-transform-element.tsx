import {Component, Element, h, Prop, State} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {SlotType} from '../../../../types/editor/slot-type';

import {SlideScope} from '../../../../models/data/slide';
import {Template, TemplateDataSlot} from '../../../../models/data/template';

import {SlideUtils} from '../../../../utils/editor/slide.utils';
import {TemplateUtils} from '../../../../utils/editor/template.utils';

@Component({
  tag: 'app-transform-element',
  styleUrl: 'app-transform-element.scss'
})
export class AppTransformElement {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private slotTypes: SlotType[] | undefined;

  async componentWillLoad() {
    const slotName: string | null = this.selectedElement.getAttribute('slot');

    if (!slotName) {
      this.slotTypes = undefined;
      return;
    }

    const scope: SlideScope = SlideUtils.slideScope(this.selectedElement.parentElement);

    const template: Template | undefined = await TemplateUtils.getTemplate(scope, this.selectedElement.parentElement?.nodeName.toLowerCase());

    if (!template) {
      this.slotTypes = undefined;
      return;
    }

    const slot: TemplateDataSlot | undefined = template.data.slots?.find((slot: TemplateDataSlot) => slot.name === slotName);
    this.slotTypes = slot?.types as SlotType[];
  }

  private async closePopover(type?: SlotType) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      type: type
    });
  }

  render() {
    return [
      <ion-toolbar>
        <h2>{i18n.state.editor.transform_element}</h2>
        <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
      </ion-toolbar>,

      <app-slot-type
        selectedElement={this.selectedElement}
        slotTypes={this.slotTypes}
        onSelectType={($event: CustomEvent<SlotType>) => this.closePopover($event.detail)}></app-slot-type>
    ];
  }
}
