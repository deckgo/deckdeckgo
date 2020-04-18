import {Component, Element, h, Prop, State} from '@stencil/core';

import {SlotUtils} from '../../../utils/editor/slot.utils';

@Component({
  tag: 'app-reveal',
  styleUrl: 'app-reveal.scss',
})
export class AppReveal {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private currentReveal: boolean = false;

  componentWillLoad() {
    this.currentReveal = SlotUtils.isNodeReveal(this.selectedElement) || SlotUtils.isNodeRevealList(this.selectedElement);
  }

  private async closePopover(reveal: boolean) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      reveal: reveal,
    });
  }

  render() {
    return (
      <ion-list>
        <ion-item>
          <ion-label>Animate transition</ion-label>
          <ion-checkbox slot="end" checked={this.currentReveal} onIonChange={() => this.closePopover(!this.currentReveal)}></ion-checkbox>
        </ion-item>
      </ion-list>
    );
  }
}
