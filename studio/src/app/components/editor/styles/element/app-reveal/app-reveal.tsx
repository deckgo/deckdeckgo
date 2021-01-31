import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlotUtils} from '../../../../../utils/editor/slot.utils';

@Component({
  tag: 'app-reveal',
  styleUrl: 'app-reveal.scss',
})
export class AppReveal {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private reveal: boolean = false;

  @Event() toggleReveal: EventEmitter<boolean>;

  componentWillLoad() {
    this.reveal = SlotUtils.isNodeReveal(this.selectedElement) || SlotUtils.isNodeRevealList(this.selectedElement);
  }

  private toggle() {
    this.reveal = !this.reveal;
    this.toggleReveal.emit(this.reveal);
  }

  render() {
    return (
      <ion-list class="article">
        <ion-item>
          <ion-label>Animate transition</ion-label>
          <ion-checkbox slot="end" color="dark" checked={this.reveal} onIonChange={() => this.toggle()}></ion-checkbox>
        </ion-item>
      </ion-list>
    );
  }
}
