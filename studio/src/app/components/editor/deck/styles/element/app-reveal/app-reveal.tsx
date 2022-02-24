import {SlotUtils} from '@deckdeckgo/studio';
import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';
import i18n from '../../../../../../stores/i18n.store';

@Component({
  tag: 'app-reveal',
  styleUrl: 'app-reveal.scss'
})
export class AppReveal {
  @Prop()
  selectedTarget: HTMLElement;

  @State()
  private reveal: boolean = false;

  @Event() toggleReveal: EventEmitter<boolean>;

  componentWillLoad() {
    this.reveal = SlotUtils.isNodeReveal(this.selectedTarget) || SlotUtils.isNodeRevealList(this.selectedTarget);
  }

  private toggle() {
    this.reveal = !this.reveal;
    this.toggleReveal.emit(this.reveal);
  }

  render() {
    return (
      <ion-list class="article">
        <ion-item>
          <ion-label>{i18n.state.editor.animate_transition}</ion-label>
          <ion-checkbox slot="end" color="dark" checked={this.reveal} onIonChange={() => this.toggle()}></ion-checkbox>
        </ion-item>
      </ion-list>
    );
  }
}
