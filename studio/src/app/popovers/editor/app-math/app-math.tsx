import {Component, Element, EventEmitter, h, Prop, State} from '@stencil/core';
import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-math',
  styleUrl: 'app-math.scss'
})
export class AppMath {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @Prop()
  mathDidChange: EventEmitter<HTMLElement>;

  @State()
  private macros: string | undefined;

  constructor() {}

  async componentWillLoad() {
    await this.initCurrent();
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async initCurrent(): Promise<void> {
    this.macros = this.selectedTarget ? this.selectedTarget.getAttribute('macros') : undefined;
  }

  private emitMathDidChange() {
    this.mathDidChange.emit(this.selectedTarget);
  }

  private handleMacrosInput($event: CustomEvent<KeyboardEvent>) {
    this.macros = ($event.target as InputTargetEvent).value;
  }

  private async applyMacrosInput(): Promise<void> {
    if (!this.selectedTarget) {
      return;
    }

    if (this.macros && this.macros !== '') {
      this.selectedTarget.setAttribute('macros', this.macros);
    } else {
      this.selectedTarget.removeAttribute('macros');
    }

    this.emitMathDidChange();
  }

  render() {
    return [
      <ion-toolbar>
        <h2>{i18n.state.editor.math_options}</h2>
        <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
      </ion-toolbar>,
      <ion-list class="article">
        <ion-item-divider>
          <ion-label>{i18n.state.editor.macros}</ion-label>
        </ion-item-divider>

        <ion-item class="select">
          <ion-textarea
            rows={5}
            value={this.macros}
            debounce={500}
            maxlength={254}
            placeholder="A collection of custom macros. Property with a name like \\name which maps to a string that describes the expansion."
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleMacrosInput($event)}
            onIonChange={() => this.applyMacrosInput()}></ion-textarea>
        </ion-item>
      </ion-list>
    ];
  }
}
