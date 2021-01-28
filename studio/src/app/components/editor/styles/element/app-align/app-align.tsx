import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import settingsStore from '../../../../../stores/settings.store';

import {AlignUtils, TextAlign} from '../../../../../utils/editor/align.utils';
import {SettingsUtils} from '../../../../../utils/core/settings.utils';

import {Expanded} from '../../../../../types/core/settings';

@Component({
  tag: 'app-align',
  styleUrl: 'app-align.scss',
})
export class AppAlign {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private align: TextAlign | undefined;

  @Event() alignChange: EventEmitter<void>;

  async componentWillLoad() {
    this.align = await AlignUtils.getAlignment(this.selectedElement);
  }

  private async updateAlign($event: CustomEvent): Promise<void> {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    this.selectedElement.style.textAlign = $event.detail.value;
    this.align = $event.detail.value;

    this.alignChange.emit();
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.align = ($event.target as InputTargetEvent).value as TextAlign;
  }

  private updateAlignCSS() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.textAlign = this.align;

    this.alignChange.emit();
  }

  render() {
    if (this.align === undefined) {
      return undefined;
    }

    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.align}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({align: $event.detail})}>
        <ion-label slot="title">Alignment</ion-label>
        <ion-list>
          <ion-item class="select properties">
            <ion-label>Alignment</ion-label>

            <ion-select
              value={this.align}
              placeholder="Select an alignment"
              onIonChange={($event: CustomEvent) => this.updateAlign($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={TextAlign.LEFT}>Left</ion-select-option>
              <ion-select-option value={TextAlign.CENTER}>Center</ion-select-option>
              <ion-select-option value={TextAlign.RIGHT}>Right</ion-select-option>
            </ion-select>
          </ion-item>

          <ion-item class="with-padding css">
            <ion-input
              value={this.align}
              placeholder="text-align"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={() => this.updateAlignCSS()}></ion-input>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
