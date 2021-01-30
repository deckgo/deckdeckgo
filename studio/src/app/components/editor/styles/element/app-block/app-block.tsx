import {Component, Event, EventEmitter, Fragment, h, Prop, State} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import settingsStore from '../../../../../stores/settings.store';

import {SettingsUtils} from '../../../../../utils/core/settings.utils';

import {EditMode, Expanded} from '../../../../../types/core/settings';

@Component({
  tag: 'app-block',
})
export class AppBlock {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private width: number = 100;

  @State()
  private widthCSS: string;

  @Event() blockChange: EventEmitter<void>;

  private destroyListener;

  async componentWillLoad() {
    await this.initWidth();
    this.widthCSS = this.selectedElement?.style.width;

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
      if (edit === 'css') {
        this.widthCSS = this.selectedElement?.style.width;
        return;
      }

      await this.initWidth();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async initWidth() {
    const width: number = parseInt(this.selectedElement?.style.width);
    this.width = isNaN(width) ? 100 : width;
  }

  private async updateWidth($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    this.width = $event.detail.value as number;

    this.selectedElement.style.width = `${this.width}%`;

    this.blockChange.emit();
  }

  private handleWidthInput($event: CustomEvent<KeyboardEvent>) {
    this.widthCSS = ($event.target as InputTargetEvent).value;
  }

  private updateWidthCSS() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.width = this.widthCSS;

    this.blockChange.emit();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.align}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({align: $event.detail})}>
        <ion-label slot="title">Block</ion-label>
        <ion-list>{this.renderWidth()}</ion-list>
      </app-expansion-panel>
    );
  }

  private renderWidth() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top properties">
          <ion-label>
            Width <small>{this.width}%</small>
          </ion-label>
        </ion-item-divider>
        <ion-item class="item-opacity properties">
          <ion-range
            min={1}
            max={100}
            value={this.width}
            mode="md"
            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateWidth($event)}></ion-range>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.widthCSS}
            placeholder="width"
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleWidthInput(e)}
            onIonChange={() => this.updateWidthCSS()}></ion-input>
        </ion-item>
      </Fragment>
    );
  }
}
