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

  @State()
  private padding: number;

  @State()
  private paddingCSS: string;

  @State()
  private rotate: number;

  @State()
  private transformCSS: string;

  @Event() blockChange: EventEmitter<void>;

  private destroyListener;

  async componentWillLoad() {
    await this.initWidth();
    await this.initWidthCSS();

    await this.initPadding();
    await this.initPaddingCSS();

    await this.initRotate();
    await this.initTransformCSS();

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
      if (edit === 'css') {
        await this.initWidthCSS();
        await this.initPaddingCSS();
        await this.initTransformCSS();
        return;
      }

      await this.initWidth();
      await this.initPadding();
      await this.initRotate();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async initPadding() {
    const css: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);
    const padding: number = parseInt(css.paddingTop);
    this.padding = isNaN(padding) ? 0 : padding;
  }

  private async initPaddingCSS() {
    const css: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);
    this.paddingCSS = css.padding;
  }

  private async initWidth() {
    const width: number = parseInt(this.selectedElement?.style.width);
    this.width = isNaN(width) ? 100 : width;
  }

  private async initWidthCSS() {
    this.widthCSS = this.selectedElement?.style.width;
  }

  private async initRotate() {
    const matches: RegExpMatchArray | null = this.selectedElement?.style.transform.match(/(\d+)/);
    const rotate: number = parseInt(matches?.[0]);
    this.rotate = isNaN(rotate) ? 0 : rotate;
  }

  private async initTransformCSS() {
    this.transformCSS = this.selectedElement?.style.transform;
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

  private async updatePadding($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0) {
      return;
    }

    $event.stopPropagation();

    this.padding = $event.detail.value as number;

    this.selectedElement.style.padding = `${this.padding}px`;

    this.blockChange.emit();
  }

  private handlePaddingInput($event: CustomEvent<KeyboardEvent>) {
    this.paddingCSS = ($event.target as InputTargetEvent).value;
  }

  private updatePaddingCSS() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.padding = this.paddingCSS;

    this.blockChange.emit();
  }

  private async updateRotate($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 360) {
      return;
    }

    $event.stopPropagation();

    this.rotate = $event.detail.value as number;

    this.selectedElement.style.transform = `rotate(${this.rotate}deg)`;

    this.blockChange.emit();
  }

  private handleTransformInput($event: CustomEvent<KeyboardEvent>) {
    this.transformCSS = ($event.target as InputTargetEvent).value;
  }

  private updateTransformCSS() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.transform = this.transformCSS;

    this.blockChange.emit();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.block}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({block: $event.detail})}>
        <ion-label slot="title">Block</ion-label>
        <ion-list>
          {this.renderWidth()}
          {this.renderPadding()}
          {this.renderRotate()}
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderRotate() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top">
          <ion-label>Rotate {settingsStore.state.editMode === 'properties' ? <small>{this.rotate}deg</small> : undefined}</ion-label>
        </ion-item-divider>

        <ion-item class="item-opacity properties">
          <ion-range
            value={this.rotate}
            min={0}
            max={360}
            mode="md"
            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateRotate($event)}></ion-range>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.transformCSS}
            placeholder="transform"
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleTransformInput(e)}
            onIonChange={() => this.updateTransformCSS()}></ion-input>
        </ion-item>
      </Fragment>
    );
  }

  private renderPadding() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top">
          <ion-label>Padding {settingsStore.state.editMode === 'properties' ? <small>{this.padding}px</small> : undefined}</ion-label>
        </ion-item-divider>

        <ion-item class="item-opacity properties">
          <ion-range value={this.padding} mode="md" onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updatePadding($event)}></ion-range>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.paddingCSS}
            placeholder="padding"
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handlePaddingInput(e)}
            onIonChange={() => this.updatePaddingCSS()}></ion-input>
        </ion-item>
      </Fragment>
    );
  }

  private renderWidth() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top">
          <ion-label>Width {settingsStore.state.editMode === 'properties' ? <small>{this.width}%</small> : undefined}</ion-label>
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
