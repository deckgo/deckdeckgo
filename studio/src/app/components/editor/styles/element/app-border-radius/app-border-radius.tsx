import {Component, Event, EventEmitter, h, Prop, State, Fragment} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import settingsStore from '../../../../../stores/settings.store';

import {SettingsUtils} from '../../../../../utils/core/settings.utils';

import {EditMode, Expanded} from '../../../../../types/core/settings';

@Component({
  tag: 'app-border-radius',
})
export class AppBorderRadius {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private borderRadiuses: Map<string, number> = new Map([
    ['General', 0],
    ['TopLeft', 0],
    ['TopRight', 0],
    ['BottomLeft', 0],
    ['BottomRight', 0],
  ]);

  @State()
  private cornersExpanded: boolean = false;

  @State()
  private borderRadiusCSS: string;

  private readonly maxBorderRadius: number = 64;

  @Event() borderRadiusDidChange: EventEmitter<void>;

  private destroyListener;

  async componentWillLoad() {
    await this.initBorderRadius();
    await this.initCornersExpanded();

    await this.initBorderRadiusCSS();

    this.destroyListener = settingsStore.onChange('edit', async (edit: EditMode) => {
      if (edit === 'css') {
        await this.initBorderRadiusCSS();
        return;
      }

      await this.initBorderRadius();
      await this.initCornersExpanded();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async initBorderRadiusCSS() {
    this.borderRadiusCSS = this.selectedElement?.style.borderRadius;
  }

  private async initBorderRadius() {
    if (!this.selectedElement || !window) {
      return;
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);

    if (!style) {
      return;
    }

    this.borderRadiuses.set('TopLeft', parseInt(style.borderTopLeftRadius));
    this.borderRadiuses.set('TopRight', parseInt(style.borderTopRightRadius));
    this.borderRadiuses.set('BottomRight', parseInt(style.borderBottomRightRadius));
    this.borderRadiuses.set('BottomLeft', parseInt(style.borderBottomLeftRadius));
  }

  private async initCornersExpanded() {
    this.cornersExpanded = !(
      this.borderRadiuses.get('TopLeft') === this.borderRadiuses.get('TopRight') &&
      this.borderRadiuses.get('TopLeft') === this.borderRadiuses.get('BottomRight') &&
      this.borderRadiuses.get('TopLeft') === this.borderRadiuses.get('BottomLeft')
    );

    if (!this.cornersExpanded) {
      this.borderRadiuses.set('General', this.borderRadiuses.get('TopLeft'));
    }
  }

  private emitBorderRadiusChange() {
    this.borderRadiusDidChange.emit();
  }

  private async updateBorderRadius($event: CustomEvent, corner: string = ''): Promise<void> {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }
    if (corner === 'General') {
      this.borderRadiuses.forEach((_, key) => {
        this.borderRadiuses.set(key, $event.detail.value);
      });
      this.selectedElement.style.borderRadius = `${$event.detail.value}px`;
    } else {
      this.borderRadiuses.set(corner, $event.detail.value);
      this.selectedElement.style[`border${corner}Radius`] = `${$event.detail.value}px`;
    }
    this.borderRadiuses = new Map<string, number>(this.borderRadiuses);

    this.emitBorderRadiusChange();
  }

  private selectCornersToShow($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }
    this.cornersExpanded = $event.detail.value;
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.borderRadiusCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateBorderRadiusCSS() {
    this.selectedElement.style.borderRadius = this.borderRadiusCSS;

    this.emitBorderRadiusChange();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.borderRadius}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({borderRadius: $event.detail})}>
        <ion-label slot="title">Border radius</ion-label>
        <ion-item class="select properties">
          <ion-select
            value={this.cornersExpanded}
            onIonChange={($event: CustomEvent) => this.selectCornersToShow($event)}
            interface="popover"
            mode="md"
            class="ion-padding-start ion-padding-end">
            <ion-select-option value={false}>All corners</ion-select-option>
            <ion-select-option value={true}>Individual corners</ion-select-option>
          </ion-select>
        </ion-item>
        <ion-list class="properties">
          {!this.cornersExpanded ? this.renderOption('General', 'Every corner') : undefined}
          {this.cornersExpanded && (
            <Fragment>
              {this.renderOption('TopLeft', 'Top left')}
              {this.renderOption('TopRight', 'Top right')}
              {this.renderOption('BottomRight', 'Bottom right')}
              {this.renderOption('BottomLeft', 'Bottom left')}
            </Fragment>
          )}
        </ion-list>

        <ion-list class="css">
          <ion-item class="with-padding">
            <ion-input
              value={this.borderRadiusCSS}
              placeholder="border-radius"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={async () => await this.updateBorderRadiusCSS()}></ion-input>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderOption(option: 'General' | 'TopLeft' | 'TopRight' | 'BottomRight' | 'BottomLeft', text: string) {
    const borderRadius: number = this.borderRadiuses.get(option);

    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>
          {text} <small>{borderRadius}px</small>
        </ion-label>
      </ion-item-divider>,
      <ion-item class="item-opacity">
        <ion-range
          color="primary"
          min={0}
          max={this.maxBorderRadius}
          value={this.borderRadiuses.get(option)}
          mode="md"
          onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, option)}></ion-range>
      </ion-item>,
    ];
  }
}
