import {Component, Event, EventEmitter, h, Prop, State, Fragment} from '@stencil/core';

import type {RangeChangeEventDetail} from '@ionic/core';

import settingsStore from '../../../../../../stores/settings.store';
import i18n from '../../../../../../stores/i18n.store';

import {EditMode, Expanded} from '../../../../../../types/core/settings';
import {SelectedTarget} from '../../../../../../types/editor/selected-target';

import {SettingsUtils} from '../../../../../../utils/core/settings.utils';
import {setStyle} from '../../../../../../utils/editor/undo-redo.deck.utils';

@Component({
  tag: 'app-border-radius'
})
export class AppBorderRadius {
  @Prop()
  selectedTarget: SelectedTarget;

  @State()
  private borderRadiuses: Map<string, number> = new Map([
    ['general', 0],
    ['top-left', 0],
    ['top-right', 0],
    ['bottom-left', 0],
    ['bottom-right', 0]
  ]);

  @State()
  private cornersExpanded: boolean = false;

  @State()
  private borderRadiusCSS: string;

  private readonly maxBorderRadius: number = 64;

  @Event() borderRadiusDidChange: EventEmitter<void>;

  private destroyListener;

  private ignoreUpdateStyle: boolean = false;

  async componentWillLoad() {
    await this.initBorderRadius();
    await this.initCornersExpanded();

    await this.initBorderRadiusCSS();

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
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
    this.borderRadiusCSS = this.selectedTarget?.target?.style.borderRadius;
  }

  private async initBorderRadius() {
    if (!this.selectedTarget || !this.selectedTarget.target || !window) {
      return;
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(this.selectedTarget.target);

    if (!style) {
      return;
    }

    this.borderRadiuses.set('top-left', parseInt(style.borderTopLeftRadius));
    this.borderRadiuses.set('top-right', parseInt(style.borderTopRightRadius));
    this.borderRadiuses.set('bottom-right', parseInt(style.borderBottomRightRadius));
    this.borderRadiuses.set('bottom-left', parseInt(style.borderBottomLeftRadius));
  }

  private async initCornersExpanded() {
    this.cornersExpanded = !(
      this.borderRadiuses.get('top-left') === this.borderRadiuses.get('top-right') &&
      this.borderRadiuses.get('top-left') === this.borderRadiuses.get('bottom-right') &&
      this.borderRadiuses.get('top-left') === this.borderRadiuses.get('bottom-left')
    );

    if (!this.cornersExpanded) {
      this.borderRadiuses.set('general', this.borderRadiuses.get('top-left'));
    }
  }

  private emitBorderRadiusChange() {
    this.borderRadiusDidChange.emit();
  }

  private async updateBorderRadius($event: CustomEvent, corner: string = ''): Promise<void> {
    if (!this.selectedTarget || !$event || !$event.detail) {
      return;
    }
    if (corner === 'general') {
      this.borderRadiuses.forEach((_, key) => {
        this.borderRadiuses.set(key, $event.detail.value);
      });

      this.updateStyle({property: 'border-radius', value: `${$event.detail.value}px`});
    } else {
      this.borderRadiuses.set(corner, $event.detail.value);

      this.updateStyle({property: `border-${corner}-radius`, value: `${$event.detail.value}px`});
    }

    this.updateBorderRadiuses();

    this.emitBorderRadiusChange();
  }

  // To apply a re-render
  private updateBorderRadiuses() {
    this.borderRadiuses = new Map<string, number>(this.borderRadiuses);
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
    this.selectedTarget.target.style.borderRadius = this.borderRadiusCSS;

    this.emitBorderRadiusChange();
  }

  private updateStyle({property, value}: {property: string; value: string}) {
    if (this.ignoreUpdateStyle) {
      this.ignoreUpdateStyle = false;
      return;
    }

    setStyle(this.selectedTarget.target, {
      properties: [{property, value}],
      type: this.selectedTarget.type,
      updateUI: async () => {
        // ion-change triggers the event each time its value changes, because we re-render, it triggers it again
        this.ignoreUpdateStyle = true;

        if (settingsStore.state.editMode === 'css') {
          await this.initBorderRadiusCSS();

          return;
        }

        await this.initBorderRadius();
        await this.initCornersExpanded();

        this.updateBorderRadiuses();
      }
    });
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.borderRadius}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({borderRadius: $event.detail})}
      >
        <ion-label slot="title">{i18n.state.editor.border_radius}</ion-label>
        <ion-list class="properties">
          <ion-item class="select">
            <ion-select
              value={this.cornersExpanded}
              onIonChange={($event: CustomEvent) => this.selectCornersToShow($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end"
            >
              <ion-select-option value={false}>{i18n.state.editor.all_corners}</ion-select-option>
              <ion-select-option value={true}>{i18n.state.editor.individual_corners}</ion-select-option>
            </ion-select>
          </ion-item>
          {!this.cornersExpanded ? this.renderOption('general', 'Every corner') : undefined}
          {this.cornersExpanded && (
            <Fragment>
              {this.renderOption('top-left', i18n.state.editor.top_left)}
              {this.renderOption('top-right', i18n.state.editor.top_right)}
              {this.renderOption('bottom-right', i18n.state.editor.bottom_right)}
              {this.renderOption('bottom-left', i18n.state.editor.bottom_left)}
            </Fragment>
          )}
        </ion-list>

        <ion-list class="css">
          <ion-item class="with-padding">
            <ion-input
              value={this.borderRadiusCSS}
              placeholder={i18n.state.editor.border_radius}
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={async () => await this.updateBorderRadiusCSS()}
            ></ion-input>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderOption(option: 'general' | 'top-left' | 'top-right' | 'bottom-right' | 'bottom-left', text: string) {
    const borderRadius: number = this.borderRadiuses.get(option);

    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>
          {text} <small>{borderRadius}px</small>
        </ion-label>
      </ion-item-divider>,
      <ion-item class="item-range">
        <ion-range
          color="dark"
          min={0}
          max={this.maxBorderRadius}
          value={this.borderRadiuses.get(option)}
          mode="md"
          onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, option)}
        ></ion-range>
      </ion-item>
    ];
  }
}
