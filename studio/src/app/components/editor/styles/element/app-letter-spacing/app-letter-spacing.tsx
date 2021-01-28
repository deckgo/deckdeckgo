import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import settingsStore from '../../../../../stores/settings.store';

import {SettingsUtils} from '../../../../../utils/core/settings.utils';

enum LetterSpacing {
  TIGHTER,
  TIGHT,
  NORMAL,
  WIDE,
  WIDER,
  SUPERWIDE,
  WIDEST,
}

@Component({
  tag: 'app-letter-spacing',
})
export class AppLetterSpacing {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private letterSpacing: LetterSpacing = LetterSpacing.NORMAL;

  @Event() letterSpacingDidChange: EventEmitter<void>;

  async componentWillLoad() {
    this.letterSpacing = await this.initLetterSpacing();
  }

  private async initLetterSpacing(): Promise<LetterSpacing> {
    if (!this.selectedElement) {
      return LetterSpacing.NORMAL;
    }

    const spacing: string = this.selectedElement.style.letterSpacing;

    if (!spacing || spacing === '') {
      return LetterSpacing.NORMAL;
    }

    if (spacing === '-0.1em') {
      return LetterSpacing.TIGHTER;
    } else if (spacing === '-0.05em') {
      return LetterSpacing.TIGHT;
    } else if (spacing === '0.1em') {
      return LetterSpacing.WIDE;
    } else if (spacing === '0.2em') {
      return LetterSpacing.WIDER;
    } else if (spacing === '0.3em') {
      return LetterSpacing.SUPERWIDE;
    } else if (spacing === '0.4em') {
      return LetterSpacing.WIDEST;
    }

    return LetterSpacing.NORMAL;
  }

  private emitLetterSpacingChange() {
    this.letterSpacingDidChange.emit();
  }

  private async updateLetterSpacing($event: CustomEvent): Promise<void> {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    let letterSpacingConverted = '';
    switch ($event.detail.value) {
      case LetterSpacing.TIGHTER:
        letterSpacingConverted = '-0.1em';
        break;
      case LetterSpacing.TIGHT:
        letterSpacingConverted = '-0.05em';
        break;
      case LetterSpacing.WIDE:
        letterSpacingConverted = '0.1em';
        break;
      case LetterSpacing.WIDER:
        letterSpacingConverted = '0.2em';
        break;
      case LetterSpacing.SUPERWIDE:
        letterSpacingConverted = '0.3em';
        break;
      case LetterSpacing.WIDEST:
        letterSpacingConverted = '0.4em';
        break;
      default:
        letterSpacingConverted = 'normal';
    }
    this.letterSpacing = $event.detail.value;
    this.selectedElement.style.letterSpacing = letterSpacingConverted;

    this.emitLetterSpacingChange();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.letterSpacing}
        onExpansion={($event: CustomEvent<'open' | 'close'>) => SettingsUtils.update({letterSpacing: $event.detail})}>
        <ion-label slot="title">Letter spacing</ion-label>
        <ion-list>
          <ion-item class="select">
            <ion-label>Letter spacing</ion-label>
            <ion-select
              value={this.letterSpacing}
              placeholder="Select letter spacing option"
              onIonChange={($event: CustomEvent) => this.updateLetterSpacing($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={LetterSpacing.TIGHTER}>Tighter</ion-select-option>
              <ion-select-option value={LetterSpacing.TIGHT}>Tight</ion-select-option>
              <ion-select-option value={LetterSpacing.NORMAL}>Normal</ion-select-option>
              <ion-select-option value={LetterSpacing.WIDE}>Wide</ion-select-option>
              <ion-select-option value={LetterSpacing.WIDER}>Wider</ion-select-option>
              <ion-select-option value={LetterSpacing.SUPERWIDE}>Superwide</ion-select-option>
              <ion-select-option value={LetterSpacing.WIDEST}>Widest</ion-select-option>
            </ion-select>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
