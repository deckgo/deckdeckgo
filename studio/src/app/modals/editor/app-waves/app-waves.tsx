import {Component, Element, Listen, State, h, Fragment} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import paletteStore from '../../../stores/palette.store';

import {ColorUtils} from '../../../utils/editor/color.utils';
import {PaletteUtils} from '../../../utils/editor/palette.utils';
import {WavesUtils} from '../../../utils/editor/waves.utils';

@Component({
  tag: 'app-waves',
  styleUrl: 'app-waves.scss',
})
export class AppWaves {
  @Element() el: HTMLElement;

  @State()
  private color: string = '56, 128, 255'; // quinary color

  @State()
  private nodes: number = 8;

  @State()
  private coordinates: [string, string][] = [];

  @State()
  private orientation: WavesOrientation = 'upward';

  @State()
  private waves: Waves | null = null;

  async componentWillLoad() {
    await this.generateWaves();
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private updateOpacity($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    const opacity: string = String(($event.detail.value as number).toFixed(2));
    this.waves = {...this.waves, opacity};
  }

  private async selectColor($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    $event.stopPropagation();

    await PaletteUtils.updatePalette($event.detail);

    this.color = $event.detail.rgb;
    this.waves = {...this.waves, fill: $event.detail.hex};
  }

  private updateNodes($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 2 || $event.detail.value > 20) {
      return;
    }

    $event.stopPropagation();

    this.nodes = $event.detail.value as number;
    this.generateWaves();
  }

  private setOrientation(orientation: WavesOrientation) {
    this.orientation = orientation;
    this.generateWaves(true);
  }

  private async generateWaves(mirror: boolean = false) {
    if (!mirror) {
      // only generate new coordinates when isn't mirroring
      this.coordinates = WavesUtils.generateCoordinates(this.nodes);
    }
    const fill = ColorUtils.rgbaToHex(`rgba(${this.color}, 1)`);
    const fullPath: string = WavesUtils.getFullPath(this.nodes, this.orientation, this.coordinates);
    this.waves = {
      width: '100%',
      viewBox: '0 0 500 150',
      preserveAspectRatio: 'none',
      fill,
      opacity: this.waves ? this.waves.opacity : '1',
      style: {alignSelf: this.orientation === 'upward' ? 'flex-end' : 'flex-start', verticalAlign: 'middle'},
      path: {d: fullPath},
    };
  }

  private selectWave(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const svg: Waves = this.waves;

      await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(svg);

      resolve();
    });
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="quinary">
            <ion-buttons slot="start">{this.renderCloseButton()}</ion-buttons>
            <ion-title class="ion-text-uppercase">Waves</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding">
          <div class="container ion-margin-bottom">
            <svg {...this.waves}>
              <path d={this.waves.path.d} />
            </svg>
          </div>

          <app-expansion-panel>
            <ion-label slot="title">Waves</ion-label>
            {this.renderOptions()}
          </app-expansion-panel>

          <app-expansion-panel>
            <ion-label slot="title">Color</ion-label>
            {this.renderColor()}
          </app-expansion-panel>

          <ion-button onClick={() => this.selectWave()} color="quinary">
            Add
          </ion-button>
        </ion-content>
      </Fragment>
    );
  }

  private renderCloseButton() {
    return (
      <ion-button onClick={() => this.closeModal()}>
        <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
      </ion-button>
    );
  }

  private renderOptions() {
    return (
      <div class="options">
        <ion-fab-button size="small" onClick={() => this.generateWaves()} color="quinary" aria-label="Randomize">
          <ion-icon name="shuffle-outline"></ion-icon>
        </ion-fab-button>

        <ion-fab-button
          size="small"
          onClick={() => this.setOrientation('upward')}
          color={this.orientation === 'upward' ? 'quinary' : 'medium'}
          aria-label="Direction up">
          <ion-icon name="chevron-up"></ion-icon>
        </ion-fab-button>

        <ion-fab-button
          size="small"
          onClick={() => this.setOrientation('downward')}
          color={this.orientation === 'downward' ? 'quinary' : 'medium'}
          aria-label="Direction down">
          <ion-icon name="chevron-down"></ion-icon>
        </ion-fab-button>

        <ion-range
          color="primary"
          min={2}
          max={20}
          step={2}
          value={this.nodes}
          mode="md"
          onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateNodes(e)}></ion-range>
      </div>
    );
  }

  private renderColor() {
    return (
      <ion-list class="ion-no-padding">
        <ion-item-divider class="ion-padding-top">
          <ion-label>
            Opacity <small>{Math.floor(Number(this.waves.opacity) * 100)}%</small>
          </ion-label>
        </ion-item-divider>
        <ion-item class="item-opacity">
          <ion-range
            color="primary"
            min={0}
            max={1}
            step={0.01}
            disabled={!this.waves.fill || this.waves.fill === undefined}
            value={Number(this.waves.opacity)}
            mode="md"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity(e)}></ion-range>
        </ion-item>
        <ion-item class="item-opacity">
          <deckgo-color
            more
            palette={paletteStore.state.palette}
            class="ion-padding-start ion-padding-end ion-padding-bottom"
            onColorChange={($event: CustomEvent) => this.selectColor($event)}
            color-rgb={this.color}>
            <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
          </deckgo-color>
        </ion-item>
      </ion-list>
    );
  }
}
