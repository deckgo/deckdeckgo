import {RangeChangeEventDetail} from '@ionic/core';
import {Component, Element, Listen, State, h} from '@stencil/core';

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
  private color: string;

  @State()
  private nodes: number = 8;

  @State()
  private coordinates: [string, string][] = [];

  @State()
  private orientation: WavesOrientation = 'upward';

  @State()
  private waves: Waves | null = null;

  componentWillLoad() {
    this.color = paletteStore.state.palette[0] ? paletteStore.state.palette[0].color.rgb : 'rgba(255, 0, 0, 1)';
    this.generateWaves();
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

  private updateOpacity($event: CustomEvent<RangeChangeEventDetail>): void {
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

  private updateNodes($event: CustomEvent<RangeChangeEventDetail>): void {
    if (!$event || !$event.detail || $event.detail.value < 2 || $event.detail.value > 20) {
      return;
    }

    $event.stopPropagation();

    this.nodes = $event.detail.value as number;
    this.generateWaves();
  }

  private mirror(): void {
    this.orientation = this.orientation === 'upward' ? 'downward' : 'upward';
    this.generateWaves(true);
  }

  private generateWaves(mirror: boolean = false): void {
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
    return [
      <ion-header>
        <ion-toolbar color="quaternary">
          <ion-buttons slot="start">{this.renderCloseButton()}</ion-buttons>
          <ion-title class="ion-text-uppercase">SVG Waves</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        {this.renderOptions()}
        <svg {...this.waves}>
          <path d={this.waves.path.d} />
        </svg>
      </ion-content>,
      <ion-footer class="ion-padding-horizontal">
        <ion-button onClick={() => this.selectWave()} color="quaternary">
          Add
        </ion-button>
      </ion-footer>,
    ];
  }

  private renderCloseButton() {
    return (
      <ion-button onClick={() => this.closeModal()}>
        <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
      </ion-button>
    );
  }

  private renderMirrorToggle() {
    return (
      <div>
        <ion-button onClick={() => this.mirror()} disabled={this.orientation === 'upward'} color="quaternary">
          <ion-icon name="chevron-up"></ion-icon>
        </ion-button>
        <ion-button onClick={() => this.mirror()} disabled={this.orientation === 'downward'} color="quaternary">
          <ion-icon name="chevron-down"></ion-icon>
        </ion-button>
      </div>
    );
  }

  private renderOptions() {
    return (
      <ion-list class="ion-no-padding">
        <ion-item-divider class="ion-padding-top">
          <ion-label class="label-waves">Waves</ion-label>
          <ion-button onClick={() => this.generateWaves()} color="quaternary">
            Random
          </ion-button>
          {this.renderMirrorToggle()}
        </ion-item-divider>
        <ion-item class="item-opacity">
          <ion-range
            color="primary"
            min={2}
            max={20}
            step={2}
            value={this.nodes}
            mode="md"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateNodes(e)}></ion-range>
        </ion-item>
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
