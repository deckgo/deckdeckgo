import {Waves, WavesOrientation} from '@deckdeckgo/editor';
import type {RangeChangeEventDetail} from '@ionic/core';
import {Component, Element, Fragment, h, Listen, State} from '@stencil/core';
import {AppIcon} from '../../../components/core/app-icon/app-icon';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import i18n from '../../../stores/i18n.store';
import {ColorUtils, InitStyleColor} from '../../../utils/editor/color.utils';
import {WavesUtils} from '../../../utils/editor/waves.utils';

@Component({
  tag: 'app-waves',
  styleUrl: 'app-waves.scss'
})
export class AppWaves {
  @Element() el: HTMLElement;

  @State()
  private color: string = '100, 29, 128'; // quaternary color

  @State()
  private nodes: number = 8;

  @State()
  private coordinates: [string, string][] = [];

  @State()
  private orientation: WavesOrientation = 'upward';

  @State()
  private waves: Waves | null = null;

  private imageHistoryService: ImageHistoryService;

  constructor() {
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

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

  private initColor = async (): Promise<InitStyleColor> => {
    return {
      rgb: this.color,
      opacity: 100
    };
  };

  private resetColor() {
    this.color = '100, 29, 128';

    this.waves = {...this.waves, fill: `rgba(${this.color}, 1)`, opacity: '1'};
  }

  private async applyColor($event: CustomEvent<string>) {
    if (!$event || !$event.detail) {
      return;
    }

    $event.stopPropagation();

    const color: InitStyleColor = await ColorUtils.splitColor($event.detail);

    this.waves = {...this.waves, fill: `rgba(${color.rgb}, 1)`, opacity: `${color.opacity / 100}`};

    this.color = color.rgb;
  }

  private async updateNodes($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 2 || $event.detail.value > 20) {
      return;
    }

    $event.stopPropagation();

    this.nodes = $event.detail.value as number;
    await this.generateWaves();
  }

  private async setOrientation(orientation: WavesOrientation) {
    this.orientation = orientation;
    await this.generateWaves(true);
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
      wave: this.orientation,
      path: {d: fullPath}
    };
  }

  private async selectWave() {
    const svg: Waves = this.waves;

    await this.imageHistoryService.push(svg);

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(svg);
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="quaternary">
            <ion-buttons slot="start">{this.renderCloseButton()}</ion-buttons>
            <ion-title class="ion-text-uppercase">{i18n.state.editor.waves}</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding">
          <div class="container ion-margin-bottom">
            <svg {...this.waves}>
              <path d={this.waves.path.d} />
            </svg>
          </div>

          <app-expansion-panel>
            <ion-label slot="title">{i18n.state.editor.waves}</ion-label>
            {this.renderOptions()}
          </app-expansion-panel>

          <app-expansion-panel>
            <ion-label slot="title">{i18n.state.editor.color}</ion-label>
            {this.renderColor()}
          </app-expansion-panel>

          <ion-button onClick={() => this.selectWave()} color="dark" shape="round">
            {i18n.state.core.add}
          </ion-button>
        </ion-content>
      </Fragment>
    );
  }

  private renderCloseButton() {
    return (
      <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
        <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
      </ion-button>
    );
  }

  private renderOptions() {
    return (
      <div class="options">
        <ion-fab-button size="small" onClick={() => this.generateWaves()} color="quaternary" aria-label="Randomize">
          <AppIcon name="shuffle" ariaLabel="" ariaHidden={true}></AppIcon>
        </ion-fab-button>

        <ion-fab-button
          size="small"
          onClick={() => this.setOrientation('upward')}
          color={this.orientation === 'upward' ? 'quaternary' : 'medium'}
          aria-label={i18n.state.editor.direction_up}>
          <AppIcon name="chevron-up" ariaLabel="" ariaHidden={true}></AppIcon>
        </ion-fab-button>

        <ion-fab-button
          size="small"
          onClick={() => this.setOrientation('downward')}
          color={this.orientation === 'downward' ? 'quaternary' : 'medium'}
          aria-label={i18n.state.editor.direction_down}>
          <AppIcon name="chevron-down" ariaLabel="" ariaHidden={true}></AppIcon>
        </ion-fab-button>

        <div class="complexity">
          <AppIcon name="waves-simple" path="icons" ariaLabel="" ariaHidden={true}></AppIcon>

          <ion-range
            color="quaternary"
            min={2}
            max={20}
            step={2}
            value={this.nodes}
            mode="md"
            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateNodes($event)}></ion-range>

          <AppIcon name="waves-complex" path="icons" ariaLabel="" ariaHidden={true}></AppIcon>
        </div>
      </div>
    );
  }

  private renderColor() {
    return (
      <app-color
        initColor={this.initColor}
        onResetColor={() => this.resetColor()}
        onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}></app-color>
    );
  }
}
