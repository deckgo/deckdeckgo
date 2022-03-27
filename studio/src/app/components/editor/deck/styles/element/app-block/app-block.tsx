import type {RangeChangeEventDetail} from '@ionic/core';
import {Component, Event, EventEmitter, Fragment, h, Prop, State} from '@stencil/core';
import i18n from '../../../../../../stores/i18n.store';
import settingsStore from '../../../../../../stores/settings.store';
import {EditMode, Expanded} from '../../../../../../types/core/settings';
import {SelectedTarget} from '../../../../../../types/editor/selected-target';
import {SettingsUtils} from '../../../../../../utils/core/settings.utils';
import {setStyle} from '../../../../../../utils/editor/undo-redo.deck.utils';

@Component({
  tag: 'app-block'
})
export class AppBlock {
  @Prop()
  selectedTarget: SelectedTarget;

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

  private ignoreUpdateStyle: boolean = false;

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
    const css: CSSStyleDeclaration = window.getComputedStyle(this.selectedTarget?.target);
    const padding: number = parseInt(css.paddingTop);
    this.padding = isNaN(padding) ? 0 : padding;
  }

  private async initPaddingCSS() {
    const css: CSSStyleDeclaration = window.getComputedStyle(this.selectedTarget?.target);
    this.paddingCSS = css.padding;
  }

  private async initWidth() {
    const width: number = parseInt(this.selectedTarget?.target?.style.width);
    this.width = isNaN(width) ? 100 : width;
  }

  private async initWidthCSS() {
    this.widthCSS = this.selectedTarget?.target?.style.width;
  }

  private async initRotate() {
    const matches: RegExpMatchArray | null = this.selectedTarget?.target?.style.transform.match(/(\d+)/);
    const rotate: number = parseInt(matches?.[0]);
    this.rotate = isNaN(rotate) ? 0 : rotate;
  }

  private async initTransformCSS() {
    this.transformCSS = this.selectedTarget?.target?.style.transform;
  }

  private async updateWidth($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    this.width = $event.detail.value as number;

    this.updateStyle({property: 'width', value: `${this.width}%`});

    this.blockChange.emit();
  }

  private handleWidthInput($event: CustomEvent<KeyboardEvent>) {
    this.widthCSS = ($event.target as InputTargetEvent).value;
  }

  private updateWidthCSS() {
    if (!this.selectedTarget) {
      return;
    }

    this.updateStyle({property: 'width', value: this.widthCSS});

    this.blockChange.emit();
  }

  private async updatePadding($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0) {
      return;
    }

    $event.stopPropagation();

    this.padding = $event.detail.value as number;

    this.updateStyle({property: 'padding', value: `${this.padding}px`});

    this.blockChange.emit();
  }

  private handlePaddingInput($event: CustomEvent<KeyboardEvent>) {
    this.paddingCSS = ($event.target as InputTargetEvent).value;
  }

  private updatePaddingCSS() {
    if (!this.selectedTarget) {
      return;
    }

    this.updateStyle({property: 'padding', value: this.paddingCSS});

    this.blockChange.emit();
  }

  private async updateRotate($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 360) {
      return;
    }

    $event.stopPropagation();

    this.rotate = $event.detail.value as number;

    this.updateStyle({property: 'transform', value: `rotate(${this.rotate}deg)`});

    this.blockChange.emit();
  }

  private handleTransformInput($event: CustomEvent<KeyboardEvent>) {
    this.transformCSS = ($event.target as InputTargetEvent).value;
  }

  private updateTransformCSS() {
    if (!this.selectedTarget) {
      return;
    }

    this.updateStyle({property: 'transform', value: this.transformCSS});

    this.blockChange.emit();
  }

  private updateStyle({property, value}: {property: 'transform' | 'width' | 'padding'; value: string}) {
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
          switch (property) {
            case 'transform':
              await this.initTransformCSS();
              break;
            case 'width':
              await this.initWidthCSS();
              break;
            case 'padding':
              await this.initPaddingCSS();
          }

          return;
        }

        switch (property) {
          case 'transform':
            await this.initRotate();
            break;
          case 'width':
            await this.initWidth();
            break;
          case 'padding':
            await this.initPadding();
        }
      }
    });
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.block}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({block: $event.detail})}>
        <ion-label slot="title">{i18n.state.editor.block}</ion-label>
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
          <ion-label>
            {i18n.state.editor.rotate}{' '}
            {settingsStore.state.editMode === 'properties' ? (
              <small>
                {this.rotate}
                {i18n.state.editor.deg}
              </small>
            ) : undefined}
          </ion-label>
        </ion-item-divider>

        <ion-item class="item-range properties">
          <ion-range
            color="dark"
            value={this.rotate}
            min={0}
            max={360}
            mode="md"
            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateRotate($event)}></ion-range>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.transformCSS}
            placeholder={i18n.state.editor.transform}
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
          <ion-label>
            {i18n.state.editor.padding} {settingsStore.state.editMode === 'properties' ? <small>{this.padding}px</small> : undefined}
          </ion-label>
        </ion-item-divider>

        <ion-item class="item-range properties">
          <ion-range
            color="dark"
            value={this.padding}
            mode="md"
            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updatePadding($event)}></ion-range>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.paddingCSS}
            placeholder={i18n.state.editor.padding}
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
          <ion-label>
            {i18n.state.editor.width} {settingsStore.state.editMode === 'properties' ? <small>{this.width}%</small> : undefined}
          </ion-label>
        </ion-item-divider>
        <ion-item class="item-range properties">
          <ion-range
            color="dark"
            min={1}
            max={100}
            value={this.width}
            mode="md"
            onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateWidth($event)}></ion-range>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.widthCSS}
            placeholder={i18n.state.editor.width}
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleWidthInput(e)}
            onIonChange={() => this.updateWidthCSS()}></ion-input>
        </ion-item>
      </Fragment>
    );
  }
}
