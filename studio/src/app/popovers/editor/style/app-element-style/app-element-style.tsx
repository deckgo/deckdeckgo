import {Component, Element, Event, EventEmitter, Host, h, Prop, State} from '@stencil/core';

import settingsStore from '../../../../stores/settings.store';

import {TargetElement} from '../../../../types/editor/target-element';
import {ImageAction} from '../../../../types/editor/image-action';
import {SelectedElement} from '../../../../types/editor/selected-element';

import {ImageHelper} from '../../../../helpers/editor/image.helper';

@Component({
  tag: 'app-element-style',
  styleUrl: 'app-element-style.scss',
})
export class AppElementStyle {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: SelectedElement;

  @Prop()
  imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  imageHelper: ImageHelper;

  @Event() optionsDidChange: EventEmitter<void>;

  @State()
  private applyToTargetElement: TargetElement = TargetElement.SLIDE;

  async componentWillLoad() {
    this.applyToTargetElement = this.selectedElement.slot?.image
      ? TargetElement.IMAGE
      : this.selectedElement.slot?.code || this.selectedElement.slot?.markdown
      ? TargetElement.CODE
      : this.selectedElement.slot?.wordCloud
      ? TargetElement.WORD_CLOUD
      : this.selectedElement.slide?.qrCode || this.selectedElement.slide?.poll
      ? TargetElement.QR_CODE
      : this.selectedElement.slide?.chart
      ? TargetElement.CHART
      : this.selectedElement.slide?.author || this.selectedElement.slide?.split
      ? TargetElement.SIDES
      : TargetElement.SLIDE;
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async selectApplyToTargetElement($event: CustomEvent<TargetElement>) {
    if ($event && $event.detail) {
      this.applyToTargetElement = $event.detail;

      await this.initCurrentColors();
    }
  }

  private async initCurrentColors() {
    if (this.applyToTargetElement !== TargetElement.QR_CODE) {
      return;
    }

    let element: HTMLElement = this.el.querySelector('app-color-qrcode');

    if (element) {
      await (element as any).initCurrentColors();
    }
  }

  private emitStyleChange() {
    this.optionsDidChange.emit();
  }

  private async onImageAction($event: CustomEvent<ImageAction>) {
    if (this.selectedElement.type === 'element') {
      return;
    }

    if ($event && $event.detail) {
      const popover = this.el.closest('ion-popover') as HTMLIonPopoverElement;

      popover.onWillDismiss().then(async () => {
        await this.imageHelper.imageAction(this.selectedElement.element, true, false, $event.detail);
      });

      await popover.dismiss();
    }
  }

  private onImgDidChange($event: CustomEvent<HTMLElement>) {
    if ($event && $event.detail) {
      this.imgDidChange.emit($event.detail);
    }
  }

  render() {
    return (
      <Host edit-mode={settingsStore.state.editMode}>
        <ion-toolbar>
          <h2>{this.selectedElement.type === 'slide' ? 'Slide style' : 'Style'}</h2>
          <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
        </ion-toolbar>

        {this.renderSelectTarget()}

        {this.renderStyleOptions()}

        {this.renderEditMode()}
      </Host>
    );
  }

  private renderEditMode() {
    if (this.applyToTargetElement === TargetElement.TRANSITION) {
      return undefined;
    }

    return <app-edit-mode></app-edit-mode>;
  }

  private renderSelectTarget() {
    if (this.selectedElement.slot?.shape === 'shape') {
      return;
    }

    const elementTarget: boolean = this.selectedElement.type === 'element' && !this.selectedElement.slot?.image;
    const transition: boolean =
      this.selectedElement.type === 'element' &&
      !this.selectedElement.slot?.code &&
      !this.selectedElement.slot?.markdown &&
      !this.selectedElement.slot?.math &&
      !this.selectedElement.slot?.wordCloud &&
      this.selectedElement.slot?.shape === undefined &&
      !this.selectedElement.slot?.demo;

    return (
      <app-select-target-element
        textTarget={elementTarget}
        slide={this.selectedElement.type === 'slide'}
        background={!this.selectedElement.slot?.image}
        qrCode={this.selectedElement.slide?.qrCode || this.selectedElement.slide?.poll}
        chart={this.selectedElement.slide?.chart || this.selectedElement.slide?.poll}
        code={this.selectedElement.slot?.code || this.selectedElement.slot?.markdown}
        image={this.selectedElement.slot?.image}
        sides={this.selectedElement.slide?.author || this.selectedElement.slide?.split}
        transition={transition}
        onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}></app-select-target-element>
    );
  }

  private renderStyleOptions() {
    if (this.applyToTargetElement === TargetElement.QR_CODE) {
      return <app-color-qrcode selectedElement={this.selectedElement.element} onColorChange={() => this.emitStyleChange()}></app-color-qrcode>;
    } else if (this.applyToTargetElement === TargetElement.CHART) {
      return <app-color-chart selectedElement={this.selectedElement.element} onColorChange={() => this.emitStyleChange()}></app-color-chart>;
    } else if (this.applyToTargetElement === TargetElement.CODE) {
      return <app-color-code selectedElement={this.selectedElement.element} onCodeDidChange={() => this.emitStyleChange()}></app-color-code>;
    } else if (this.applyToTargetElement === TargetElement.WORD_CLOUD) {
      return <app-color-word-cloud selectedElement={this.selectedElement.element} onWordCloudDidChange={() => this.emitStyleChange()}></app-color-word-cloud>;
    } else if (this.applyToTargetElement === TargetElement.SIDES) {
      return (
        <app-color-sides
          selectedElement={this.selectedElement.element}
          template={this.selectedElement.slide?.author ? 'author' : 'split'}
          onColorChange={() => this.emitStyleChange()}></app-color-sides>
      );
    } else if (this.applyToTargetElement === TargetElement.BACKGROUND) {
      return this.renderBackground();
    } else if (this.applyToTargetElement === TargetElement.TRANSITION) {
      return <app-reveal selectedElement={this.selectedElement.element} onToggleReveal={() => this.closePopover()}></app-reveal>;
    } else if (this.applyToTargetElement === TargetElement.IMAGE) {
      return (
        <app-image-style
          selectedElement={this.selectedElement.element}
          onImgDidChange={($event: CustomEvent<HTMLElement>) => this.onImgDidChange($event)}></app-image-style>
      );
    } else {
      return [
        this.renderFontSize(),
        this.renderBlock(),
        this.renderLetterSpacing(),
        this.renderList(),
        <app-color-text-background
          key={'text'}
          selectedElement={this.selectedElement.element}
          slide={this.selectedElement.type === 'slide'}
          onColorChange={() => this.emitStyleChange()}></app-color-text-background>,
      ];
    }
  }

  private renderBlock() {
    if (this.selectedElement.type === 'slide') {
      return undefined;
    }

    return <app-block selectedElement={this.selectedElement.element} onAlignChange={() => this.emitStyleChange()}></app-block>;
  }

  private renderLetterSpacing() {
    if (this.selectedElement.slot?.code) {
      return undefined;
    }

    return <app-letter-spacing selectedElement={this.selectedElement.element} onLetterSpacingDidChange={() => this.emitStyleChange()}></app-letter-spacing>;
  }

  private renderBackground() {
    const background = [
      <app-color-text-background
        key={'background'}
        colorType={'background'}
        selectedElement={this.selectedElement.element}
        onColorChange={() => this.emitStyleChange()}></app-color-text-background>,
      this.renderImage(),
    ];

    if (this.selectedElement.type === 'element') {
      background.push(
        <app-border-radius selectedElement={this.selectedElement.element} onBorderRadiusDidChange={() => this.emitStyleChange()}></app-border-radius>
      );
      background.push(<app-box-shadow selectedElement={this.selectedElement.element} onBoxShadowDidChange={() => this.emitStyleChange()}></app-box-shadow>);
    }

    return background;
  }

  private renderImage() {
    if (this.selectedElement.type === 'element') {
      return undefined;
    }

    return (
      <app-image
        selectedElement={this.selectedElement.element}
        deck={true}
        onAction={($event: CustomEvent<ImageAction>) => this.onImageAction($event)}></app-image>
    );
  }

  private renderList() {
    if (!this.selectedElement?.slot?.list) {
      return undefined;
    }

    return (
      <app-list
        selectedElement={this.selectedElement.element}
        onToggleList={() => this.closePopover()}
        onListStyleChanged={() => this.emitStyleChange()}></app-list>
    );
  }

  private renderFontSize() {
    if (!this.selectedElement.slot?.code && !this.selectedElement.slot?.math) {
      return undefined;
    }

    return (
      <app-font-size
        selectedElement={this.selectedElement.element}
        selector={this.selectedElement.slot?.math ? '--deckgo-math-font-size' : '--deckgo-highlight-code-font-size'}
        onCodeDidChange={() => this.emitStyleChange()}></app-font-size>
    );
  }
}
