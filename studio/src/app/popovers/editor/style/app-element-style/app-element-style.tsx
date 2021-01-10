import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {TargetElement} from '../../../../utils/editor/target-element';
import {SlotType} from '../../../../utils/editor/slot-type';
import {ImageAction} from '../../../../utils/editor/image-action';

import {ImageHelper} from '../../../../helpers/editor/image.helper';
import {SelectedElementDescription} from '../../../../utils/editor/selected-element.utils';

@Component({
  tag: 'app-element-style',
  styleUrl: 'app-element-style.scss',
})
export class AppElementStyle {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  selectedDescription: SelectedElementDescription;

  @Prop()
  imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  imageHelper: ImageHelper;

  @Event() optionsDidChange: EventEmitter<void>;

  @State()
  private applyToTargetElement: TargetElement = TargetElement.SLIDE;

  @State()
  private list: SlotType.OL | SlotType.UL | undefined;

  async componentWillLoad() {
    this.applyToTargetElement = this.selectedDescription.slot.image
      ? TargetElement.IMAGE
      : this.selectedDescription.slot.code || this.selectedDescription.slot.markdown
      ? TargetElement.CODE
      : this.selectedDescription.slot.wordCloud
      ? TargetElement.WORD_CLOUD
      : this.selectedDescription.slide.qrCode || this.selectedDescription.slide.poll
      ? TargetElement.QR_CODE
      : this.selectedDescription.slide.chart
      ? TargetElement.CHART
      : this.selectedDescription.slide.author || this.selectedDescription.slide.split
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
    if (this.selectedDescription.type === 'element') {
      return;
    }

    if ($event && $event.detail) {
      const popover = this.el.closest('ion-popover') as HTMLIonPopoverElement;

      popover.onWillDismiss().then(async () => {
        await this.imageHelper.imageAction(this.selectedElement, true, false, $event.detail);
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
    return [
      <ion-toolbar>
        <h2>{this.selectedDescription.type === 'slide' ? 'Slide style' : 'Style'}</h2>
        <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
      </ion-toolbar>,
      this.renderSelectTarget(),

      this.renderStyleOptions(),
    ];
  }

  private renderSelectTarget() {
    if (this.selectedDescription.slot.shape === 'shape') {
      return;
    }

    const elementTarget: boolean = this.selectedDescription.type === 'element' && !this.selectedDescription.slot.image;
    const transition: boolean =
      this.selectedDescription.type === 'element' &&
      !this.selectedDescription.slot.code &&
      !this.selectedDescription.slot.markdown &&
      !this.selectedDescription.slot.math &&
      !this.selectedDescription.slot.wordCloud &&
      this.selectedDescription.slot.shape === undefined &&
      !this.selectedDescription.slot.demo;

    return (
      <app-select-target-element
        textTarget={elementTarget}
        slide={this.selectedDescription.type === 'slide'}
        background={!this.selectedDescription.slot.image}
        qrCode={this.selectedDescription.slide.qrCode || this.selectedDescription.slide.poll}
        chart={this.selectedDescription.slide.chart || this.selectedDescription.slide.poll}
        code={this.selectedDescription.slot.code || this.selectedDescription.slot.markdown}
        image={this.selectedDescription.slot.image}
        sides={this.selectedDescription.slide.author || this.selectedDescription.slide.split}
        transition={transition}
        onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}></app-select-target-element>
    );
  }

  private renderStyleOptions() {
    if (this.applyToTargetElement === TargetElement.QR_CODE) {
      return <app-color-qrcode selectedElement={this.selectedElement} onColorChange={() => this.emitStyleChange()}></app-color-qrcode>;
    } else if (this.applyToTargetElement === TargetElement.CHART) {
      return <app-color-chart selectedElement={this.selectedElement} onColorChange={() => this.emitStyleChange()}></app-color-chart>;
    } else if (this.applyToTargetElement === TargetElement.CODE) {
      return <app-color-code selectedElement={this.selectedElement} onCodeDidChange={() => this.emitStyleChange()}></app-color-code>;
    } else if (this.applyToTargetElement === TargetElement.WORD_CLOUD) {
      return <app-color-word-cloud selectedElement={this.selectedElement} onWordCloudDidChange={() => this.emitStyleChange()}></app-color-word-cloud>;
    } else if (this.applyToTargetElement === TargetElement.SIDES) {
      return (
        <app-color-sides
          selectedElement={this.selectedElement}
          template={this.selectedDescription.slide.author ? 'author' : 'split'}
          onColorChange={() => this.emitStyleChange()}></app-color-sides>
      );
    } else if (this.applyToTargetElement === TargetElement.BACKGROUND) {
      return this.renderBackground();
    } else if (this.applyToTargetElement === TargetElement.TRANSITION) {
      return <app-reveal selectedElement={this.selectedElement} onToggleReveal={() => this.closePopover()}></app-reveal>;
    } else if (this.applyToTargetElement === TargetElement.IMAGE) {
      return (
        <app-image-style
          selectedElement={this.selectedElement}
          onImgDidChange={($event: CustomEvent<HTMLElement>) => this.onImgDidChange($event)}></app-image-style>
      );
    } else {
      return [
        this.renderFontSize(),
        <app-align selectedElement={this.selectedElement} onAlignChange={() => this.emitStyleChange()}></app-align>,
        this.renderLetterSpacing(),
        this.renderList(),
        <app-color-text-background
          expanded={!this.selectedDescription.slot.code}
          key={'text'}
          selectedElement={this.selectedElement}
          slide={this.selectedDescription.type === 'slide'}
          onColorChange={() => this.emitStyleChange()}></app-color-text-background>,
      ];
    }
  }

  private renderLetterSpacing() {
    if (this.selectedDescription.slot.code) {
      return undefined;
    }

    return <app-letter-spacing selectedElement={this.selectedElement} onLetterSpacingDidChange={() => this.emitStyleChange()}></app-letter-spacing>;
  }

  private renderBackground() {
    const background = [
      <app-color-text-background
        key={'background'}
        colorType={'background'}
        selectedElement={this.selectedElement}
        expanded={false}
        onColorChange={() => this.emitStyleChange()}></app-color-text-background>,
      this.renderImage(),
    ];

    if (this.selectedDescription.type === 'element') {
      background.push(<app-border-radius selectedElement={this.selectedElement} onBorderRadiusDidChange={() => this.emitStyleChange()}></app-border-radius>);
      background.push(<app-box-shadow selectedElement={this.selectedElement} onBoxShadowDidChange={() => this.emitStyleChange()}></app-box-shadow>);
    }

    return background;
  }

  private renderImage() {
    if (this.selectedDescription.type === 'element') {
      return undefined;
    }

    return (
      <app-image selectedElement={this.selectedElement} deck={true} onAction={($event: CustomEvent<ImageAction>) => this.onImageAction($event)}></app-image>
    );
  }

  private renderList() {
    if (!this.list) {
      return undefined;
    }

    return (
      <app-list selectedElement={this.selectedElement} onToggleList={() => this.closePopover()} onListStyleChanged={() => this.emitStyleChange()}></app-list>
    );
  }

  private renderFontSize() {
    if (!this.selectedDescription.slot.code && !this.selectedDescription.slot.math) {
      return undefined;
    }

    return (
      <app-font-size
        selectedElement={this.selectedElement}
        selector={this.selectedDescription.slot.math ? '--deckgo-math-font-size' : '--deckgo-highlight-code-font-size'}
        onCodeDidChange={() => this.emitStyleChange()}></app-font-size>
    );
  }
}
