import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {isIPad} from '@deckdeckgo/utils';

import {TargetElement} from '../../../../utils/editor/target-element';
import {SlotType} from '../../../../utils/editor/slot-type';
import {ImageAction} from '../../../../utils/editor/image-action';
import {ListUtils} from '../../../../utils/editor/list.utils';

import {ImageHelper} from '../../../../helpers/editor/image.helper';

@Component({
  tag: 'app-element-style',
  styleUrl: 'app-element-style.scss',
})
export class AppElementStyle {
  @Element() el: HTMLElement;

  @Prop()
  slide: boolean = false;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  imageHelper: ImageHelper;

  @Prop()
  code: boolean = false;

  @Prop()
  math: boolean = false;

  @Prop()
  shape: 'shape' | 'text' | undefined = undefined;

  @Prop()
  image: boolean = false;

  @Event() optionsDidChange: EventEmitter<void>;

  @State()
  private applyToTargetElement: TargetElement = TargetElement.SLIDE;

  @State()
  private moreColors: boolean = true;

  @State()
  private qrCode: boolean = false;

  @State()
  private chart: boolean = false;

  @State()
  private poll: boolean = false;

  @State()
  private author: boolean = false;

  @State()
  private split: boolean = false;

  @State()
  private demo: boolean = false;

  @State()
  private list: SlotType.OL | SlotType.UL | undefined;

  async componentWillLoad() {
    if (this.slide) {
      this.qrCode = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-qrcode'.toUpperCase();
      this.chart = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-chart'.toUpperCase();
      this.poll = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-poll'.toUpperCase();
      this.author = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-author'.toUpperCase();
      this.split = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-split'.toUpperCase();
    }

    this.demo = this.selectedElement && this.selectedElement.nodeName && this.selectedElement.nodeName.toLocaleLowerCase() === SlotType.DEMO;

    this.list = await ListUtils.isElementList(this.selectedElement);

    this.applyToTargetElement = this.image
      ? TargetElement.IMAGE
      : this.code
      ? TargetElement.CODE
      : this.qrCode || this.poll
      ? TargetElement.QR_CODE
      : this.chart
      ? TargetElement.CHART
      : this.author || this.split
      ? TargetElement.SIDES
      : TargetElement.SLIDE;

    this.moreColors = !isIPad();
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
    let element: HTMLElement;

    if (this.applyToTargetElement === TargetElement.QR_CODE) {
      element = this.el.querySelector('app-color-qrcode');
    } else {
      element = this.el.querySelector('app-color-text-background');
    }

    if (element) {
      await (element as any).initCurrentColors();
    }
  }

  private emitStyleChange() {
    this.optionsDidChange.emit();
  }

  private async onImageAction($event: CustomEvent<ImageAction>) {
    if (!this.slide) {
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
        <h2>{this.slide ? 'Slide style' : 'Style'}</h2>
        <button slot="end" class="close-options" onClick={() => this.closePopover()} tabindex={0}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>
      </ion-toolbar>,
      this.renderSelectTarget(),

      this.renderStyleOptions(),
    ];
  }

  private renderSelectTarget() {
    const elementTarget: boolean = !this.slide && this.shape !== 'shape' && !this.image;
    const transition: boolean = !this.slide && !this.code && !this.math && this.shape === undefined && !this.demo;

    return (
      <app-select-target-element
        textTarget={elementTarget}
        slide={this.slide}
        background={!this.image}
        qrCode={this.qrCode || this.poll}
        chart={this.chart || this.poll}
        code={this.code}
        image={this.image}
        sides={this.author || this.split}
        shape={this.shape === 'shape'}
        transition={transition}
        onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}></app-select-target-element>
    );
  }

  private renderStyleOptions() {
    if (this.applyToTargetElement === TargetElement.QR_CODE) {
      return (
        <app-color-qrcode selectedElement={this.selectedElement} onColorChange={() => this.emitStyleChange()} moreColors={this.moreColors}></app-color-qrcode>
      );
    } else if (this.applyToTargetElement === TargetElement.CHART) {
      return (
        <app-color-chart selectedElement={this.selectedElement} onColorChange={() => this.emitStyleChange()} moreColors={this.moreColors}></app-color-chart>
      );
    } else if (this.applyToTargetElement === TargetElement.CODE) {
      return (
        <app-color-code selectedElement={this.selectedElement} onCodeDidChange={() => this.emitStyleChange()} moreColors={this.moreColors}></app-color-code>
      );
    } else if (this.applyToTargetElement === TargetElement.SIDES) {
      return (
        <app-color-sides
          selectedElement={this.selectedElement}
          template={this.author ? 'author' : 'split'}
          onColorChange={() => this.emitStyleChange()}
          moreColors={this.moreColors}></app-color-sides>
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
        <app-box-shadow selectedElement={this.selectedElement}></app-box-shadow>,
        this.renderList(),
        <app-color-text-background
          expander={!this.slide}
          key={'text'}
          selectedElement={this.selectedElement}
          moreColors={this.moreColors}
          slide={this.slide}
          onColorChange={() => this.emitStyleChange()}></app-color-text-background>,
      ];
    }
  }

  private renderBackground() {
    const background = [
      <app-color-text-background
        key={'background'}
        colorType={'background'}
        selectedElement={this.selectedElement}
        moreColors={this.moreColors}
        onColorChange={() => this.emitStyleChange()}></app-color-text-background>,
      this.renderImage(),
    ];

    if (!this.slide) {
      background.push(<app-border-radius selectedElement={this.selectedElement} onBorderRadiusDidChange={() => this.emitStyleChange()}></app-border-radius>);
    }

    return background;
  }

  private renderImage() {
    if (!this.slide) {
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

    return <app-list selectedElement={this.selectedElement} onToggleList={() => this.closePopover()}></app-list>;
  }

  private renderFontSize() {
    if (!this.code && !this.math) {
      return undefined;
    }

    return (
      <app-font-size
        selectedElement={this.selectedElement}
        selector={this.math ? '--deckgo-math-font-size' : '--deckgo-highlight-code-font-size'}
        onCodeDidChange={() => this.emitStyleChange()}></app-font-size>
    );
  }
}
