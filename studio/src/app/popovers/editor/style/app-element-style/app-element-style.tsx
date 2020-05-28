import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {isIPad} from '@deckdeckgo/utils';

import {TargetElement} from '../../../../utils/editor/target-element';
import {SlotType} from '../../../../utils/editor/slot-type';
import {ImageAction} from '../../../../utils/editor/image-action';
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

  @Event() styleDidChange: EventEmitter<boolean>;

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
  private code: boolean = false;

  @State()
  private shape: boolean = false;

  @State()
  private author: boolean = false;

  @State()
  private split: boolean = false;

  async componentWillLoad() {
    if (this.slide) {
      this.qrCode = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-qrcode'.toUpperCase();
      this.chart = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-chart'.toUpperCase();
      this.poll = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-poll'.toUpperCase();
      this.author = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-author'.toUpperCase();
      this.split = this.selectedElement && this.selectedElement.tagName && this.selectedElement.tagName.toUpperCase() === 'deckgo-slide-split'.toUpperCase();
    }

    this.code = this.selectedElement && this.selectedElement.nodeName && this.selectedElement.nodeName.toLocaleLowerCase() === SlotType.CODE;
    this.shape = this.selectedElement && this.selectedElement.nodeName && this.selectedElement.nodeName.toLocaleLowerCase() === SlotType.DRAG_RESIZE_ROTATE;

    // prettier-ignore
    this.applyToTargetElement = this.code ? TargetElement.CODE : (this.qrCode || this.poll ? TargetElement.QR_CODE : (this.chart ? TargetElement.CHART : (this.author || this.split ? TargetElement.SIDES : TargetElement.SLIDE)));

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

  private colorChange($event: CustomEvent<boolean>) {
    if ($event) {
      this.styleDidChange.emit($event.detail);
    }
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
        <ion-router-link slot="end" onClick={() => this.closePopover()}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      this.renderSelectTarget(),

      this.renderColorOptions(),
    ];
  }

  private renderSelectTarget() {
    const elementTarget: boolean = !this.slide && !this.shape;

    return (
      <app-select-target-element
        textTarget={elementTarget}
        slide={this.slide}
        background={true}
        qrCode={this.qrCode || this.poll}
        chart={this.chart || this.poll}
        code={this.code}
        sides={this.author || this.split}
        onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}></app-select-target-element>
    );
  }

  private renderColorOptions() {
    if (this.applyToTargetElement === TargetElement.QR_CODE) {
      return (
        <app-color-qrcode
          selectedElement={this.selectedElement}
          onColorChange={($event: CustomEvent<boolean>) => this.colorChange($event)}
          moreColors={this.moreColors}></app-color-qrcode>
      );
    } else if (this.applyToTargetElement === TargetElement.CHART) {
      return (
        <app-color-chart
          selectedElement={this.selectedElement}
          onColorChange={($event: CustomEvent<boolean>) => this.colorChange($event)}
          moreColors={this.moreColors}></app-color-chart>
      );
    } else if (this.applyToTargetElement === TargetElement.CODE) {
      return (
        <app-color-code
          selectedElement={this.selectedElement}
          onColorChange={($event: CustomEvent<boolean>) => this.colorChange($event)}
          moreColors={this.moreColors}></app-color-code>
      );
    } else if (this.applyToTargetElement === TargetElement.SIDES) {
      return (
        <app-color-sides
          selectedElement={this.selectedElement}
          template={this.author ? 'author' : 'split'}
          onColorChange={($event: CustomEvent<boolean>) => this.colorChange($event)}
          moreColors={this.moreColors}></app-color-sides>
      );
    } else if (this.applyToTargetElement === TargetElement.BACKGROUND) {
      return [
        <app-color-text-background
          colorType={'background'}
          selectedElement={this.selectedElement}
          moreColors={this.moreColors}
          onColorChange={($event: CustomEvent<boolean>) => this.colorChange($event)}></app-color-text-background>,
        this.renderImage(),
      ];
    } else {
      return (
        <app-color-text-background
          selectedElement={this.selectedElement}
          moreColors={this.moreColors}
          slide={this.slide}
          shape={this.shape}
          onColorChange={($event: CustomEvent<boolean>) => this.colorChange($event)}></app-color-text-background>
      );
    }
  }

  private renderImage() {
    if (!this.slide) {
      return undefined;
    }

    return (
      <app-image
        selectedElement={this.selectedElement}
        deck={true}
        onAction={($event: CustomEvent<ImageAction>) => this.onImageAction($event)}
        onImgDidChange={($event: CustomEvent<HTMLElement>) => this.onImgDidChange($event)}></app-image>
    );
  }
}
