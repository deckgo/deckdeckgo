import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';

import {TargetElement} from '../../../utils/editor/target-element';

@Component({
  tag: 'app-select-target-element',
  styleUrl: 'app-select-target-element.scss',
})
export class AppSelectTargetElement {
  @Prop()
  slide: boolean = false;

  @Prop()
  qrCode: boolean = false;

  @Prop()
  chart: boolean = false;

  @Prop()
  code: boolean = false;

  @Prop()
  sides: boolean = false;

  // color is a reserved prop word
  @Prop()
  textTarget: boolean = false;

  @Prop()
  background: boolean = false;

  @Prop()
  transition: boolean = false;

  @Prop()
  headerFooter: boolean = false;

  @Prop()
  image: boolean = false;

  @Prop()
  shape: boolean = false;

  @Event()
  applyTo: EventEmitter<TargetElement>;

  private selectApplyToAll($event: CustomEvent) {
    if ($event && $event.detail) {
      this.applyTo.emit($event.detail.value);
    }
  }

  render() {
    const selectedValue: TargetElement = this.code
      ? TargetElement.CODE
      : this.image
      ? TargetElement.IMAGE
      : this.textTarget
      ? TargetElement.TEXT
      : this.sides
      ? TargetElement.SIDES
      : this.qrCode
      ? TargetElement.QR_CODE
      : this.chart
      ? TargetElement.CHART
      : TargetElement.SLIDE;

    return (
      <ion-segment mode="md" class="ion-padding-bottom" value={selectedValue} onIonChange={($event: CustomEvent) => this.selectApplyToAll($event)}>
        {this.renderQRCode()}
        {this.renderChart()}
        {this.renderSides()}
        {this.renderSlide()}
        {this.renderCode()}
        {this.renderImage()}
        {this.renderText()}
        {this.renderBackground()}
        {this.renderHeaderFooter()}
        {this.renderTransition()}
      </ion-segment>
    );
  }

  private renderQRCode() {
    if (this.qrCode) {
      return (
        <ion-segment-button value={TargetElement.QR_CODE} mode="md">
          <ion-label>QR code</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderChart() {
    if (this.chart) {
      return (
        <ion-segment-button value={TargetElement.CHART} mode="md">
          <ion-label>Chart</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderSlide() {
    if (this.slide) {
      return (
        <ion-segment-button value={TargetElement.SLIDE} mode="md">
          <ion-label>Font</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderText() {
    if (this.textTarget) {
      return (
        <ion-segment-button value={TargetElement.TEXT} mode="md">
          <ion-label>Font</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderHeaderFooter() {
    if (this.headerFooter) {
      return (
        <ion-segment-button value={TargetElement.HEADER_FOOTER} mode="md" class="header-footer">
          <ion-label>Header &amp; Footer</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderBackground() {
    if (this.background) {
      return (
        <ion-segment-button value={TargetElement.BACKGROUND} mode="md">
          <ion-label>{this.shape ? 'Color' : 'Background'}</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderCode() {
    if (this.code) {
      return (
        <ion-segment-button value={TargetElement.CODE} mode="md">
          <ion-label>Code</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderImage() {
    if (this.image) {
      return (
        <ion-segment-button value={TargetElement.IMAGE} mode="md">
          <ion-label>Image</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderTransition() {
    if (this.transition) {
      return (
        <ion-segment-button value={TargetElement.TRANSITION} mode="md">
          <ion-label>Transition</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }

  private renderSides() {
    if (this.sides) {
      return (
        <ion-segment-button value={TargetElement.SIDES} mode="md">
          <ion-label>Sides</ion-label>
        </ion-segment-button>
      );
    } else {
      return undefined;
    }
  }
}
