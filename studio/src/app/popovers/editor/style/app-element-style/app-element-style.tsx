import {Component, Element, Event, EventEmitter, Host, h, Prop, State} from '@stencil/core';

import settingsStore from '../../../../stores/settings.store';
import i18n from '../../../../stores/i18n.store';

import {TargetElement} from '../../../../types/editor/target-element';
import {ImageAction} from '../../../../types/editor/image-action';
import {SelectedTarget} from '../../../../types/editor/selected-target';

import {ImageHelper} from '../../../../helpers/editor/image.helper';

@Component({
  tag: 'app-element-style',
  styleUrl: 'app-element-style.scss'
})
export class AppElementStyle {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: SelectedTarget;

  @Prop()
  imgDidChange?: EventEmitter<HTMLElement>;

  @Prop()
  imageHelper?: ImageHelper;

  @Event() optionsDidChange: EventEmitter<void>;

  @State()
  private applyToTargetElement: TargetElement = TargetElement.SLIDE;

  async componentWillLoad() {
    this.applyToTargetElement = this.selectedTarget.element?.image
      ? TargetElement.IMAGE
      : this.selectedTarget.element?.code || this.selectedTarget.element?.markdown
      ? TargetElement.CODE
      : this.selectedTarget.element?.wordCloud
      ? TargetElement.WORD_CLOUD
      : this.selectedTarget.slide?.qrCode || this.selectedTarget.slide?.poll
      ? TargetElement.QR_CODE
      : this.selectedTarget.slide?.chart
      ? TargetElement.CHART
      : this.selectedTarget.slide?.author || this.selectedTarget.slide?.split
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
    if (this.selectedTarget.type === 'element') {
      return;
    }

    if ($event && $event.detail) {
      const popover = this.el.closest('ion-popover') as HTMLIonPopoverElement;

      popover.onWillDismiss().then(async () => {
        await this.imageHelper.imageAction(this.selectedTarget.target, true, false, $event.detail);
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
          <h2>{this.selectedTarget.type === 'slide' ? i18n.state.editor.slide_style : i18n.state.editor.style}</h2>
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
    if (this.selectedTarget.element?.shape === 'shape') {
      return;
    }

    const elementTarget: boolean =
      this.selectedTarget.type === 'element' && !this.selectedTarget.element?.image && !this.selectedTarget.element?.wordCloud;
    const transition: boolean =
      this.selectedTarget.type === 'element' &&
      !this.selectedTarget.element?.code &&
      !this.selectedTarget.element?.markdown &&
      !this.selectedTarget.element?.math &&
      !this.selectedTarget.element?.wordCloud &&
      this.selectedTarget.element?.shape === undefined &&
      !this.selectedTarget.element?.demo;

    return (
      <app-select-target-element
        textTarget={elementTarget}
        slide={this.selectedTarget.type === 'slide'}
        qrCode={this.selectedTarget.slide?.qrCode || this.selectedTarget.slide?.poll}
        chart={this.selectedTarget.slide?.chart || this.selectedTarget.slide?.poll}
        code={this.selectedTarget.element?.code || this.selectedTarget.element?.markdown}
        image={this.selectedTarget.element?.image}
        sides={this.selectedTarget.slide?.author || this.selectedTarget.slide?.split}
        wordCloud={this.selectedTarget.element?.wordCloud}
        transition={transition}
        onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}
      ></app-select-target-element>
    );
  }

  private renderStyleOptions() {
    if (this.applyToTargetElement === TargetElement.QR_CODE) {
      return <app-color-qrcode selectedTarget={this.selectedTarget.target} onColorChange={() => this.emitStyleChange()}></app-color-qrcode>;
    } else if (this.applyToTargetElement === TargetElement.CHART) {
      return <app-color-chart selectedTarget={this.selectedTarget.target} onColorChange={() => this.emitStyleChange()}></app-color-chart>;
    } else if (this.applyToTargetElement === TargetElement.CODE) {
      return <app-color-code selectedTarget={this.selectedTarget.target} onCodeDidChange={() => this.emitStyleChange()}></app-color-code>;
    } else if (this.applyToTargetElement === TargetElement.WORD_CLOUD) {
      return (
        <app-color-word-cloud
          selectedTarget={this.selectedTarget.target}
          onWordCloudDidChange={() => this.emitStyleChange()}
        ></app-color-word-cloud>
      );
    } else if (this.applyToTargetElement === TargetElement.SIDES) {
      return (
        <app-color-sides
          selectedTarget={this.selectedTarget.target}
          template={this.selectedTarget.slide?.author ? 'author' : 'split'}
          onColorChange={() => this.emitStyleChange()}
        ></app-color-sides>
      );
    } else if (this.applyToTargetElement === TargetElement.BACKGROUND) {
      return this.renderBackground();
    } else if (this.applyToTargetElement === TargetElement.TRANSITION) {
      return <app-reveal selectedTarget={this.selectedTarget.target} onToggleReveal={() => this.closePopover()}></app-reveal>;
    } else if (this.applyToTargetElement === TargetElement.IMAGE) {
      return [
        <app-image-style
          selectedTarget={this.selectedTarget.target}
          onImgDidChange={($event: CustomEvent<HTMLElement>) => this.onImgDidChange($event)}
        ></app-image-style>,
        this.renderBlock()
      ];
    } else {
      return [
        this.renderText(),
        this.renderBlock(),
        this.renderList(),
        <app-color-text-background
          key={'text'}
          selectedTarget={this.selectedTarget.target}
          slide={this.selectedTarget.type === 'slide'}
          onColorChange={() => this.emitStyleChange()}
        ></app-color-text-background>
      ];
    }
  }

  private renderBlock() {
    if (this.selectedTarget.type === 'slide') {
      return undefined;
    }

    return <app-block selectedTarget={this.selectedTarget} onBlockChange={() => this.emitStyleChange()}></app-block>;
  }

  private renderText() {
    return <app-text selectedTarget={this.selectedTarget} onTextDidChange={() => this.emitStyleChange()}></app-text>;
  }

  private renderBackground() {
    const background = [
      <app-color-text-background
        key={'background'}
        colorType={'background'}
        slide={this.selectedTarget.type === 'slide'}
        selectedTarget={this.selectedTarget.target}
        onColorChange={() => this.emitStyleChange()}
      ></app-color-text-background>,
      this.renderImage()
    ];

    if (this.selectedTarget.type === 'element') {
      background.push(
        <app-border-radius selectedTarget={this.selectedTarget} onBorderRadiusDidChange={() => this.emitStyleChange()}></app-border-radius>
      );
      background.push(
        <app-box-shadow selectedTarget={this.selectedTarget} onBoxShadowDidChange={() => this.emitStyleChange()}></app-box-shadow>
      );
    }

    return background;
  }

  private renderImage() {
    if (this.selectedTarget.type === 'element') {
      return undefined;
    }

    return (
      <app-image-choice
        selectedTarget={this.selectedTarget.target}
        deck={true}
        onAction={($event: CustomEvent<ImageAction>) => this.onImageAction($event)}
      ></app-image-choice>
    );
  }

  private renderList() {
    if (!this.selectedTarget?.element?.list) {
      return undefined;
    }

    return (
      <app-list
        selectedTarget={this.selectedTarget.target}
        onToggleList={() => this.closePopover()}
        onListStyleChanged={() => this.emitStyleChange()}
      ></app-list>
    );
  }
}
