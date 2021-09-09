import {Component, EventEmitter, h, Prop, State, Event} from '@stencil/core';

import {ImageAlign, ImageSize} from '../../../types/enums';

import {DeckdeckgoInlineEditorUtils} from '../../../utils/utils';

@Component({
  tag: 'deckgo-ie-image-actions',
  styleUrl: 'image-actions.scss',
  shadow: true
})
export class ImageActions {
  @Prop()
  anchorEvent: MouseEvent | TouchEvent;

  @Prop()
  imgPropertyWidth: string = 'width';

  @Prop()
  imgPropertyCssFloat: string = 'cssFloat';

  @Prop()
  imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  containers: string;

  @Prop()
  imgAnchor: string;

  @Prop()
  mobile: boolean;

  @State()
  private imageSize: ImageSize;

  @State()
  private imageAlign: ImageAlign;

  @Event()
  imgModified: EventEmitter<void>;

  componentWillLoad() {
    const target: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;

    if (target.style.getPropertyValue(this.imgPropertyWidth) === '25%') {
      this.imageSize = ImageSize.SMALL;
    } else if (target.style.getPropertyValue(this.imgPropertyWidth) === '50%') {
      this.imageSize = ImageSize.MEDIUM;
    } else if (target.style.getPropertyValue(this.imgPropertyWidth) === '75%') {
      this.imageSize = ImageSize.LARGE;
    } else {
      this.imageSize = ImageSize.ORIGINAL;
    }

    if (target.style.getPropertyValue(this.imgPropertyCssFloat) === 'left') {
      this.imageAlign = ImageAlign.START;
    } else {
      this.imageAlign = ImageAlign.STANDARD;
    }
  }

  private styleImage(e: UIEvent, applyFunction: Function, param: ImageSize | ImageAlign): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const isAnchorImg: boolean = await this.isAnchorImage();
      if (!isAnchorImg) {
        resolve();
        return;
      }

      e.stopPropagation();

      applyFunction(param);

      const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;
      const container: HTMLElement | undefined = await DeckdeckgoInlineEditorUtils.findContainer(this.containers, anchorImg);

      if (!container) {
        resolve();
        return;
      }

      this.imgDidChange.emit(container);

      this.imgModified.emit();

      resolve();
    });
  }

  private setImageWith = async (size: ImageSize) => {
    const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;
    anchorImg.style.setProperty(this.imgPropertyWidth, size.toString());
  };

  private setImageAlignment = (align: ImageAlign) => {
    const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;

    if (align === ImageAlign.START) {
      anchorImg.style.setProperty(this.imgPropertyCssFloat, 'left');
    } else {
      anchorImg.style.removeProperty(this.imgPropertyCssFloat);
    }
  };

  private deleteImage($event: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const isAnchorImg: boolean = await this.isAnchorImage();
      if (!isAnchorImg) {
        resolve();
        return;
      }

      $event.stopPropagation();

      const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;

      if (!anchorImg || !anchorImg.parentElement) {
        resolve();
        return;
      }

      const container: HTMLElement | undefined = await DeckdeckgoInlineEditorUtils.findContainer(this.containers, anchorImg);

      if (!container) {
        resolve();
        return;
      }

      anchorImg.parentElement.removeChild(anchorImg);

      this.imgDidChange.emit(container);

      this.imgModified.emit();

      resolve();
    });
  }

  private isAnchorImage(): Promise<boolean> {
    return DeckdeckgoInlineEditorUtils.isAnchorImage(this.anchorEvent, this.imgAnchor);
  }

  render() {
    return [
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.styleImage($event.detail, this.setImageWith, ImageSize.ORIGINAL)}
        class={this.imageSize === ImageSize.ORIGINAL ? 'active' : undefined}>
        <deckgo-ie-action-image class={'image original'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.styleImage($event.detail, this.setImageWith, ImageSize.LARGE)}
        class={this.imageSize === ImageSize.LARGE ? 'active' : undefined}>
        <deckgo-ie-action-image class={'image large'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.styleImage($event.detail, this.setImageWith, ImageSize.MEDIUM)}
        class={this.imageSize === ImageSize.MEDIUM ? 'active' : undefined}>
        <deckgo-ie-action-image class={'image medium'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.styleImage($event.detail, this.setImageWith, ImageSize.SMALL)}
        class={this.imageSize === ImageSize.SMALL ? 'active' : undefined}>
        <deckgo-ie-action-image class={'image small'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,

      <deckgo-ie-separator mobile={this.mobile}></deckgo-ie-separator>,

      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.styleImage($event.detail, this.setImageAlignment, ImageAlign.STANDARD)}
        class={this.imageAlign === ImageAlign.STANDARD ? 'active' : undefined}>
        <deckgo-ie-action-image class={'image-align standard'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.styleImage($event.detail, this.setImageAlignment, ImageAlign.START)}
        class={this.imageAlign === ImageAlign.START ? 'active' : undefined}>
        <deckgo-ie-action-image class={'image-align start'}></deckgo-ie-action-image>
        <div></div>
      </deckgo-ie-action-button>,

      <deckgo-ie-separator mobile={this.mobile}></deckgo-ie-separator>,

      <deckgo-ie-action-button mobile={this.mobile} onAction={($event: CustomEvent<UIEvent>) => this.deleteImage($event.detail)}>
        <deckgo-ie-action-image class={'image-delete'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>
    ];
  }
}
