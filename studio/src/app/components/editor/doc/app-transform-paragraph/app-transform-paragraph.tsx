import {Component, ComponentInterface, h, Host, Listen, Prop, State} from '@stencil/core';

import {modalController, OverlayEventDetail} from '@ionic/core';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

import {SlotType} from '../../../../types/editor/slot-type';
import {EditAction} from '../../../../types/editor/edit-action';

import {focusParagraph} from '../../../../utils/editor/paragraph.utils';

import {AppAssetChoice} from '../../common/app-asset-choice/app-asset-choice';
import {insertUnorderedList, formatBlock, insertHTML, insertImage} from '../../../../utils/editor/insert-element.utils';

@Component({
  tag: 'app-transform-paragraph',
  styleUrl: 'app-transform-paragraph.scss',
  shadow: false
})
export class AppTransformParagraph implements ComponentInterface {
  @Prop()
  containerRef: HTMLElement | undefined;

  @State()
  private display: boolean = false;

  @State()
  private position: {left: number; top: number; downward: boolean} | undefined = undefined;

  @State()
  private selectImages: boolean = false;

  private paragraph: HTMLElement | undefined | null;

  private slotTypes: SlotType[] = [SlotType.H1, SlotType.H2, SlotType.H3, SlotType.CODE, SlotType.IMG, SlotType.UL];

  componentDidRender() {
    this.display = this.position !== undefined;
  }

  @Listen('keydown', {target: 'document', passive: true})
  onKeyDown(_$event: KeyboardEvent) {
    this.hide();
  }

  @Listen('click', {target: 'document', passive: true})
  onMouseDown(_$event: MouseEvent | TouchEvent) {
    this.hide();
  }

  @Listen('sizeDidChange', {target: 'document', passive: true})
  onSizeDidChange(_$event: CustomEvent<{width: number; height: number}>) {
    this.hide();
  }

  private hide() {
    this.position = undefined;
    this.selectImages = false;
  }

  @Listen('selectParagraph', {target: 'document', passive: true})
  onSelectParagraph({detail: element}: CustomEvent<HTMLElement | undefined>) {
    if (!element) {
      this.hide();
      return;
    }

    const {left, height, top}: DOMRect = element.getBoundingClientRect();

    // top + size + margin
    const downward: boolean = top + 220 + 16 < (window.innerHeight || screen.height);

    this.position = {
      top: element.offsetTop + (downward ? height : -1 * height),
      downward,
      left: left
    };

    this.paragraph = element;
  }

  private transformSlot(slotType: SlotType | null) {
    if (!this.selectImages && slotType === SlotType.IMG) {
      this.selectImages = true;
      return;
    }

    if (!this.containerRef) {
      return;
    }

    focusParagraph({paragraph: this.paragraph});

    if ([SlotType.H1, SlotType.H2, SlotType.H3].includes(slotType)) {
      formatBlock(slotType);
    } else if ([SlotType.OL, SlotType.UL].includes(slotType)) {
      insertUnorderedList();
    } else {
      insertHTML(slotType);
    }

    this.hide();
  }

  private async selectImage(action: EditAction, _image?: UnsplashPhoto | TenorGif | StorageFile | Waves) {
    if (!this.paragraph || !this.containerRef) {
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: action === EditAction.OPEN_UNSPLASH ? 'app-unsplash' : action === EditAction.OPEN_GIFS ? 'app-gif' : 'app-storage-images'
    });

    modal.onDidDismiss().then(async ({data}: OverlayEventDetail<UnsplashPhoto | TenorGif | StorageFile>) => {
      insertImage({image: data, paragraph: this.paragraph, container: this.containerRef});
    });

    await modal.present();

    this.hide();
  }

  render() {
    const style: Record<string, string> =
      this.position === undefined
        ? {}
        : {
            '--actions-top': `${this.position.top}px`,
            '--actions-left': `${this.position.left}px`,
            '--actions-translate-y': `${this.position.downward ? '0' : '-100%'}`
          };

    return (
      <Host style={style} class={`${this.display ? 'display' : 'hidden'} ${this.selectImages ? 'images' : ''}`}>
        {this.renderSlots()}
        {this.renderSelectImages()}
      </Host>
    );
  }

  private renderSlots() {
    if (this.selectImages) {
      return undefined;
    }

    return (
      <app-slot-type
        slotTypes={this.slotTypes}
        onSelectType={({detail}: CustomEvent<SlotType>) => this.transformSlot(detail)}></app-slot-type>
    );
  }

  private renderSelectImages() {
    if (!this.selectImages) {
      return undefined;
    }

    return (
      <AppAssetChoice
        selectAction={async (action: EditAction, image?: UnsplashPhoto | TenorGif | StorageFile | Waves) =>
          await this.selectImage(action, image)
        }></AppAssetChoice>
    );
  }
}
