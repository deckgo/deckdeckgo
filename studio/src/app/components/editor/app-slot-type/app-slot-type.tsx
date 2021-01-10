import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../types/editor/slot-type';
import {SlotUtils} from '../../../utils/editor/slot.utils';

@Component({
  tag: 'app-slot-type',
  styleUrl: 'app-slot-type.scss',
})
export class AppSlotType {
  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  skip: boolean = false;

  @State()
  private currentType: SlotType;

  @State()
  private onlyTextTypes: boolean = false;

  @Event() private selectType: EventEmitter<SlotType | null>;

  async componentWillLoad() {
    if (this.selectedElement) {
      if (SlotUtils.isNodeRevealList(this.selectedElement)) {
        await this.initCurrentTypeList();
      } else {
        await this.initCurrentType();
      }
    }

    this.onlyTextTypes = this.selectedElement?.parentElement?.nodeName?.toLowerCase() === 'deckgo-slide-poll';
  }

  private initCurrentType(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // prettier-ignore
      const element: HTMLElement = SlotUtils.isNodeReveal(this.selectedElement) ? this.selectedElement.firstElementChild as HTMLElement : this.selectedElement;

      if (element.nodeName && element.nodeName !== '') {
        this.currentType = await this.initSlotType(element.nodeName.toLowerCase());
      }

      resolve();
    });
  }

  private initCurrentTypeList(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.currentType = this.selectedElement.getAttribute('list-tag') === SlotType.UL ? SlotType.UL : SlotType.OL;

      resolve();
    });
  }

  private initSlotType(type: string): Promise<SlotType> {
    return new Promise<SlotType>((resolve) => {
      const templateKey: string = Object.keys(SlotType).find((key: string) => {
        return type === SlotType[key];
      });

      resolve(SlotType[templateKey]);
    });
  }

  private select(type: SlotType | null) {
    this.selectType.emit(type && this.currentType !== type ? type : null);
  }

  render() {
    return (
      <ion-list class="article">
        <a onClick={() => this.select(SlotType.H1)} class={this.currentType === SlotType.H1 ? 'current' : ''}>
          <ion-item>
            <ion-icon name="text-outline" slot="start"></ion-icon>
            <h1>Huge title</h1>
          </ion-item>
        </a>
        <a onClick={() => this.select(SlotType.H2)} class={this.currentType === SlotType.H2 ? 'current' : ''}>
          <ion-item>
            <span class="placeholder" slot="start">
              &nbsp;
            </span>
            <h2>Large title</h2>
          </ion-item>
        </a>
        <a onClick={() => this.select(SlotType.H3)} class={this.currentType === SlotType.H3 ? 'current' : ''}>
          <ion-item>
            <span class="placeholder" slot="start">
              &nbsp;
            </span>
            <h3>Small title</h3>
          </ion-item>
        </a>
        <a onClick={() => this.select(SlotType.SECTION)} class={this.currentType === SlotType.SECTION ? 'current' : ''}>
          <ion-item>
            <span class="placeholder" slot="start">
              &nbsp;
            </span>
            <p>Paragraph</p>
          </ion-item>
        </a>
        {this.renderComplexTypes()}

        {this.renderSkip()}
      </ion-list>
    );
  }

  private renderSkip() {
    if (!this.skip) {
      return;
    }

    return (
      <a class="skip" onClick={() => this.select(null)}>
        <ion-item>
          <p>Skip second element</p>
        </ion-item>
      </a>
    );
  }

  private renderComplexTypes() {
    if (this.onlyTextTypes) {
      return undefined;
    }

    return [
      <a onClick={() => this.select(SlotType.OL)} class={this.currentType === SlotType.OL ? 'current' : ''}>
        <ion-item>
          <ion-icon src="/assets/icons/ionicons/list.svg" slot="start"></ion-icon>
          <ion-label>List</ion-label>
        </ion-item>
      </a>,
      <a onClick={() => this.select(SlotType.IMG)} class={this.currentType === SlotType.IMG ? 'current' : ''}>
        <ion-item>
          <ion-icon src="/assets/icons/ionicons/images.svg" slot="start"></ion-icon>
          <ion-label>Image</ion-label>
        </ion-item>
      </a>,
      <a onClick={() => this.select(SlotType.CODE)} class={this.currentType === SlotType.CODE ? 'current' : ''}>
        <ion-item>
          <ion-icon src="/assets/icons/ionicons/code.svg" slot="start"></ion-icon>
          <ion-label>Code</ion-label>
        </ion-item>
      </a>,
      // <a onClick={() => this.select(SlotType.MARKDOWN)} class={this.currentType === SlotType.MARKDOWN ? 'current' : ''}>
      //   <ion-item>
      //     <ion-icon src="/assets/icons/markdown.svg" slot="start"></ion-icon>
      //     <ion-label>Markdown</ion-label>
      //   </ion-item>
      // </a>,
      <a onClick={() => this.select(SlotType.MATH)} class={this.currentType === SlotType.MATH ? 'current' : ''}>
        <ion-item>
          <ion-icon src="/assets/icons/math.svg" slot="start"></ion-icon>
          <ion-label>Math</ion-label>
        </ion-item>
      </a>,
      <a onClick={() => this.select(SlotType.WORD_CLOUD)} class={this.currentType === SlotType.WORD_CLOUD ? 'current' : ''}>
        <ion-item>
          <ion-icon src="/assets/icons/word-cloud.svg" slot="start"></ion-icon>
          <ion-label>Word Cloud</ion-label>
        </ion-item>
      </a>,
    ];
  }
}
