import {Component, Event, EventEmitter, Fragment, h, JSX, Prop, State} from '@stencil/core';

import {SlotType} from '../../../types/editor/slot-type';
import {SlotUtils} from '../../../utils/editor/slot.utils';
import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-slot-type',
  styleUrl: 'app-slot-type.scss',
})
export class AppSlotType {
  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  skip: boolean = false;

  @Prop()
  slotTypes: SlotType[] | undefined;

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
        {this.renderSlot(
          SlotType.H1,
          <Fragment>
            <ion-icon name="text-outline" slot="start"></ion-icon>
            <h1>{i18n.state.editor.huge_title}</h1>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.H2,
          <Fragment>
            <span class="placeholder" slot="start">
              &nbsp;
            </span>
            <h2>{i18n.state.editor.large_title}</h2>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.H3,
          <Fragment>
            <span class="placeholder" slot="start">
              &nbsp;
            </span>
            <h3>{i18n.state.editor.small_title}</h3>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.SECTION,
          <Fragment>
            <span class="placeholder" slot="start">
              &nbsp;
            </span>
            <p>{i18n.state.editor.paragraph}</p>
          </Fragment>
        )}

        {this.renderComplexTypes()}

        {this.renderSkip()}
      </ion-list>
    );
  }

  private renderSlot(slotType: SlotType, item: JSX.IntrinsicElements) {
    // User or community slides might limits which slot types can be used in the slide
    if (this.slotTypes !== undefined && this.slotTypes.find((type: SlotType) => slotType === type) === undefined) {
      return undefined;
    }

    return (
      <a onClick={() => this.select(slotType)} class={this.currentType === slotType ? 'current' : ''}>
        <ion-item>{item}</ion-item>
      </a>
    );
  }

  private renderSkip() {
    if (!this.skip) {
      return;
    }

    return (
      <a class="skip" onClick={() => this.select(null)}>
        <ion-item>
          <p>{i18n.state.editor.skip}</p>
        </ion-item>
      </a>
    );
  }

  private renderComplexTypes() {
    if (this.onlyTextTypes) {
      return undefined;
    }

    return (
      <Fragment>
        {this.renderSlot(
          SlotType.OL,
          <Fragment>
            <ion-icon src="/assets/icons/ionicons/list.svg" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.list}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.IMG,
          <Fragment>
            <ion-icon src="/assets/icons/ionicons/images.svg" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.image}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.CODE,
          <Fragment>
            <ion-icon src="/assets/icons/ionicons/code.svg" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.code}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.MARKDOWN,
          <Fragment>
            <ion-icon src="/assets/icons/markdown.svg" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.markdown}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.MATH,
          <Fragment>
            <ion-icon src="/assets/icons/math.svg" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.math}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.WORD_CLOUD,
          <Fragment>
            <ion-icon src="/assets/icons/word-cloud.svg" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.word_cloud}</ion-label>
          </Fragment>
        )}
      </Fragment>
    );
  }
}
