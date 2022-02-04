import {Component, Event, EventEmitter, Fragment, h, JSX, Prop, State} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {SlotType} from '../../../../types/editor/slot-type';

import {SlotUtils} from '../../../../utils/editor/slot.utils';

import {AppIcon} from '../../../core/app-icon/app-icon';

@Component({
  tag: 'app-slot-type',
  styleUrl: 'app-slot-type.scss'
})
export class AppSlotType {
  @Prop()
  selectedTarget: HTMLElement | undefined;

  @Prop()
  skip: boolean = false;

  @Prop()
  slotTypes: SlotType[] | undefined = Object.keys(SlotType).map((key: string) => SlotType[key]);

  @State()
  private currentType: SlotType;

  @State()
  private onlyTextTypes: boolean = false;

  @Event() private selectType: EventEmitter<SlotType | null>;

  componentWillLoad() {
    if (!this.selectedTarget) {
      return;
    }

    if (SlotUtils.isNodeRevealList(this.selectedTarget)) {
      this.initCurrentTypeList();
    } else {
      this.initCurrentType();
    }

    this.onlyTextTypes = this.selectedTarget.parentElement?.nodeName?.toLowerCase() === 'deckgo-slide-poll';
  }

  private initCurrentType() {
    // prettier-ignore
    const element: HTMLElement = SlotUtils.isNodeReveal(this.selectedTarget) ? this.selectedTarget.firstElementChild as HTMLElement : this.selectedTarget;

    if (element.nodeName && element.nodeName !== '') {
      this.currentType = this.initSlotType(element.nodeName.toLowerCase());
    }
  }

  private initCurrentTypeList() {
    this.currentType = this.selectedTarget.getAttribute('list-tag') === SlotType.UL ? SlotType.UL : SlotType.OL;
  }

  private initSlotType(type: string): SlotType {
    const templateKey: string = Object.keys(SlotType).find((key: string) => {
      return type === SlotType[key];
    });

    return SlotType[templateKey];
  }

  private select($event: UIEvent, type: SlotType | null) {
    $event.stopPropagation();

    this.selectType.emit(type && this.currentType !== type ? type : null);
  }

  render() {
    return (
      <ion-list class="article ion-no-padding">
        {this.renderSlot(
          SlotType.H1,
          <Fragment>
            <AppIcon name="text" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
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

        {this.renderSlot(
          SlotType.HR,
          <Fragment>
            <AppIcon name="ellipsis-horizontal" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.separator}</ion-label>
          </Fragment>
        )}

        {this.renderSkip()}
      </ion-list>
    );
  }

  private renderSlot(slotType: SlotType, item: JSX.IntrinsicElements) {
    if (this.slotTypes.find((type: SlotType) => slotType === type) === undefined) {
      return undefined;
    }

    return (
      <a onClick={($event: UIEvent) => this.select($event, slotType)} class={this.currentType === slotType ? 'current' : ''}>
        <ion-item>{item}</ion-item>
      </a>
    );
  }

  private renderSkip() {
    if (!this.skip) {
      return;
    }

    return (
      <a class="skip" onClick={($event: UIEvent) => this.select($event, null)}>
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
          SlotType.UL,
          <Fragment>
            <AppIcon name="list" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.list}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.IMG,
          <Fragment>
            <AppIcon name="images" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.image}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.CODE,
          <Fragment>
            <AppIcon name="code" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.code}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.MARKDOWN,
          <Fragment>
            <AppIcon name="markdown" path="icons" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.markdown}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.MATH,
          <Fragment>
            <AppIcon name="math" path="icons" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.math}</ion-label>
          </Fragment>
        )}

        {this.renderSlot(
          SlotType.WORD_CLOUD,
          <Fragment>
            <AppIcon name="word-cloud" path="icons" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
            <ion-label>{i18n.state.editor.word_cloud}</ion-label>
          </Fragment>
        )}
      </Fragment>
    );
  }
}
