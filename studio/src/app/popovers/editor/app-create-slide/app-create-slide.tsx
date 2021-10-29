import {Component, Element, Event, EventEmitter, Fragment, h, JSX, State} from '@stencil/core';

import type {SegmentChangeEventDetail} from '@ionic/core';

import {SlideAttributes, SlideTemplate, SlideScope, Deck, Template} from '@deckdeckgo/editor';

import editorStore from '../../../stores/editor.store';
import authStore from '../../../stores/auth.store';
import i18n from '../../../stores/i18n.store';

import {CreateSlidesUtils, InitTemplate} from '../../../utils/editor/create-slides.utils';
import {SlideUtils} from '../../../utils/editor/slide.utils';

import {SlotType} from '../../../types/editor/slot-type';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-create-slide',
  styleUrl: 'app-create-slide.scss'
})
export class AppCreateSlide {
  @Element() el: HTMLElement;

  @State()
  private composeTemplate: InitTemplate = undefined;

  @State()
  private elements: SlotType[] | undefined = undefined;

  @State()
  private templatesCategory: 'default' | 'community' | 'user' = 'default';

  @Event() signIn: EventEmitter<void>;

  private async addSlide(template: SlideTemplate, deck?: Deck, elements?: SlotType[], style?: {[key: string]: string}) {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide({template, elements, style}, deck);
    await this.closePopover(template, slide);
  }

  private async addSlideSplit(template: SlideTemplate, attributes?: SlideAttributes, elements?: SlotType[]) {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideSplit(elements, attributes);
    await this.closePopover(template, slide);
  }

  private async addSlideQRCode() {
    await this.addSlide(SlideTemplate.QRCODE, editorStore.state.deck);
  }

  // We need the data in the user account (like twitter, profile image etc.) to generate the author slide
  // User template is also only possible if user is logged in
  private async addRestrictedSlide(template: SlideTemplate | Template, scope: SlideScope = SlideScope.DEFAULT, elements?: SlotType[]) {
    if (!authStore.state.authUser) {
      this.signIn.emit();
      await this.closePopover(null);
      return;
    }

    await this.addSlideTemplate(template, scope, elements);
  }

  private async addSlideTemplate(template: SlideTemplate | Template, scope: SlideScope = SlideScope.DEFAULT, elements?: SlotType[]) {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide({template, scope: scope, elements});
    await this.closePopover(template, slide);
  }

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async closePopover(template: SlideTemplate | Template, slide?: JSX.IntrinsicElements, attributes?: SlideAttributes) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      template,
      slide,
      attributes
    });
  }

  private async closePopoverOpenTemplateOptions(template: SlideTemplate | Template, attributes?: SlideAttributes) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      template,
      attributes
    });
  }

  private async selectSlideSlottedElements(slotType: SlotType | null) {
    if (this.elements === undefined && !slotType) {
      return;
    }

    if (!this.composeTemplate) {
      return;
    }

    const slideTemplate: SlideTemplate | undefined = this.getSlideTemplate();

    const customSlotsCount: number | undefined = (this.composeTemplate?.template as Template)?.data?.slots?.length;

    // We want to rerender only when needed and not when the last slot is selected
    let elements: SlotType[] = this.elements ? [...this.elements] : undefined;

    // First element
    if (this.elements === undefined) {
      elements = [slotType];

      if (slideTemplate !== undefined || customSlotsCount > 1) {
        this.elements = [...elements];
        return;
      }
    }

    // We might just want only one element
    if (slotType && (customSlotsCount === undefined || customSlotsCount > 1)) {
      elements.push(slotType);

      if (
        SlideUtils.isSlideTemplate(this.composeTemplate.scope) &&
        this.elements.length < (this.composeTemplate?.template as Template)?.data?.slots?.length - 1
      ) {
        this.elements = [...elements];
        return;
      }
    }

    // We've got all, or at least one, the elements, go we can create the slide
    if (this.composeTemplate.template === SlideTemplate.SPLIT) {
      await this.addSlideSplit(SlideTemplate.SPLIT, this.composeTemplate.attributes, elements);
    } else if (this.composeTemplate.template === SlideTemplate.CONTENT) {
      await this.addSlide(SlideTemplate.CONTENT, undefined, elements, this.composeTemplate.style);
    } else if (this.composeTemplate.scope === SlideScope.USER) {
      await this.addRestrictedSlide(this.composeTemplate.template, SlideScope.USER, elements);
    } else if (this.composeTemplate.scope === SlideScope.COMMUNITY) {
      await this.addSlideTemplate(this.composeTemplate.template, SlideScope.COMMUNITY, elements);
    } else {
      await this.addSlide(SlideTemplate.TITLE, undefined, elements);
    }
  }

  private backCompose() {
    if (this.elements === undefined) {
      this.composeTemplate = undefined;
    } else {
      this.elements = undefined;
    }
  }

  private getSlideTemplate(): SlideTemplate | undefined {
    return this.composeTemplate.scope === undefined
      ? SlideTemplate[(this.composeTemplate.template as SlideTemplate)?.toUpperCase()]
      : undefined;
  }

  private async selectTemplateUser($event: CustomEvent<Template>) {
    const template: Template = $event.detail;

    if (!template || !template.data) {
      return;
    }

    const customSlotsCount: number | undefined = template.data.slots?.length;

    if (customSlotsCount !== undefined && customSlotsCount > 0) {
      this.composeTemplate = {
        template,
        scope: SlideScope.USER
      };
      return;
    }

    // User has created a template without any editable slots
    await this.addRestrictedSlide(template, SlideScope.USER);
  }

  render() {
    return (
      <Fragment>
        <ion-toolbar>
          {this.renderToolbarTitle()}
          {this.renderToolbarAction()}
        </ion-toolbar>

        {this.renderTemplatesCategory()}

        {this.renderTemplatesDefault()}
        {this.renderTemplatesCommunity()}
        {this.renderTemplatesUser()}
        {this.renderCompose()}
      </Fragment>
    );
  }

  private renderTemplatesCategory() {
    return (
      <ion-segment
        mode="md"
        value={this.templatesCategory}
        color="dark"
        onIonChange={($event: CustomEvent<SegmentChangeEventDetail>) =>
          (this.templatesCategory = $event?.detail?.value as 'default' | 'community' | 'user')
        }
        disabled={this.composeTemplate !== undefined}>
        <ion-segment-button mode="md" value="default">
          <ion-label>{i18n.state.editor.default}</ion-label>
        </ion-segment-button>

        <ion-segment-button mode="md" value="community">
          <ion-label>{i18n.state.editor.community}</ion-label>
        </ion-segment-button>

        <ion-segment-button mode="md" value="user">
          <ion-label>{i18n.state.editor.yours}</ion-label>
        </ion-segment-button>
      </ion-segment>
    );
  }

  private renderToolbarTitle() {
    if (this.composeTemplate == undefined) {
      return <h2>{i18n.state.editor.add_new_slide}</h2>;
    }

    return (
      <h2>{this.composeTemplate?.template === SlideTemplate.CHART ? i18n.state.editor.select_chart : i18n.state.editor.compose_slide}</h2>
    );
  }

  private renderToolbarAction() {
    if (this.composeTemplate == undefined) {
      return <app-close-menu slot="end" onClose={() => this.closePopoverWithoutResults()}></app-close-menu>;
    }

    return (
      <app-close-menu slot="start" onClose={() => this.backCompose()}>
        <AppIcon name="arrow-back" ariaLabel={i18n.state.editor.back_to_slides}></AppIcon>
      </app-close-menu>
    );
  }

  private renderTemplatesCommunity() {
    if (this.composeTemplate !== undefined) {
      return undefined;
    }

    if (this.templatesCategory !== 'community') {
      return undefined;
    }

    return (
      <app-templates-community
        class="container ion-margin-bottom"
        onSelectedTemplate={($event: CustomEvent<Template>) =>
          (this.composeTemplate = {
            template: $event.detail,
            scope: SlideScope.COMMUNITY
          })
        }></app-templates-community>
    );
  }

  private renderTemplatesUser() {
    if (this.composeTemplate !== undefined) {
      return undefined;
    }

    if (this.templatesCategory !== 'user') {
      return undefined;
    }

    return (
      <app-templates-user
        class="container ion-margin-bottom"
        onSelectedTemplate={async ($event: CustomEvent<Template>) => await this.selectTemplateUser($event)}
        onNavigateSignIn={() => this.closePopoverWithoutResults()}></app-templates-user>
    );
  }

  private renderTemplatesDefault() {
    if (this.composeTemplate !== undefined) {
      return undefined;
    }

    if (this.templatesCategory !== 'default') {
      return undefined;
    }

    return (
      <app-templates-default
        class="container ion-margin-bottom"
        onSelectedTemplate={($event: CustomEvent) =>
          this.closePopoverOpenTemplateOptions($event.detail?.template, $event.detail?.attributes)
        }
        onAddSlideQRCode={() => this.addSlideQRCode()}
        onAddSlideAuthor={() => this.addRestrictedSlide(SlideTemplate.AUTHOR)}
        onAddSlideAspectRatio={() => this.addSlide(SlideTemplate['ASPECT-RATIO'])}
        onComposeTemplate={($event: CustomEvent<InitTemplate>) => (this.composeTemplate = $event.detail)}
        onSelectCharts={() => (this.composeTemplate = {template: SlideTemplate.CHART})}></app-templates-default>
    );
  }

  private renderCompose() {
    if (this.composeTemplate === undefined) {
      return undefined;
    }

    if (this.composeTemplate.template === SlideTemplate.CHART) {
      return (
        <app-templates-charts
          class="container ion-margin-bottom"
          onSelectedTemplate={($event: CustomEvent) =>
            this.closePopover($event.detail?.template, null, $event.detail?.attributes)
          }></app-templates-charts>
      );
    }

    return (
      <div class="container compose ion-margin-bottom">
        {this.renderComposeSlide()}
        {this.renderSlotType()}
      </div>
    );
  }

  private renderComposeSlide() {
    const slideTemplate: SlideTemplate | undefined = this.getSlideTemplate();

    const attr = {
      highlight: true,
      highlightIndex: this.elements?.length
    };

    if (slideTemplate === SlideTemplate.CONTENT) {
      return (
        <app-templates-content
          highlight={true}
          highlightIndex={this.elements?.length}
          {...attr}
          style={this.composeTemplate.style}></app-templates-content>
      );
    } else if (slideTemplate === SlideTemplate.SPLIT) {
      return <app-templates-split vertical={this.composeTemplate.attributes !== undefined} {...attr}></app-templates-split>;
    } else if (SlideUtils.isSlideTemplate(this.composeTemplate.scope)) {
      return <app-template-showcase template={this.composeTemplate.template as Template}></app-template-showcase>;
    } else if (slideTemplate === SlideTemplate.TITLE) {
      return <app-templates-title {...attr}></app-templates-title>;
    }

    return undefined;
  }

  private renderSlotType() {
    const slideTemplate: SlideTemplate | undefined = this.getSlideTemplate();

    const skip: boolean = this.elements !== undefined && slideTemplate !== SlideTemplate.SPLIT;

    const slotTypes: SlotType[] | undefined = this.slotTypes();

    return (
      <app-slot-type
        skip={skip}
        slotTypes={slotTypes}
        onSelectType={($event: CustomEvent<SlotType>) => this.selectSlideSlottedElements($event.detail)}></app-slot-type>
    );
  }

  private slotTypes(): SlotType[] | undefined {
    if (!this.composeTemplate || !this.composeTemplate.scope || this.composeTemplate.scope === SlideScope.DEFAULT) {
      return undefined;
    }

    return (this.composeTemplate.template as Template).data.slots?.[this.elements ? this.elements.length : 0].types as SlotType[];
  }
}
