import {SlideScope, Template, TemplateDataProp} from '@deckdeckgo/editor';
import type {InputChangeEventDetail} from '@ionic/core';
import {Component, EventEmitter, Fragment, h, Prop, State} from '@stencil/core';
import templatesStore from '../../../../stores/templates.store';
import {SelectedTarget} from '../../../../types/editor/selected-target';

interface Property {
  prop: TemplateDataProp;
  value: string | null;
}

@Component({
  tag: 'app-edit-slide-user'
})
export class AppEditSlideUser {
  @Prop()
  selectedTarget: SelectedTarget;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @State()
  private stringProperties: Property[] | undefined;

  private template: Template | undefined;

  async componentWillLoad() {
    this.template = this.getTemplates().find((template: Template) => template.data.tag === this.selectedTarget.slide?.nodeName);

    this.stringProperties = this.template?.data?.props
      ?.filter((prop: TemplateDataProp) => prop.type === 'string' || prop.type === 'number')
      .map((prop: TemplateDataProp) => {
        return {
          prop,
          value: this.selectedTarget.target.getAttribute(prop.name)
        };
      });
  }

  private getTemplates(): Template[] {
    return this.selectedTarget?.slide?.scope === SlideScope.COMMUNITY ? templatesStore.state.community : templatesStore.state.user;
  }

  private async onInputCustomUrlChange($event: CustomEvent<InputChangeEventDetail>, prop: TemplateDataProp) {
    const value: string | undefined | null = $event.detail.value;

    if (!value || value === '') {
      this.selectedTarget.target.removeAttribute(prop.name);
    } else {
      this.selectedTarget.target.setAttribute(prop.name, value);
    }

    this.slideDidChange.emit(this.selectedTarget.target);
  }

  render() {
    if (!this.template) {
      return undefined;
    }

    return this.stringProperties?.map((prop: Property) => {
      return (
        <Fragment>
          {prop.prop.placeholder ? (
            <ion-item-divider>
              <ion-label>{prop.prop.placeholder}</ion-label>
            </ion-item-divider>
          ) : undefined}

          <ion-item class="with-padding">
            <ion-input
              value={prop.value}
              debounce={500}
              onIonChange={($event: CustomEvent<InputChangeEventDetail>) => this.onInputCustomUrlChange($event, prop.prop)}
            ></ion-input>
          </ion-item>
        </Fragment>
      );
    });
  }
}
