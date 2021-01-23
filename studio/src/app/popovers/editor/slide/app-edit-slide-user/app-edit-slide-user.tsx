import {Component, EventEmitter, Fragment, h, Prop, State} from '@stencil/core';

import {InputChangeEventDetail} from '@ionic/core';

import templatesStore from '../../../../stores/templates.store';

import {SelectedElement} from '../../../../types/editor/selected-element';

import {Template, TemplateDataProp} from '../../../../models/data/template';
import {SlideScope} from '../../../../models/data/slide';

interface Property {
  prop: TemplateDataProp;
  value: string | null;
}

@Component({
  tag: 'app-edit-slide-user',
})
export class AppEditSlideUser {
  @Prop()
  selectedElement: SelectedElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @State()
  private stringProperties: Property[] | undefined;

  private template: Template | undefined;

  async componentWillLoad() {
    this.template = this.getTemplates().find((template: Template) => template.data.tag === this.selectedElement.slide?.nodeName);

    this.stringProperties = this.template?.data?.props
      ?.filter((prop: TemplateDataProp) => prop.type === 'string' || prop.type === 'number')
      .map((prop: TemplateDataProp) => {
        return {
          prop,
          value: this.selectedElement.element.getAttribute(prop.name),
        };
      });
  }

  private getTemplates(): Template[] {
    return this.selectedElement?.slide?.scope === SlideScope.COMMUNITY ? templatesStore.state.community : templatesStore.state.user;
  }

  private async onInputCustomUrlChange($event: CustomEvent<InputChangeEventDetail>, prop: TemplateDataProp) {
    const value: string | undefined | null = $event.detail.value;

    if (!value || value === '') {
      this.selectedElement.element.removeAttribute(prop.name);
    } else {
      this.selectedElement.element.setAttribute(prop.name, value);
    }

    this.slideDidChange.emit(this.selectedElement.element);
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
              onIonChange={($event: CustomEvent<InputChangeEventDetail>) => this.onInputCustomUrlChange($event, prop.prop)}></ion-input>
          </ion-item>
        </Fragment>
      );
    });
  }
}
