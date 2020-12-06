import {Component, Element, Fragment, h, Listen, Prop, State} from '@stencil/core';

import authStore from '../../../stores/auth.store';

import {Template, TemplateData} from '../../../models/data/template';

@Component({
  tag: 'app-template',
  styleUrl: 'app-template.scss',
})
export class AppTemplate {
  @Element() el: HTMLElement;

  @Prop()
  template: Template | undefined;

  @State()
  private templateData: TemplateData | undefined;

  private inputFileRef!: HTMLInputElement;

  async componentWillLoad() {
    if (!this.template || !this.template.data) {
      this.templateData = {
        owner_id: authStore.state.authUser.uid,
      };
      return;
    }

    this.templateData = this.template.data;
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private async handleSubmit($event: Event) {
    $event.preventDefault();

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss({
      ...this.template,
      data: {
        ...this.templateData,
      },
    });
  }

  private async onCdnInput($event: CustomEvent<KeyboardEvent>) {
    if (!this.templateData) {
      return;
    }

    this.templateData.cdn = ($event.target as InputTargetEvent).value;
  }

  private async onTagInput($event: CustomEvent<KeyboardEvent>) {
    if (!this.templateData) {
      return;
    }

    this.templateData = {
      ...this.templateData,
      tag: ($event.target as InputTargetEvent).value,
    };
  }

  private async onFileInput() {
    if (!this.inputFileRef || !this.templateData?.tag) {
      return;
    }

    if (this.inputFileRef.files && this.inputFileRef.files.length > 0) {
      const jsonContent: string = await this.load(this.inputFileRef.files[0]);

      const components: TemplateData[] = JSON.parse(jsonContent);
      if (components && components.length > 0) {
        const cmp: TemplateData | undefined = components.find((cmp: TemplateData) => cmp.tag === this.templateData.tag);
        this.templateData = {
          ...this.templateData,
          ...(cmp && cmp.slots && {slots: cmp.slots}),
        };
      }
    }
  }

  private load(myFile: File): Promise<string> {
    return new Promise<string>((resolve, reject) => {
      const fileReader: FileReader = new FileReader();

      if (fileReader && myFile) {
        fileReader.readAsText(myFile);
        fileReader.onload = () => {
          resolve(fileReader.result as string);
        };
        fileReader.onerror = (error) => {
          reject(error);
        };
      } else {
        reject('No file provided');
      }
    });
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="primary">
            <ion-buttons slot="start">
              <ion-button onClick={() => this.closeModal()}>
                <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
              </ion-button>
            </ion-buttons>
            <ion-title class="ion-text-uppercase">Template</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding">
          <form onSubmit={(e: Event) => this.handleSubmit(e)}>
            <ion-list class="inputs-list">
              <ion-item class="item-title">
                <ion-label>CDN</ion-label>
              </ion-item>

              <ion-item>
                <ion-input
                  value={this.templateData?.cdn}
                  debounce={500}
                  minlength={3}
                  required={true}
                  input-mode="text"
                  onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onCdnInput($event)}></ion-input>
              </ion-item>

              <ion-item class="item-title">
                <ion-label>Tag</ion-label>
              </ion-item>

              <ion-item>
                <ion-input
                  value={this.templateData?.tag}
                  debounce={500}
                  minlength={3}
                  required={true}
                  input-mode="text"
                  onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onTagInput($event)}></ion-input>
              </ion-item>

              <ion-item class="item-title">
                <ion-label>Definition</ion-label>
              </ion-item>

              <input
                ref={(el) => (this.inputFileRef = el as HTMLInputElement)}
                type="file"
                accept="application/json"
                disabled={!this.templateData || !this.templateData.tag}
                onChange={() => this.onFileInput()}
              />
            </ion-list>

            <ion-button type="submit" color="primary" class="ion-margin-top" shape="round">
              <ion-label>Save</ion-label>
            </ion-button>
          </form>
        </ion-content>
      </Fragment>
    );
  }
}
