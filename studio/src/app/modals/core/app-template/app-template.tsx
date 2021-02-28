import {Component, Element, Fragment, h, Listen, Prop, State} from '@stencil/core';

import authStore from '../../../stores/auth.store';
import navStore, {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';

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
  private templateData: Partial<TemplateData> | undefined;

  @State()
  private validCdn: boolean = false;

  @State()
  private validTag: boolean = false;

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

    await this.validateCDNInput();
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

    this.templateData = {...this.templateData};
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
          ...(cmp && cmp.props && {props: cmp.props}),
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

  private async validateCDNInput() {
    try {
      const url: URL = new URL(this.templateData.cdn);

      this.validCdn = /unpkg\.com|cdnjs\.cloudflare\.com|cdn\.jsdelivr\.net/g.test(url.hostname);
    } catch (err) {
      this.validCdn = false;
    }
  }

  private async validateTag() {
    this.validTag = !/(?:deckgo|deckdeckgo|ddg)/.test(this.templateData.tag);
  }

  private async navigateContact() {
    navStore.state.nav = {
      url: '/contact',
      direction: NavDirection.FORWARD,
    };

    await this.closeModal();
  }

  render() {
    const errorCdn: string | undefined = !this.validCdn && this.templateData?.cdn !== '' && this.templateData?.cdn !== undefined ? 'error' : '';
    const errorTag: string | undefined = !this.validTag && this.templateData?.tag !== '' && this.templateData?.tag !== undefined ? 'error' : '';

    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="primary">
            <ion-buttons slot="start">
              <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
                <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
              </ion-button>
            </ion-buttons>
            <ion-title class="ion-text-uppercase">Template</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding" color="light">
          <main class="ion-padding">
            <form onSubmit={(e: Event) => this.handleSubmit(e)}>
              <ion-list class="inputs-list">
                <ion-item class={`item-title ${errorCdn}`}>
                  <ion-label>
                    CDN - ES Modules <span>1</span>
                  </ion-label>
                </ion-item>

                <ion-item>
                  <ion-input
                    value={this.templateData?.cdn}
                    debounce={500}
                    minlength={3}
                    required={true}
                    input-mode="text"
                    onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onCdnInput($event)}
                    onIonChange={() => this.validateCDNInput()}></ion-input>
                </ion-item>

                <ion-item class={`item-title ${errorTag}`}>
                  <ion-label>
                    Tag <span>2</span>
                  </ion-label>
                </ion-item>

                <ion-item>
                  <ion-input
                    value={this.templateData?.tag}
                    debounce={500}
                    minlength={3}
                    required={true}
                    input-mode="text"
                    onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onTagInput($event)}
                    onIonChange={() => this.validateTag()}></ion-input>
                </ion-item>

                <ion-item class="item-title">
                  <ion-label>
                    Definition <span>3</span>
                  </ion-label>
                </ion-item>

                <input
                  ref={(el) => (this.inputFileRef = el as HTMLInputElement)}
                  type="file"
                  accept="application/json"
                  disabled={!this.templateData || !this.templateData.tag}
                  onChange={() => this.onFileInput()}
                />
              </ion-list>

              <ion-button type="submit" color="primary" class="ion-margin-top" shape="round" disabled={!this.validCdn || !this.validTag}>
                <ion-label>Save</ion-label>
              </ion-button>
            </form>

            {this.renderNotes(errorCdn, errorTag)}
          </main>
        </ion-content>
      </Fragment>
    );
  }

  private renderNotes(errorCdn: string, errorTag: string) {
    return (
      <div class="notes">
        <p class={`small`}>
          <span>1</span> URL to the ES Modules as for example <mark>https://unpkg.com/my-component@latest/dist/my-component/my-component.esm.js</mark>.
        </p>

        <p class={`small ${errorCdn}`}>
          We support Unpkg, Cloudfare and jsDelivr. If you would like to use another CDN, <a onClick={() => this.navigateContact()}>get in touch</a>.
        </p>

        <p class={`small ${errorTag}`}>
          <span>2</span> The tag should not contain any of the reserved keywords <mark>deckdeckgo</mark>, <mark>deckgo</mark> or <mark>ddg</mark>.
        </p>

        <p class={`small`}>
          <span>3</span> Upload the definition which was automatically generated when you built your template <mark>./src/components.desc.json</mark>.
        </p>
      </div>
    );
  }
}
