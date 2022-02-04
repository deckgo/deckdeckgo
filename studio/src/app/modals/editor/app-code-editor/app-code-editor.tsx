import {Component, ComponentInterface, Element, Fragment, h, Listen, Prop} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

import '@deckdeckgo/monaco-editor';

import type {MonacoEditorOptions} from '@deckdeckgo/monaco-editor';

// @ts-ignore
self.MonacoEnvironment = {
  getWorkerUrl: function (_moduleId, label) {
    if (label === 'json') {
      return './build/json.worker.js';
    }
    if (label === 'css' || label === 'scss' || label === 'less') {
      return './build/css.worker.js';
    }
    if (label === 'html' || label === 'handlebars' || label === 'razor') {
      return './build/html.worker.js';
    }
    if (label === 'typescript' || label === 'javascript') {
      return './build/ts.worker.js';
    }
    return './build/editor.worker.js';
  }
};

@Component({
  tag: 'app-code-editor',
  styleUrl: 'app-code-editor.scss'
})
export class AppCodeEditor implements ComponentInterface {
  @Element() el: HTMLElement;

  @Prop()
  code: string;

  @Prop()
  options: MonacoEditorOptions;

  private codeEditor: HTMLDeckgoMonacoEditorElement | null;

  componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  @Listen('editorDidLoad')
  onEditorDidLoad() {
    setTimeout(async () => await this.codeEditor?.setFocus(), 500);
  }

  private async closeModal(data?: {code: string}) {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(data);
  }

  private async save() {
    const code: string | undefined = await this.codeEditor?.save();
    await this.closeModal({code});
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="dark">
            <ion-buttons slot="start">
              <ion-button onClick={async () => await this.closeModal()} aria-label={i18n.state.core.close}>
                <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
              </ion-button>
            </ion-buttons>
            <ion-title class="ion-text-uppercase">{i18n.state.editor.code}</ion-title>
          </ion-toolbar>
        </ion-header>

        <main class="ion-padding">
          <deckgo-monaco-editor ref={(el: HTMLDeckgoMonacoEditorElement | null) => (this.codeEditor = el)} options={this.options}>
            <code innerHTML={this.code}></code>
          </deckgo-monaco-editor>
        </main>

        <ion-footer>
          <ion-toolbar>
            <div class="ion-padding-bottom">
              <ion-button color="dark" shape="round" onClick={async () => await this.save()}>
                <ion-label>{i18n.state.core.save}</ion-label>
              </ion-button>
            </div>
          </ion-toolbar>
        </ion-footer>
      </Fragment>
    );
  }
}
