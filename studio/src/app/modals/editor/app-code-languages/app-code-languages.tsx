import {Component, Element, EventEmitter, h, Listen, Prop, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {filterCodeLanguages} from '../../../utils/editor/prism.utils';

import {PrismLanguage} from '../../../types/editor/prism-language';

@Component({
  tag: 'app-code-languages',
  styleUrl: 'app-code-languages.scss'
})
export class AppCodeLanguages {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  codeDidChange: EventEmitter<HTMLElement>;

  @Prop()
  currentLanguage: PrismLanguage | undefined;

  @State()
  private filteredLanguages: PrismLanguage[];

  @State()
  private filter: string;

  async componentWillLoad() {
    await this.search();
  }

  componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal(selectedLanguage?: PrismLanguage) {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(selectedLanguage);
  }

  private async search() {
    const filtered: PrismLanguage[] = await filterCodeLanguages(this.filter);
    this.filteredLanguages = [...filtered];
  }

  private async clear() {
    this.filter = undefined;

    await this.search();
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.filter = ($event.target as InputTargetEvent).value;
  }

  private selectCodeLanguage(language: PrismLanguage): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (!language) {
        resolve();
        return;
      }

      const currentLanguage: string = this.selectedElement.getAttribute('language');

      if (language.language === currentLanguage) {
        await this.closeModal(language);

        resolve();
        return;
      }

      this.selectedElement.setAttribute('language', language.language);

      // Reload component with new language
      await (this.selectedElement as any).load();

      this.codeDidChange.emit(this.selectedElement);

      await this.closeModal(language);

      resolve();
    });
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="primary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.editor.languages}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list>
          <ion-radio-group value={this.currentLanguage ? this.currentLanguage.language : undefined}>{this.renderLanguages()}</ion-radio-group>
        </ion-list>
      </ion-content>,
      <ion-footer>
        <ion-toolbar>
          <ion-searchbar
            debounce={500}
            placeholder={i18n.state.editor.filter_languages}
            value={this.filter}
            onIonClear={() => this.clear()}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
            onIonChange={() => {
              this.search();
            }}></ion-searchbar>
        </ion-toolbar>
      </ion-footer>
    ];
  }

  private renderLanguages() {
    if (this.filteredLanguages) {
      return this.filteredLanguages.map((language: PrismLanguage) => {
        return (
          <ion-item key={language.language} onClick={() => this.selectCodeLanguage(language)} class="ion-margin-end">
            <ion-label>{language.title}</ion-label>
            <ion-radio value={language.language} mode="ios" />
          </ion-item>
        );
      });
    } else {
      return undefined;
    }
  }
}
