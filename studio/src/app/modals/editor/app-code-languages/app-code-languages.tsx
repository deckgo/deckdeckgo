import {Component, Element, EventEmitter, h, Listen, Prop, State} from '@stencil/core';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

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

  private prismService: PrismService;

  @State()
  private languages: PrismLanguage[];

  @State()
  private filteredLanguages: PrismLanguage[];

  @State()
  private filter: string;

  constructor() {
    this.prismService = PrismService.getInstance();
  }

  async componentWillLoad() {
    await this.load();
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

  private load(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.languages = await this.prismService.getLanguages();

      await this.search();

      resolve();
    });
  }

  private search(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const filtered: PrismLanguage[] | undefined = await this.filterLanguages();
      this.filteredLanguages = [...filtered];

      resolve();
    });
  }

  private filterLanguages(): Promise<PrismLanguage[]> {
    return new Promise<PrismLanguage[]>((resolve) => {
      if (!this.languages || this.languages.length <= 0) {
        resolve([]);
        return;
      }

      if (!this.filter || this.filter === undefined || this.filter === '') {
        resolve(this.languages);
        return;
      }

      const results: PrismLanguage[] = this.languages.filter((language: PrismLanguage) => {
        return language.language.toLowerCase().indexOf(this.filter.toLowerCase()) > -1;
      });

      resolve(results);
    });
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
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon name="close"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Languages</ion-title>
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
            placeholder="Filter languages"
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
