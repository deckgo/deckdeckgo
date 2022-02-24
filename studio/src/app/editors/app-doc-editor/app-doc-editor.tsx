import {throwError} from '@deckdeckgo/editor';
import {DocEvents} from '@deckdeckgo/sync';
import {modalController} from '@ionic/core';
import {StyloConfig, StyloPaletteColor} from '@papyrs/stylo';
import {Component, ComponentInterface, Fragment, h, Listen, Method, State} from '@stencil/core';
import {editorConfig} from '../../config/editor';
import {CodeEvents} from '../../events/editor/code/code.events';
import authStore from '../../stores/auth.store';
import busyStore from '../../stores/busy.store';
import colorStore from '../../stores/color.store';
import i18n from '../../stores/i18n.store';
import {cloud} from '../../utils/core/environment.utils';
import {signIn} from '../../utils/core/signin.utils';
import {ColorUtils} from '../../utils/editor/color.utils';

@Component({
  tag: 'app-doc-editor',
  styleUrl: 'app-doc-editor.scss'
})
export class AppDocEditor implements ComponentInterface {
  @State()
  private styloConfig: Partial<StyloConfig> | undefined;

  private readonly codeEvents: CodeEvents = new CodeEvents();

  private studioEditorRef!: HTMLDeckgoStudioDocElement;

  private i18nListener: () => void | undefined;

  private readonly docEvent: DocEvents = DocEvents.getInstance();

  componentWillLoad() {
    this.updateEditorConfig();

    this.i18nListener = i18n.onChange('lang', () => this.updateEditorConfig());
  }

  async componentDidLoad() {
    this.codeEvents.init();
  }

  async disconnectedCallback() {
    this.codeEvents.destroy();

    this.i18nListener?.();
  }

  private onDocDidLoad = ({detail: containerRef}: CustomEvent<HTMLElement>) => {
    this.docEvent.initDomEvents(containerRef);
    this.docEvent.initDataEvents();
  };

  private onDocDataEvents = ({detail}: CustomEvent<'destroy' | 'init'>) => {
    if (detail === 'destroy') {
      this.docEvent.destroyDataEvents();
      return;
    }

    this.docEvent.initDataEvents();
  };

  @Listen('actionPublish', {target: 'document'})
  async onActionPublish() {
    if (!cloud()) {
      throwError('No cloud provider to publish material.');
      return;
    }

    if (!authStore.state.authUser) {
      signIn();
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-publish',
      cssClass: 'fullscreen'
    });

    await modal.present();
  }

  @Listen('keydown', {target: 'document'})
  onKeyDown($event: KeyboardEvent) {
    const {key, ctrlKey, metaKey} = $event;

    if (key === 'p' && (ctrlKey || metaKey)) {
      this.print($event);
    }
  }

  @Listen('colorChange', {target: 'document', passive: true})
  onColorChange({detail}: CustomEvent<StyloPaletteColor>) {
    ColorUtils.updateColor(detail);

    this.updateEditorConfig();
  }

  private updateEditorConfig() {
    this.styloConfig = {
      ...editorConfig,
      i18n: {
        lang: i18n.state.lang,
        custom: {...i18n.state.editor}
      },
      toolbar: {palette: colorStore.state.history.slice(0, 11)}
    };
  }

  private print(_$event: KeyboardEvent) {
    // TODO: print - Ionic issue
    // if (!this.containerRef) {
    //   return;
    // }
    //
    // $event.preventDefault();
    //
    // printDoc({element: this.containerRef});
  }

  @Method()
  async initNewDoc() {
    await this.studioEditorRef?.initNewDoc();
  }

  render() {
    return (
      <Fragment>
        <ion-content class={`ion-no-padding`}>
          <main>
            {this.renderLoading()}

            <deckgo-studio-doc
              ref={(el) => (this.studioEditorRef = el as HTMLDeckgoStudioDocElement)}
              styloConfig={this.styloConfig}
              onDocDidLoad={this.onDocDidLoad}
              onDocDataEvents={this.onDocDataEvents}></deckgo-studio-doc>

            <deckgo-doc-indicator busy={busyStore.state.busy}></deckgo-doc-indicator>
          </main>
        </ion-content>
      </Fragment>
    );
  }

  private renderLoading() {
    if (busyStore.state.docReady) {
      return undefined;
    }

    return <app-spinner></app-spinner>;
  }
}
