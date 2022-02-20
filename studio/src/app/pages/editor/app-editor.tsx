import {SyncEvent} from '@deckdeckgo/editor';
import {initSyncState, sync, getEdit} from '@deckdeckgo/studio';
import {isIOS} from '@deckdeckgo/utils';
import {Component, ComponentInterface, Fragment, h, Listen, State} from '@stencil/core';
import {EnvironmentAppConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {Editor} from '../../types/editor/editor';
import {startSyncTimer, stopSyncTimer} from '../../workers/sync.worker';
import {worker} from '../../workers/sync.worker.ts?worker';

@Component({
  tag: 'app-editor',
  styleUrl: 'app-editor.scss'
})
export class AppDeckEditor implements ComponentInterface {
  @State()
  private type: 'deck' | 'doc' | undefined;

  @State()
  private hideNavigation: boolean = false;

  private deckEditorRef: HTMLAppDeckEditorElement;
  private docEditorRef: HTMLAppDocEditorElement;

  @Listen('ionRouteDidChange', {target: 'window'})
  async onRouteDidChange($event: CustomEvent) {
    // ionViewDidEnter and ionViewDidLeave, kind of
    if ($event?.detail?.to === '/') {
      await this.init();
    } else if ($event?.detail?.from === '/') {
      await this.destroy();
    }
  }

  async componentDidLoad() {
    await this.syncData();
  }

  async disconnectedCallback() {
    await stopSyncTimer();
  }

  private async syncData() {
    await startSyncTimer();

    worker.onmessage = async ({data}: MessageEvent<SyncEvent>) => {
      if (!data || data.msg !== 'deckdeckgo_sync') {
        return;
      }

      await sync(data.data);
    };

    await initSyncState();
  }

  private async init() {
    const {features} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');

    const editor: Editor | undefined = await getEdit();
    this.type = editor?.type || features[0];
  }

  private async destroy() {
    this.type = undefined;
  }

  @Listen('toolbarActivated')
  onToolbarActivated($event: CustomEvent<boolean>) {
    this.hideNavigation = $event ? isIOS() && $event.detail : false;
  }

  @Listen('reloadEditor', {target: 'document'})
  async onReloadEditor({detail}: CustomEvent<'deck' | 'doc'>) {
    if (detail !== this.type) {
      this.type = detail;
      return;
    }

    if (detail === 'deck') {
      await this.deckEditorRef?.initNewDeck();
    } else if (detail === 'doc') {
      await this.docEditorRef?.initNewDoc();
    }
  }

  render() {
    return (
      <Fragment>
        <app-navigation actions="all" class={this.hideNavigation ? 'hidden' : undefined}></app-navigation>

        {this.renderEditor()}
      </Fragment>
    );
  }

  private renderEditor() {
    if (this.type === undefined) {
      return <app-spinner></app-spinner>;
    }

    return this.type === 'doc' ? (
      <app-doc-editor ref={(el) => (this.docEditorRef = el as HTMLAppDocEditorElement)}></app-doc-editor>
    ) : (
      <app-deck-editor ref={(el) => (this.deckEditorRef = el as HTMLAppDeckEditorElement)}></app-deck-editor>
    );
  }
}
