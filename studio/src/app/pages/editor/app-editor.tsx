import {Component, ComponentInterface, Fragment, h, Listen, State} from '@stencil/core';

import {isIOS} from '@deckdeckgo/utils';

import {Editor} from '../../types/editor/editor';

import {getEdit} from '../../utils/editor/editor.utils';

@Component({
  tag: 'app-editor',
  styleUrl: 'app-editor.scss'
})
export class AppDeckEditor implements ComponentInterface {
  @State()
  private type: 'deck' | 'doc' | undefined;

  @State()
  private hideNavigation: boolean = false;

  @Listen('ionRouteDidChange', {target: 'window'})
  async onRouteDidChange($event: CustomEvent) {
    // ionViewDidEnter and ionViewDidLeave, kind of
    if ($event?.detail?.to === '/') {
      await this.init();
    } else if ($event?.detail?.from === '/') {
      await this.destroy();
    }
  }

  private async init() {
    const editor: Editor | undefined = await getEdit();
    this.type = editor?.type || 'deck';
  }

  private async destroy() {
    this.type = undefined;
  }

  @Listen('stickyToolbarActivated')
  onStickyToolbarActivated($event: CustomEvent<boolean>) {
    this.hideNavigation = $event ? isIOS() && $event.detail : false;
  }

  render() {
    return (
      <Fragment>
        <app-navigation class={this.hideNavigation ? 'hidden' : undefined}></app-navigation>

        {this.renderEditor()}
      </Fragment>
    );
  }

  private renderEditor() {
    if (this.type === undefined) {
      return <app-spinner></app-spinner>;
    }

    return this.type === 'doc' ? <app-doc-editor></app-doc-editor> : <app-deck-editor></app-deck-editor>;
  }
}
