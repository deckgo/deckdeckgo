import {Component, Element, h, Listen, Prop, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-notes',
  styleUrl: 'app-notes.scss',
})
export class AppNotes {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private notes: string;

  componentWillLoad() {
    if (this.selectedElement) {
      const element: HTMLElement = this.selectedElement.querySelector('[slot="notes"]');

      this.notes = element ? element.innerHTML : undefined;
    }
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(false);
  }

  async save() {
    if (!this.selectedElement) {
      await this.closeModal();
      return;
    }

    if (this.notes === undefined || !this.notes || this.notes === '') {
      await this.closeModal();
      return;
    }

    const text: Text = document.createTextNode(this.notes);

    let element: HTMLElement = this.selectedElement.querySelector('[slot="notes"]');

    if (!element) {
      element = document.createElement('div');
      element.setAttribute('slot', 'notes');

      this.selectedElement.appendChild(element);

      element.appendChild(text);
    } else {
      element.replaceChild(text, element.firstChild);
    }

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(true);
  }

  async delete() {
    if (!this.selectedElement) {
      await this.closeModal();
      return;
    }

    let element: HTMLElement = this.selectedElement.querySelector('[slot="notes"]');

    if (!element) {
      await this.closeModal();
      return;
    }

    element.parentElement.removeChild(element);

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(true);
  }

  private handleNotesInput($event: CustomEvent<KeyboardEvent>) {
    this.notes = ($event.target as InputTargetEvent).value;
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="quinary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Notes</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <p>
          Your notes are displayed in the{' '}
          <a href="https://deckdeckgo.app" target="_blank">
            remote control <ion-icon src="/assets/icons/ionicons/open.svg" role="presentation"></ion-icon>
          </a>
          . Markdown is supported 😉
        </p>

        <ion-list class="ion-no-padding">
          <ion-item>
            <ion-textarea
              rows={16}
              value={this.notes}
              debounce={500}
              class="ion-no-margin"
              maxlength={4096}
              placeholder="The notes related to the current slide"
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleNotesInput(e)}></ion-textarea>
          </ion-item>
        </ion-list>

        <div class="notes-actions ion-margin-top">
          <ion-button color="dark" shape="round" onClick={() => this.save()}>
            <ion-label>Save</ion-label>
          </ion-button>

          <ion-button color="dark" shape="round" onClick={() => this.delete()} fill="outline">
            <ion-label>Delete</ion-label>
          </ion-button>
        </div>
      </ion-content>,
    ];
  }
}
