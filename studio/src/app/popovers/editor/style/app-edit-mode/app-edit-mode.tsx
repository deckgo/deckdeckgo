import {Component, h} from '@stencil/core';

import settingsStore from '../../../../stores/settings.store';

@Component({
  tag: 'app-edit-mode',
  styleUrl: 'app-edit-mode.scss',
})
export class AppEditMode {
  private switchEditMode($event: CustomEvent) {
    if ($event && $event.detail) {
      settingsStore.state.editMode = $event.detail.value;
    }
  }

  render() {
    return (
      <ion-segment mode="ios" color="medium" value={settingsStore.state.editMode} onIonChange={($event: CustomEvent) => this.switchEditMode($event)}>
        <ion-segment-button value="properties">Properties</ion-segment-button>
        <ion-segment-button value="css">CSS</ion-segment-button>
      </ion-segment>
    );
  }
}
