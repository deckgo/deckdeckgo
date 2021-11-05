import {Component, Element, Prop, h, EventEmitter, Event} from '@stencil/core';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

import settingsStore from '../../../../../../stores/settings.store';
import i18n from '../../../../../../stores/i18n.store';

import {EditAction} from '../../../../../../types/editor/edit-action';
import {ImageAction} from '../../../../../../types/editor/image-action';
import {Expanded} from '../../../../../../types/core/settings';

import {SettingsUtils} from '../../../../../../utils/core/settings.utils';
import {AppAssetChoice} from '../../../../common/app-asset-choice/app-asset-choice';

@Component({
  tag: 'app-image-choice',
  styleUrl: 'app-image-choice.scss'
})
export class AppImageChoice {
  @Element() el: HTMLElement;

  @Event() private action: EventEmitter<ImageAction>;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  slide: boolean = false;

  @Prop()
  deleteBackground: boolean = true;

  @Prop()
  deck: boolean = false;

  private async selectAction(action: EditAction, image?: UnsplashPhoto | TenorGif | StorageFile | Waves) {
    const data: ImageAction = {
      action: action
    };

    if (image) {
      data['image'] = image;
    }

    this.action.emit(data);
  }

  private async selectImageFromHistory($event: CustomEvent<UnsplashPhoto | TenorGif | StorageFile | Waves>): Promise<void> {
    if (!$event || !$event.detail) {
      return;
    }

    await this.selectAction(EditAction.ADD_IMAGE, $event.detail);
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.image}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({image: $event.detail})}>
        <ion-label slot="title">{i18n.state.editor.images}</ion-label>

        <div class="image-actions ion-margin">
          <AppAssetChoice
            selectAction={async (action: EditAction, image?: UnsplashPhoto | TenorGif | StorageFile | Waves) =>
              await this.selectAction(action, image)
            }></AppAssetChoice>
          {this.renderWaves()}
          {this.renderDeleteAction()}
        </div>

        <app-image-history
          slide={this.slide}
          deck={this.deck}
          onSelectImage={async ($event: CustomEvent<UnsplashPhoto | TenorGif | StorageFile | Waves>) =>
            await this.selectImageFromHistory($event)
          }></app-image-history>
      </app-expansion-panel>
    );
  }

  private renderWaves() {
    if (!this.deck && !this.slide) {
      // Waves only available for background
      return undefined;
    }

    return (
      <ion-button shape="round" onClick={async () => await this.selectAction(EditAction.OPEN_SVG_WAVES)} color="quaternary">
        <ion-label>{i18n.state.editor.waves}</ion-label>
      </ion-button>
    );
  }

  private renderDeleteAction() {
    if ((!this.deck && !this.slide) || !this.deleteBackground) {
      return undefined;
    } else {
      return (
        <ion-button shape="round" onClick={async () => await this.selectAction(EditAction.DELETE_BACKGROUND)} fill="outline" class="delete">
          <ion-label>{i18n.state.core.reset}</ion-label>
        </ion-button>
      );
    }
  }
}
