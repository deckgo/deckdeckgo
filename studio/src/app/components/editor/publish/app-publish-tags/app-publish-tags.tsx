import {Component, EventEmitter, Prop, Event, h} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {AppIcon} from '../../../core/app-icon/app-icon';

@Component({
  tag: 'app-publish-tags',
  styleUrl: 'app-publish-tags.scss',
  shadow: false
})
export class AppPublishTags {
  @Prop()
  tags: string[] = [];

  @Prop()
  disableRemove: boolean = false;

  @Event() private removeTag: EventEmitter<string>;

  private remove($event: UIEvent, tag: string) {
    $event.preventDefault();

    if (this.disableRemove) {
      return;
    }

    this.removeTag.emit(tag);
  }

  render() {
    if (!this.tags || this.tags.length <= 0) {
      return undefined;
    } else {
      return this.tags.map((tag: string) => {
        return (
          <div class="chips">
            {this.renderCloseTags(tag)}
            <ion-label>{tag}</ion-label>
          </div>
        );
      });
    }
  }

  private renderCloseTags(tag: string) {
    return (
      <button onClick={($event: UIEvent) => this.remove($event, tag)} disabled={this.disableRemove} aria-label={i18n.state.core.close}>
        <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
      </button>
    );
  }
}
