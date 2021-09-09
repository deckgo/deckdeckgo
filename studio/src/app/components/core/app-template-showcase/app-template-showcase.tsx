import {Component, Prop, h, State, Fragment} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {Template, TemplateDataSlot} from '../../../models/data/template';

import {TemplateUtils} from '../../../utils/editor/template.utils';

import {AppIcon} from '../app-icon/app-icon';

@Component({
  tag: 'app-template-showcase',
  styleUrl: 'app-template-showcase.scss'
})
export class AppTemplateShowcase {
  @Prop()
  template: Template;

  @Prop()
  editable: boolean = false;

  @Prop()
  author: boolean = false;

  @State()
  private loaded: boolean = false;

  async componentDidLoad() {
    await TemplateUtils.loadScript(this.template);

    this.loaded = true;
  }

  private async blockSlide($event: CustomEvent) {
    await ($event?.target as HTMLDeckgoDeckElement).blockSlide(true);
  }

  render() {
    return (
      <Fragment>
        <article>
          {this.loaded ? this.renderTemplate() : this.renderSpinner()}
          {this.loaded ? this.renderEdit() : undefined}
        </article>
        {this.renderAuthor()}
      </Fragment>
    );
  }

  private renderTemplate() {
    const Element = this.template.data.tag;

    return (
      <deckgo-deck embedded={true} keyboard={false} onSlidesDidLoad={($event: CustomEvent) => this.blockSlide($event)}>
        <Element>
          {this.template.data.slots?.map((slot: TemplateDataSlot) => {
            return <ion-skeleton-text slot={slot.name} style={{width: '60%'}}></ion-skeleton-text>;
          })}
        </Element>
      </deckgo-deck>
    );
  }

  private renderSpinner() {
    return (
      <div class="spinner">
        <ion-spinner color="medium"></ion-spinner>
        <ion-label>{i18n.state.core.loading}</ion-label>
      </div>
    );
  }

  private renderEdit() {
    if (!this.editable) {
      return undefined;
    }

    return (
      <button>
        <AppIcon name="pencil" ariaLabel="" ariaHidden={true}></AppIcon>
      </button>
    );
  }

  private renderAuthor() {
    if (!this.author) {
      return undefined;
    }

    return (
      <p class={this.loaded ? 'show' : 'hidden'}>
        Created by{' '}
        <a
          href={this.template.data.author?.url}
          rel="noopener norefferer"
          target="_blank"
          onClick={($event: UIEvent) => $event.stopPropagation()}>
          {this.template.data.author?.name}
        </a>
      </p>
    );
  }
}
