import {Component, h, Prop} from '@stencil/core';

@Component({
  tag: 'app-templates-content',
})
export class AppTemplatesContent {
  @Prop()
  highlight: boolean = false;

  @Prop()
  highlightIndex: number | undefined = undefined;

  @Prop()
  selected: boolean = false;

  render() {
    const classTitle = this.highlight && this.highlightIndex === undefined ? 'highlight' : undefined;
    const classContent = this.highlight && this.highlightIndex === 1 ? 'highlight' : undefined;

    return (
      <div class={`item ${this.selected ? 'selected' : ''}`}>
        <deckgo-slide-content class="showcase">
          <p slot="title">
            <ion-skeleton-text style={{width: '60%'}} class={classTitle}></ion-skeleton-text>
          </p>
          <p slot="content">
            <ion-skeleton-text style={{width: '80%'}} class={classContent}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '82%'}} class={classContent}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '64%'}} class={classContent}></ion-skeleton-text>
          </p>
        </deckgo-slide-content>
      </div>
    );
  }
}
