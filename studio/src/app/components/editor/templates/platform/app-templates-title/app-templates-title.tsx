import {Component, h, Prop} from '@stencil/core';

@Component({
  tag: 'app-templates-title',
})
export class AppTemplatesTitle {
  @Prop()
  highlight: boolean = false;

  @Prop()
  highlightIndex: number | undefined = undefined;

  render() {
    const classTitle = this.highlight && this.highlightIndex === undefined ? 'highlight' : undefined;
    const classContent = this.highlight && this.highlightIndex === 1 ? 'highlight' : undefined;

    return (
      <div class="item">
        <deckgo-slide-title class="showcase">
          <p slot="title">
            <ion-skeleton-text style={{width: '60%'}} class={classTitle}></ion-skeleton-text>
          </p>
          <p slot="content">
            <ion-skeleton-text style={{width: '80%'}} class={classContent}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classContent}></ion-skeleton-text>
          </p>
        </deckgo-slide-title>
      </div>
    );
  }
}
