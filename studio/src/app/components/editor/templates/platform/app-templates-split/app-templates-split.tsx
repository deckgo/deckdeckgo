import {Component, h, Prop} from '@stencil/core';

@Component({
  tag: 'app-templates-split',
})
export class AppTemplatesSplit {
  @Prop()
  highlight: boolean = false;

  @Prop()
  highlightIndex: number | undefined = undefined;

  @Prop()
  vertical: boolean = false;

  render() {
    const classStart = this.highlight && this.highlightIndex === undefined ? 'highlight' : undefined;
    const classEnd = this.highlight && this.highlightIndex === 1 ? 'highlight' : undefined;

    return (
      <div class="item">
        <deckgo-slide-split vertical={this.vertical} class="showcase">
          <p slot="start">
            <ion-skeleton-text style={{width: '80%'}} class={classStart}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classStart}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}} class={classStart}></ion-skeleton-text>
          </p>
          <p slot="end">
            <ion-skeleton-text style={{width: '80%'}} class={classEnd}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classEnd}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}} class={classEnd}></ion-skeleton-text>
          </p>
        </deckgo-slide-split>
      </div>
    );
  }
}
