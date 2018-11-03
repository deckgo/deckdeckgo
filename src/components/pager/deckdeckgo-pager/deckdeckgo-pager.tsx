import {Component, Prop, State, Watch} from '@stencil/core';


@Component({
  tag: 'deckgo-pager',
  styleUrl: 'deckdeckgo-pager.scss',
  shadow: true
})
export class DeckdeckgoPager {

  @Prop() activeIndex: number;
  @Prop() length: number;

  @State()
  private steps: number[] = [];
  private progression: number = 0;

  @Watch('length')
  calculateProgression() {
    this.steps = [...Array(this.length).keys()];
    this.progression = Math.round(((this.activeIndex + 1) / this.length) * 100);
  }

  render() {
    return <div class="deckgo-pager-container">
      {this.renderSteps()}
    </div>
  }

  private renderSteps() {
    const style = {
      width: this.progression + '%'
    };

    return (
      this.steps.map((i: number) => {
        return <div
          class={i === this.activeIndex ? 'deckgo-pager-current deckgo-pager-step' : 'deckgo-pager-step'}
          style={style}></div>
      })
    );
  }

}
