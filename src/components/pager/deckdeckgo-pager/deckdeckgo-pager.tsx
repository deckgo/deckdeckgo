import {Component, Prop, State, Watch} from '@stencil/core';


@Component({
  tag: 'deckgo-pager',
  styleUrl: 'deckdeckgo-pager.scss',
  shadow: true
})
export class DeckdeckgoPager {

  @Prop() activeIndex: number;
  @Prop() length: number;

  @Prop() percentage: boolean = false;

  @State()
  private progression: number = 0;

  @Watch('length')
  @Watch('activeIndex')
  calculateProgression() {
    this.progression = Math.round(((this.activeIndex + 1) / this.length) * 100);
  }

  // Nice circular percentage chart from the article of Sergio Pedercini
  // https://medium.com/@pppped/how-to-code-a-responsive-circular-percentage-chart-with-svg-and-css-3632f8cd7705
  render() {
    const ratio: string = '' + this.progression + ', 100';

    return <svg viewBox="0 0 36 36" class="deckgo-pager-circular-chart">
      <path class="deckgo-pager-circle-bg"
            d="M18 2.0845
            a 15.9155 15.9155 0 0 1 0 31.831
            a 15.9155 15.9155 0 0 1 0 -31.831"
      />
      <path class="deckgo-pager-circle"
            stroke-dasharray={ratio}
            d="M18 2.0845
        a 15.9155 15.9155 0 0 1 0 31.831
        a 15.9155 15.9155 0 0 1 0 -31.831"
      />
      {this.renderText()}
    </svg>
  }

  private renderText() {
    return this.percentage ? <text x="18" y="20.35" className="deckgo-pager-percentage">{this.progression}%</text> : undefined;
  }

}
