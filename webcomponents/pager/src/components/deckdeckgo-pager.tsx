import {Component, EventEmitter, Event, Listen, State, Watch, h, Host} from '@stencil/core';

@Component({
  tag: 'deckgo-pager',
  styleUrl: 'deckdeckgo-pager.scss',
  shadow: true,
})
export class DeckdeckgoPager {
  @State() private activeIndex: number = 0;
  @State() private length: number;

  @State()
  private percentageProgression: number = 0;

  /**
   * Emitted when the user click on the pager
   */
  @Event()
  pagerClick: EventEmitter<void>;

  @Watch('length')
  @Watch('activeIndex')
  calculateProgression() {
    this.percentageProgression = this.length > 0 ? Math.round(((this.activeIndex + 1) / this.length) * 100) : 0;
  }

  @Listen('slidesDidLoad', {target: 'document'})
  async onSlidesDidLoad($event: CustomEvent) {
    if ($event) {
      this.length = $event.detail.slides.length;
    }
  }

  @Listen('slideNextDidChange', {target: 'document'})
  @Listen('slidePrevDidChange', {target: 'document'})
  @Listen('slideToChange', {target: 'document'})
  async onSlideNavigate($event: CustomEvent) {
    if ($event) {
      this.activeIndex = $event.detail;
    }
  }

  // Nice circular percentage chart from the article of Sergio Pedercini
  // https://medium.com/@pppped/how-to-code-a-responsive-circular-percentage-chart-with-svg-and-css-3632f8cd7705
  render() {
    const ratio: string = '' + this.percentageProgression + ', 100';

    return (
      <Host onClick={() => this.pagerClick.emit()}>
        <svg viewBox="0 0 36 36" class="deckgo-pager-circular-chart">
          <path
            class="deckgo-pager-circle-bg"
            d="M18 2.0845
            a 15.9155 15.9155 0 0 1 0 31.831
            a 15.9155 15.9155 0 0 1 0 -31.831"
          />
          <path
            class="deckgo-pager-circle"
            stroke-dasharray={ratio}
            d="M18 2.0845
        a 15.9155 15.9155 0 0 1 0 31.831
        a 15.9155 15.9155 0 0 1 0 -31.831"
          />
          {this.renderText()}
        </svg>
      </Host>
    );
  }

  private renderText() {
    return [
      <text x="18" y="20.35" class="deckgo-pager-progression deckgo-pager-percentage">
        {this.percentageProgression}%
      </text>,
      <text x="18" y="20.35" class="deckgo-pager-progression deckgo-pager-slides">
        {this.length > 0 ? this.activeIndex + 1 : 0}/{this.length}
      </text>,
    ];
  }
}
