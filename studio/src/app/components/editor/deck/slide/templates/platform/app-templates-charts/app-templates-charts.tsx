import {Component, Element, Event, EventEmitter, h} from '@stencil/core';

import assetsStore from '../../../../../../../stores/assets.store';
import i18n from '../../../../../../../stores/i18n.store';

import {SlideAttributes, SlideChartType, SlideTemplate} from '@deckdeckgo/editor';

@Component({
  tag: 'app-templates-charts'
})
export class AppTemplatesCharts {
  @Element() el: HTMLElement;

  @Event()
  selectedTemplate: EventEmitter<{template: SlideTemplate; attributes: SlideAttributes}>;

  private timerInterval: NodeJS.Timeout;
  private timerCounter: number = 0;

  async componentDidLoad() {
    await this.lazyLoadAllCharts();
    await this.animate();
  }

  disconnectedCallback() {
    this.unsubscribeTimer();
  }

  private unsubscribeTimer() {
    if (this.timerInterval) {
      clearInterval(this.timerInterval);
    }
  }

  private async lazyLoadAllCharts() {
    const slidesCharts: HTMLDeckgoSlideChartElement[] = Array.from(this.el.querySelectorAll('deckgo-slide-chart.showcase'));

    if (!slidesCharts || slidesCharts.length <= 0) {
      return;
    }

    const promises = [];
    Array.from(slidesCharts).forEach((slide: HTMLDeckgoSlideChartElement) => {
      promises.push(slide.lazyLoadContent());
    });

    await Promise.all(promises);
  }

  private async animate() {
    this.timerInterval = setInterval(async () => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-slide-chart[animation]');

      if (elements) {
        for (const element of Array.from(elements)) {
          await (element as any).beforeSwipe(this.timerCounter % 2 === 0, true);
        }
      }

      this.timerCounter++;
    }, 2000);
  }

  render() {
    return [
      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.PIE}})}
      >
        {/* Pie */}
        <deckgo-slide-chart
          class="showcase"
          type="pie"
          marginTop={8}
          marginBottom={8}
          marginLeft={8}
          marginRight={8}
          width={68}
          height={68}
          src={assetsStore.state.chart.pieSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.pie}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() =>
          this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.PIE, innerRadius: 100}})
        }
      >
        {/* Donut */}
        <deckgo-slide-chart
          class="showcase"
          type="pie"
          marginTop={8}
          marginBottom={8}
          marginLeft={8}
          marginRight={8}
          width={68}
          height={68}
          inner-radius={16}
          src={assetsStore.state.chart.pieSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.donut}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.PIE, animation: true}})}
      >
        {/* Animated Pie */}
        <deckgo-slide-chart
          class="showcase"
          type="pie"
          animation={true}
          marginTop={8}
          marginBottom={8}
          marginLeft={8}
          marginRight={8}
          width={68}
          height={68}
          src={assetsStore.state.chart.barCompareSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.pie_comparison}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.LINE}})}
      >
        {/* Area */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          src={assetsStore.state.chart.lineCompareSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.area}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.LINE, smooth: false}})}
      >
        {/* Sharp area */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          smooth={'false'}
          src={assetsStore.state.chart.lineSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.sharp_area}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.LINE, area: false}})}
      >
        {/* Lines */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          area={'false'}
          src={assetsStore.state.chart.lineNoDatesSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.lines}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() =>
          this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.LINE, animation: true}})
        }
      >
        {/* Animated area */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={53}
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          animation={true}
          src={assetsStore.state.chart.lineMultipleSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.line_graph_comparison}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.BAR}})}
      >
        {/* Bar */}
        <deckgo-slide-chart
          class="showcase"
          type="bar"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          src={assetsStore.state.chart.pieSrc}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.bar}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.BAR}})}
      >
        {/* Grouped bars */}
        <deckgo-slide-chart
          class="showcase"
          type="bar"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          src={assetsStore.state.chart.barCompareSrc}
          style={{
            '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
            '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
            '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'
          }}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.grouped_bars}</p>
        </deckgo-slide-chart>
      </div>,

      <div
        class="item"
        custom-tappable
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CHART, attributes: {type: SlideChartType.BAR, animation: true}})}
      >
        {/* Animation bars */}
        <deckgo-slide-chart
          class="showcase"
          type="bar"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          animation={true}
          src={assetsStore.state.chart.barCompareSrc}
          style={{
            '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
            '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
            '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'
          }}
          custom-loader={true}
        >
          <p slot="title">{i18n.state.templates.bar_comparison}</p>
        </deckgo-slide-chart>
      </div>
    ];
  }
}
