import {DeckdeckgoBarChartDataValue} from '@deckdeckgo/types';

export async function drawChart(el: HTMLElement, chartWidth: number, chartHeight: number) {
  const chartElement: HTMLElement = el.shadowRoot.querySelector('deckgo-bar-chart');

  if (chartElement && typeof (chartElement as any).draw === 'function') {
    await (chartElement as any).draw(chartWidth, chartHeight);
  }
}

export function initChartSize(el: HTMLElement): Promise<{width: number; height: number} | undefined> {
  return new Promise<{width: number; height: number} | undefined>(async (resolve) => {
    const container: HTMLElement = el.shadowRoot.querySelector('div.deckgo-slide-poll-chart');

    if (container) {
      const width: number = container.clientWidth * 0.75;
      const height: number = (width * 9) / 16;

      resolve({
        width: width,
        height: height,
      });

      return;
    }

    resolve(undefined);
  });
}

export function initChartDataBar(el: HTMLElement, answerSlotName: string, index: number): Promise<DeckdeckgoBarChartDataValue> {
  return new Promise<DeckdeckgoBarChartDataValue>((resolve) => {
    const element: HTMLElement = el.querySelector(`:scope > [slot=\'${answerSlotName}\']`);

    if (!element) {
      resolve(undefined);
      return;
    }

    resolve({
      key: `${index + 1}`,
      label: element.innerHTML,
      value: Math.floor(Math.random() * 10 + 1),
    });
  });
}

export async function updateCurrentBar(el: HTMLElement, values: DeckdeckgoBarChartDataValue[]) {
  const element: HTMLElement = el.shadowRoot.querySelector('deckgo-bar-chart');

  if (element) {
    await (element as any).updateCurrentBar(values);
  }
}
