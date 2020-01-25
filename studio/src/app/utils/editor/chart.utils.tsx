import {SlideChartType} from '../../models/data/slide';

export class ChartUtils {
  static initSlideChartType(element: HTMLElement): Promise<SlideChartType> {
    return new Promise<SlideChartType>((resolve) => {
      if (!element) {
        resolve(undefined);
        return;
      }

      const typeAttr: string = element.getAttribute('type');

      if (!typeAttr || typeAttr === undefined || typeAttr === '') {
        resolve(SlideChartType.PIE);
        return;
      }

      const chartType: string = Object.keys(SlideChartType).find((key: string) => {
        return typeAttr === SlideChartType[key];
      });

      resolve(SlideChartType[chartType]);
    });
  }
}
