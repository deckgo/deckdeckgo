// A bar
export interface DeckdeckgoBarChartDataValue {
  // The key of a bar, use for the style
  key: number | string;
  // A title for the bar
  label: string;
  // Its value
  value: number;
}

// A chart
export interface DeckdeckgoBarChartData {
  // The chart label
  label: string | Date | number;
  // 1 or multiple bars
  values: DeckdeckgoBarChartDataValue[];
}
