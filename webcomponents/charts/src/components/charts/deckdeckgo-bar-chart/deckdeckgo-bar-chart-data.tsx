// A bar
export interface DeckdeckgoBarChartDataValue {
  // The key of a bar, use for the style
  key: number | string;
  // A title for the bar
  title: string;
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

// Example
// [{
//   "label": "01.01.2018",
//   "values": [{"key": "1", "title": "Salami", "value": 5, "randomFillColor": "49c001"}, {
//     "key": "2",
//     "title": "Pastrami",
//     "value": 0,
//     "randomFillColor": "f398d0"
//   }, {"key": "3", "title": "Prosciutto", "value": 10, "randomFillColor": "4a5dfd"}]
// }, {
//   "label": "01.03.2018",
//   "values": [{"key": "1", "title": "Salami", "value": 10, "randomFillColor": "49c001"}, {
//     "key": "2",
//     "title": "Pastrami",
//     "value": 6,
//     "randomFillColor": "f398d0"
//   }, {"key": "3", "title": "Prosciutto", "value": 12, "randomFillColor": "4a5dfd"}]
// }]
