import {DocLineChart} from './Readme.mdx';

export default {
  title: 'Components/Line Chart',
  parameters: {
    docs: {
      page: DocLineChart
    }
  },
  argTypes: {
    src: {control: 'text'},
    width: {control: 'number'},
    height: {control: 'number'},
    separator: {control: 'text'},
    customLoader: {control: 'boolean'},
    marginTop: {control: 'number'},
    marginBottom: {control: 'number'},
    marginLeft: {control: 'number'},
    marginRight: {control: 'number'},
    animation: {control: 'boolean'},
    animationDuration: {control: 'number'},
    yAxisDomain: {
      type: 'select',
      options: ['max', 'extent']
    },
    datePattern: {control: 'text'},
    smooth: {control: 'boolean'},
    area: {control: 'boolean'},
    ticks: {control: 'number'},
    grid: {control: 'boolean'}
  }
};

export const LineChart = ({
  src,
  width,
  height,
  separator,
  customLoader,
  marginTop,
  marginBottom,
  marginLeft,
  marginRight,
  animation,
  animationDuration,
  yAxisDomain,
  datePattern,
  smooth,
  area,
  ticks,
  grid
}) => {
  return `<deckgo-line-chart width="${width}"
                  height="${height}"
                  src="${src}" separator="${separator}" custom-loader="${customLoader}"
                  margin-top="${marginTop}" margin-bottom="${marginBottom}" 
                  margin-left="${marginLeft}" margin-right="${marginRight}"
                  animation="${animation}" animation-duration="${animationDuration}"
                  y-axis-domain="${yAxisDomain}"
                  date-pattern="${datePattern}"
                  smooth="${smooth}" area="${area}" ticks="${ticks}" grid="${grid}">
</deckgo-line-chart>`;
};

LineChart.args = {
  src: 'https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart.csv',
  width: 300,
  height: 300,
  separator: ';',
  customLoader: false,
  marginTop: 32,
  marginBottom: 64,
  marginLeft: 32,
  marginRight: 32,
  animation: false,
  animationDuration: 1000,
  yAxisDomain: 'max',
  datePattern: 'dd.MM.yyyy',
  smooth: true,
  area: true,
  ticks: 5,
  grid: false
};
