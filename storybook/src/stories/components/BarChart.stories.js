import {Docs} from './BarChart.mdx';

export default {
  title: 'Components/Bar Chart',
  parameters: {
    docs: {
      page: Docs
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
    data: {control: 'text'},
    yAxis: {control: 'boolean'},
    yAxisMin: {control: 'number'}
  }
};

export const BarChart = ({
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
  yAxis,
  yAxisMin
}) => {
  return `<deckgo-bar-chart width="${width}"
                  height="${height}"
                  src="${src}" separator="${separator}" custom-loader="${customLoader}"
                  margin-top="${marginTop}" margin-bottom="${marginBottom}" 
                  margin-left="${marginLeft}" margin-right="${marginRight}"
                  animation="${animation}" animation-duration="${animationDuration}"
                  y-axis="${yAxis}" y-axis-min="${yAxisMin}">
</deckgo-bar-chart>`;
};

BarChart.args = {
  src: 'https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv',
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
  yAxis: true,
  yAxisMin: 0
};
