import {DocPieChart} from './Readme.mdx';

export default {
  title: 'Components/Pie Chart',
  parameters: {
    docs: {
      page: DocPieChart
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
    innerRadius: {control: 'number'}
  }
};

export const PieChart = ({
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
  innerRadius
}) => {
  return `<deckgo-pie-chart width="${width}"
                  height="${height}"
                  src="${src}" separator="${separator}" custom-loader="${customLoader}"
                  margin-top="${marginTop}" margin-bottom="${marginBottom}" 
                  margin-left="${marginLeft}" margin-right="${marginRight}"
                  animation="${animation}" animation-duration="${animationDuration}"
                  inner-radius="${innerRadius}">
</deckgo-pie-chart>`;
};

PieChart.args = {
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
  innerRadius: 0
};
