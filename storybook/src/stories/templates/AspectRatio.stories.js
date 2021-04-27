import {DocAspectRatio} from './Readme.mdx';

export default {
  title: 'Templates/Aspect Ratio',
  parameters: {
    docs: {
      page: DocAspectRatio
    }
  },
  argTypes: {
    ratio: {control: 'number'},
    grid: {control: 'boolean'}
  }
};

export const AspectRatio = ({grid, ratio}) => {
  return `<div class="container" style="width: 600px; height: 600px">
  <deckgo-deck embedded="true">
    <deckgo-slide-aspect-ratio grid="${grid}" ratio="${ratio}">
        <h1 style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); margin: 0">Any elements</h1>
      </deckgo-slide-aspect-ratio>
  </deckgo-deck>
</div>`;
};

AspectRatio.args = {
  ratio: 16 / 9,
  grid: true
};
