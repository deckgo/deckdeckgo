import {DocSplit} from './Readme.mdx';

export default {
  title: 'Templates/Split',
  parameters: {
    docs: {
      page: DocSplit
    }
  },
  argTypes: {
    vertical: {control: 'boolean'}
  }
};

export const Split = ({vertical}) => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-split vertical="${vertical}">
        <p slot="start">
          The content you want to display on the start side
        </p>
        <p slot="end">
          The content you want to display on the end side
        </p>
      </deckgo-slide-split>
  </deckgo-deck>
</div>`;
};

Split.args = {
  vertical: false
};
