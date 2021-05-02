import {DocColor} from './Readme.mdx';

export default {
  title: 'Components/Color',
  parameters: {
    docs: {
      page: DocColor
    }
  },
  argTypes: {
    colorHex: {control: 'color'}
  }
};

export const Color = ({colorHex}) => {
  return `<deckgo-color color-hex="${colorHex}"">
  </deckgo-color>`;
};

Color.args = {
  colorHex: '#FF6900'
};
