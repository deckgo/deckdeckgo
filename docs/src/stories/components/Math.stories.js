import {DocMath} from './Readme.mdx';

export default {
  title: 'Components/Math',
  parameters: {
    docs: {
      page: DocMath
    }
  },
  argTypes: {
    editable: {control: 'boolean'}
  }
};

export const Math = ({editable}) => {
  return `<deckgo-math editable="${editable}">
    <code slot="math">% \\f is defined as f(#1) using the macro
      \\f{x} = \\int_{-\\infty}^\\infty
      \\hat \\f\\xi\\,e^{2 \\pi i \\xi x}
      \\,d\\xi</code>
</deckgo-math>`;
};

Math.args = {
  editable: true
};
