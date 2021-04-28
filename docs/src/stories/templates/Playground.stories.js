import {DocPlayground} from './Readme.mdx';

export default {
  title: 'Templates/Playground',
  parameters: {
    docs: {
      page: DocPlayground
    }
  },
  argTypes: {
    src: {control: 'text'},
    theme: {
      type: 'select',
      options: ['default', 'light', 'dark']
    }
  }
};

export const Playground = ({src, theme}) => {
  return `<div class="container" style="width: 600px; height: calc(600px *  9 / 16)">
  <deckgo-deck embedded="true">
    <deckgo-slide-playground src="${src}" theme="${theme}">
      <h1 slot="title">My Codepen</h1>
    </deckgo-slide-playground>
  </deckgo-deck>
</div>`;
};

Playground.args = {
  src: 'https://codepen.io/peterpeterparker/pen/dyGbOZm',
  theme: 'default'
};
