import {DocCode} from './Readme.mdx';

export default {
  title: 'Templates/Code',
  parameters: {
    docs: {
      page: DocCode
    }
  },
  argTypes: {
    src: {control: 'text'},
    language: {control: 'text'},
    terminal: {
      type: 'select',
      options: ['carbon', 'ubuntu', 'none']
    },
    theme: {
      type: 'select',
      options: [
        '3024-night',
        'a11y-dark',
        'blackboard',
        'base16-dark',
        'base16-light',
        'cobalt',
        'dracula',
        'duotone',
        'hopscotch',
        'lucario',
        'material',
        'monokai',
        'night-owl',
        'nord',
        'oceanic-next',
        'one-light',
        'one-dark',
        'panda',
        'paraiso',
        'seti',
        'shades-of-purple',
        'solarized-dark',
        'solarized-light',
        'synthwave',
        'twilight',
        'verminal',
        'vscode',
        'yeti',
        'zenburn'
      ]
    }
  }
};

export const Code = ({src, language, terminal, theme}) => {
  return `<div class="container" style="width: 600px; height: 600px;">
  <deckgo-deck embedded="true">
    <deckgo-slide-code src="${src}" language="${language}" terminal="${terminal}" theme="${theme}">
          <h1 slot="title">slot="title"</h1>
        </deckgo-slide-code>
  </deckgo-deck>
</div>`;
};

Code.args = {
  src: 'https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/code/src/components/slide/deckdeckgo-slide-code.tsx',
  language: 'javascript',
  terminal: 'carbon',
  theme: 'dracula'
};
