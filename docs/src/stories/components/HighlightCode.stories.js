import {DocHighlightCode} from './Readme.mdx';

export default {
  title: 'Components/Highlight Code',
  parameters: {
    docs: {
      page: DocHighlightCode
    }
  },
  argTypes: {
    language: {control: 'text'},
    highlightLines: {control: 'text'},
    lineNumbers: {control: 'boolean'},
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
    },
    editable: {control: 'boolean'}
  }
};

export const HighlightCode = ({language, highlightLines, lineNumbers, terminal, theme, editable, anchor, anchorZoom, hideAnchor}) => {
  return `<deckgo-highlight-code language="${language}" highlight-lines="${highlightLines}" line-numbers="${lineNumbers}"
                                 terminal="${terminal}" theme="${theme}" editable="${editable}">
    <code slot="code">export class MyComponent {
  private handleClick = () => {
    alert('Received the button click!');
  }

  render() {
    return (
      <button onClick={this.handleClick}>Click Me!</button>
    );
  }
}</code>
</deckgo-highlight-code>`;
};

HighlightCode.args = {
  language: 'javascript',
  highlightLines: '1,3',
  lineNumbers: true,
  terminal: 'carbon',
  theme: 'dracula',
  editable: false
};
