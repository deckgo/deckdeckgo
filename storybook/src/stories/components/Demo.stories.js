import {DocDemo} from './Readme.mdx';

export default {
  title: 'Components/Demo',
  parameters: {
    docs: {
      page: DocDemo
    }
  },
  argTypes: {
    src: {control: 'text'},
    frameTitle: {control: 'text'},
    mode: {
      type: 'select',
      options: ['md', 'ios']
    },
    instant: {control: 'boolean'}
  }
};

export const Demo = ({src, frameTitle, mode, instant}) => {
  return `<deckgo-demo style="width: 150px; height: 350px;" src="${src}" frame-title="${frameTitle}" mode="${mode}" instant="${instant}"></deckgo-demo>`;
};

Demo.args = {
  src: 'https://deckdeckgo.com',
  frameTitle: 'DeckDeckGo',
  mode: 'md',
  instant: true
};
