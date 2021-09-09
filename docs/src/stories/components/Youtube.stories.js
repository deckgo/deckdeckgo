import {DocYoutube} from './Readme.mdx';

export default {
  title: 'Components/Youtube',
  parameters: {
    docs: {
      page: DocYoutube
    }
  },
  argTypes: {
    src: {control: 'text'},
    width: {control: 'number'},
    height: {control: 'number'},
    allowFullscreen: {control: 'boolean'}
  }
};

export const Youtube = ({src, width, height, allowFullscreen}) => {
  return `<deckgo-youtube src="${src}" width="${width}" height="${height}" allow-fullscreen="${allowFullscreen}" instant="true"></deckgo-youtube>`;
};

Youtube.args = {
  src: 'https://www.youtube.com/watch?v=oUOjJIfPIjw',
  width: 300,
  height: 300,
  allowFullscreen: false
};
