import {DocYoutube} from './Readme.mdx';

export default {
  title: 'Templates/Youtube',
  parameters: {
    docs: {
      page: DocYoutube
    }
  },
  argTypes: {
    src: {control: 'text'},
    fullscreen: {control: 'boolean'}
  }
};

export const Youtube = ({src, fullscreen}) => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-youtube src="${src}" fullscreen="${fullscreen}">
      <h1 slot="title">A 16/9 video</h1>
    </deckgo-slide-youtube>
  </deckgo-deck>
</div>`;
};

Youtube.args = {
  src: 'https://www.youtube.com/watch?v=oUOjJIfPIjw',
  fullscreen: true
};
