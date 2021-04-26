import {DocGif} from './Readme.mdx';

export default {
  title: 'Templates/GIF',
  parameters: {
    docs: {
      page: DocGif
    }
  },
  argTypes: {
    src: {control: 'text'},
    alt: {control: 'text'},
    fullscreen: {control: 'boolean'}
  }
};

export const GIF = ({src, alt, fullscreen}) => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-gif src="${src}" alt="${alt}" fullscreen="${fullscreen}">
      <h1 slot="title">My title</h1>
      <h1 slot="top">Hey</h1>
      <h2 slot="bottom">It's a cool gif</h2>
    </deckgo-slide-gif>
  </deckgo-deck>
</div>`;
};

GIF.args = {
  src: 'https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif',
  alt: 'My gif',
  fullscreen: true
};
