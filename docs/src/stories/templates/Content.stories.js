import {DocContent} from './Readme.mdx';

export default {
  title: 'Templates/Content',
  parameters: {
    docs: {
      page: DocContent
    }
  }
};

export const Content = () => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-content>
        <h1 slot="title">Something related to my topic</h1>
        <p slot="content">
          Slide 1
        </p>
      </deckgo-slide-content>
      
    <deckgo-slide-content>
        <h1 slot="title">Slide 2</h1>
      </deckgo-slide-content>
  </deckgo-deck>
</div>`;
};

Content.args = {
};
