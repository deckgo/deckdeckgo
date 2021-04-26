import {DocTitle} from './Readme.mdx';

export default {
  title: 'Templates/Title',
  parameters: {
    docs: {
      page: DocTitle
    }
  }
};

export const Title = () => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-title>
      <h1 slot="title">My presentation title</h1>
      <p slot="content">
        First slide 🚀
      </p>
    </deckgo-slide-title>
    
    <deckgo-slide-title>
      <h1 slot="title">Second slide</h1>
    </deckgo-slide-title>
  </deckgo-deck>
</div>`;
};

Title.args = {
};
