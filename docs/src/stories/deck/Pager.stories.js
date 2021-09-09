import {DocPager} from './Pager.mdx';

export default {
  title: 'Deck/Pager',
  parameters: {
    docs: {
      page: DocPager
    }
  }
};

export const Pager = () => {
  return `<div class="container">
  <deckgo-deck embedded="true" style="--pager-text-percentage-display: block;">
    <deckgo-slide-title>
        <h1 slot="title">Hello</h1>
    </deckgo-slide-title>
    <deckgo-slide-title>
        <h1 slot="title">World</h1>
    </deckgo-slide-title>
    <deckgo-slide-title>
        <h1 slot="title">Yolo</h1>
    </deckgo-slide-title>
    
    <deckgo-pager slot="pager">
    </deckgo-pager>
  </deckgo-deck>
</div>`;
};

Pager.args = {};
