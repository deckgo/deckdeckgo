import {DocAuthor} from './Readme.mdx';

export default {
  title: 'Templates/Author',
  parameters: {
    docs: {
      page: DocAuthor
    }
  },
  argTypes: {
    imgSrc: {control: 'text'}
  }
};

export const Author = ({imgSrc}) => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-author img-src="${imgSrc}">
        <h1 slot="title">slot="title"</h1>
        <div slot="author">slot="author"</div>
        <div slot="social-link"><deckgo-social twitter="daviddalbusco"><ion-icon aria-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon></deckgo-social></div>
        <div slot="social-link"><deckgo-social linkedin="david-dal-busco"><ion-icon aria-label="David on LinkedIn" slot="icon" name="logo-linkedin"></ion-icon></deckgo-social></div>
    </deckgo-slide-author>
  </deckgo-deck>
</div>`;
};

Author.args = {
  imgSrc: 'https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg'
};
