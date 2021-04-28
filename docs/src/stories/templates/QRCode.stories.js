import {DocQRCode} from './Readme.mdx';

export default {
  title: 'Templates/QR Code',
  parameters: {
    docs: {
      page: DocQRCode
    }
  },
  argTypes: {
    content: {control: 'text'},
    imgSrc: {control: 'text'}
  }
};

export const QRCode = ({content, imgSrc}) => {
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-qrcode content="${content}" img-src="${imgSrc}">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>`;
};

QRCode.args = {
  content: 'https://deckdeckgo.com',
  imgSrc: 'https://deckdeckgo.com/assets/deckdeckgo.png'
};
