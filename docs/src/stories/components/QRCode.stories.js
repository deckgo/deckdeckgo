import {DocQRCode} from './Readme.mdx';

export default {
  title: 'Components/QR Code',
  parameters: {
    docs: {
      page: DocQRCode
    }
  },
  argTypes: {
    content: {control: 'text'},
    type: {
      type: 'select',
      options: ['svg', 'img']
    },
    qrCellSize: {control: 'number'},
    qrMargin: {control: 'number'},
    qrBackgroundColor: {control: 'text'},
    qrFillColor: {control: 'text'},
    qrAlt: {control: 'text'}
  }
};

export const QRCode = ({content, type, qrCellSize, qrMargin, qrBackgroundColor, qrFillColor}) => {
  return `<deckgo-qrcode content="${content}" type="${type}" qr-cell-size="${qrCellSize}" 
                         qr-margin="${qrMargin}" qr-background-color="${qrBackgroundColor}" qr-fill-color="${qrFillColor}"
                      style="--deckgo-qrcode-size: 300px; --deckgo-qrcode-color-fill: #3880ff;">
  </deckgo-qrcode>`;
};

QRCode.args = {
  content: 'https://deckdeckgo.com',
  type: 'svg',
  qrCellSize: 16,
  qrMargin: 16,
  qrBackgroundColor: '',
  qrFillColor: '',
  qrAlt: ''
};
