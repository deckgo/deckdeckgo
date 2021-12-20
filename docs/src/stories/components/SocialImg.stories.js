import {DocSocialImage} from './Readme.mdx';

export default {
  title: 'Components/Social Image',
  parameters: {
    docs: {
      page: DocSocialImage
    }
  },
  argTypes: {
    width: {control: 'text'},
    height: {control: 'text'},
    padding: {control: 'number'},
    innerPadding: {control: 'number'},
    text: {control: 'text'},
    imgSrc: {control: 'text'},
    imgMimeType: {control: 'text'},
    rectStrokeWidth: {control: 'number'},
    rectColor: {control: 'text'},
    rectRx: {control: 'number'},
    rectRy: {control: 'number'},
    rectBackground: {control: 'text'},
    background: {control: 'text'}
  }
};

export const SocialImage = ({
  width,
  height,
  padding,
  innerPadding,
  text,
  imgSrc,
  imgMimeType,
  rectStrokeWidth,
  rectColor,
  rectRx,
  rectRy,
  rectBackground,
  background
}) => {
  return `<deckgo-social-img width="${width}" height="${height}" padding="${padding}"
                         inner-padding="${innerPadding}" text="${text}" img-src="${imgSrc}" 
                         img-mime-type="${imgMimeType}" rect-stroke-width="${rectStrokeWidth}" rect-color="${rectColor}" 
                         rect-rx="${rectRx}" rect-ry="${rectRy}" rect-background="${rectBackground}" background="${background}"></deckgo-social-img>`;
};

SocialImage.args = {
  width: '600px',
  height: '314px',
  padding: 32,
  innerPadding: 16,
  text: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit',
  imgSrc: 'https://deckdeckgo-studio-staging.web.app/assets/icons/deckdeckgo.svg',
  imgMimeType: 'image/svg+xml',
  rectStrokeWidth: 5,
  rectColor: '#3dc2ff',
  rectRx: 0,
  rectRy: 0,
  rectBackground: '#ffffff',
  background: '#ffffff'
};
