import {DocSocial} from './Readme.mdx';

export default {
  title: 'Components/Social',
  parameters: {
    docs: {
      page: DocSocial
    }
  },
  argTypes: {
    twitter: {control: 'text'},
    linkedin: {control: 'text'},
    medium: {control: 'text'},
    dev: {control: 'text'},
    github: {control: 'text'},
    fullUrl: {control: 'text'}
  }
};

export const Social = ({twitter, linkedin, medium, dev, github, fullUrl}) => {
  return `<deckgo-social twitter="${twitter}" linkedin="${linkedin}" medium="${medium}"
                         dev="${dev}" github="${github}" full-url="${fullUrl}"></deckgo-social>`;
};

Social.args = {
  twitter: 'deckdeckgo',
  linkedin: '',
  medium: '',
  dev: '',
  github: '',
  fullUrl: ''
};
