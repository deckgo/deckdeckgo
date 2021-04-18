import {create} from '@storybook/theming';

export default create({
  base: 'light',

  colorPrimary: '#3880ff',
  colorSecondary: '#3dc2ff',

  // UI
  appBg: '#f4f5f8',
  appContentBg: 'white',
  appBorderColor: '#f4f5f8',
  appBorderRadius: 8,

  // Typography
  fontBase: '"Open Sans", sans-serif',
  fontCode: '"JetBrains Mono", monospace',

  // Toolbar default and active colors
  barTextColor: '#222428',
  barSelectedColor: '#5260ff',
  barBg: 'white',

  // Form colors
  inputBg: '#f4f5f8',
  inputBorder: '#dedede',
  inputTextColor: 'black',
  inputBorderRadius: 4,

  brandTitle: 'DeckDeckGo',
  brandUrl: 'https://deckdeckgo.com'
});
