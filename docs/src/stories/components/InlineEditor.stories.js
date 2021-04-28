import {DocInlineEditor} from './Readme.mdx';

export default {
  title: 'Components/Inline Editor',
  parameters: {
    docs: {
      page: DocInlineEditor
    }
  },
  argTypes: {
    containers: {control: 'text'},
    mobile: {control: 'boolean'},
    stickyDesktop: {control: 'boolean'},
    stickyMobile: {control: 'boolean'},
    imgEditable: {control: 'boolean'},
    list: {control: 'boolean'},
    align: {control: 'boolean'},
    fontSize: {control: 'boolean'},
    backgroundColor: {control: 'boolean'}
  }
};

export const InlineEditor = ({containers, mobile, stickyDesktop, stickyMobile, imgEditable, list, align, fontSize, backgroundColor}) => {
  return `<div>
  <h1 contenteditable>DeckDeckGo (editable title)</h1>

  <h2 contenteditable>The Progressive Web App alternative for simple presentations 🚀 (editable subtitle)</h2>
  
  <div contenteditable>Edit anywhere, display everywhere (editable paragraph)</div>
  
  <div style="width: 200px; padding: 16px;"><img style="width: 100%;" src="https://deckdeckgo.com/assets/deckdeckgo.png"/></div>
 
</div>

<deckgo-inline-editor containers="${containers}" mobile="${mobile}" sticky-desktop="${stickyDesktop}"
                      sticky-mobile="${stickyMobile}" img-editable="${imgEditable}" align="${align}" font-size="${fontSize}" background-color="${backgroundColor}">
</deckgo-inline-editor>`;
};

InlineEditor.args = {
  containers: 'h1,h2,h3,h4,h5,h6,div',
  mobile: false,
  stickyDesktop: false,
  stickyMobile: false,
  imgEditable: true,
  list: true,
  align: true,
  fontSize: true,
  backgroundColor: true
};
