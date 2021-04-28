import {DocWordCloud} from './Readme.mdx';

export default {
  title: 'Components/Word Cloud',
  parameters: {
    docs: {
      page: DocWordCloud
    }
  },
  argTypes: {
    editable: {control: 'boolean'},
    marginTop: {control: 'number'},
    marginBottom: {control: 'number'},
    marginLeft: {control: 'number'},
    marginRight: {control: 'number'}
  }
};

export const WordCloud = ({editable, marginTop, marginBottom, marginLeft, marginRight}) => {
  return `<div style="position: relative; width: 350px; height: 350px;">
  <deckgo-word-cloud editable="${editable}" margin-top="${marginTop}" margin-bottom="${marginBottom}" 
                  margin-left="${marginLeft}" margin-right="${marginRight}">
        <code slot="words"
          >How the Word Cloud Generator Works The layout algorithm for positioning words without overlap is available on GitHub under an open source license as
          d3-cloud. Note that this is the only the layout algorithm and any code for converting text into words and rendering the final output requires
          additional development.
        </code>
    </deckgo-word-cloud>
</div>`;
};

WordCloud.args = {
  editable: true,
  marginTop: 32,
  marginBottom: 32,
  marginLeft: 32,
  marginRight: 32

};
