import marked from 'marked';

import {changeCodeCreation} from './utils/markdown.utils';

export const parseMarkdown = async (mdText: string) => {
  const renderer = new marked.Renderer();
  changeCodeCreation(renderer);

  return marked(mdText, {
    renderer,
    xhtml: true,
  });
};
