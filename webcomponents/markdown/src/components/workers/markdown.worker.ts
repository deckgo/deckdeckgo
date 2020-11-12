import marked from 'marked';

import {changeCodeCreation, ParseMarkdownCodeOptions} from './utils/markdown-code.utils';
import {changeImgCreation} from './utils/markdown-img.utils';

export interface ParseMarkdownOptions {
  mdText: string;
  code: ParseMarkdownCodeOptions;
}

export const parseMarkdown = async (options: ParseMarkdownOptions) => {
  const renderer = new marked.Renderer();
  changeCodeCreation(renderer, options.code);
  changeImgCreation(renderer);

  return marked(options.mdText, {
    renderer,
    xhtml: true,
  });
};
