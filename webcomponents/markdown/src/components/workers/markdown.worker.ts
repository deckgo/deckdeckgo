import marked from 'marked';

import {changeCodeCreation, ParseMarkdownCodeOptions} from './utils/markdown.utils';

export interface ParseMarkdownOptions {
  mdText: string;
  code: ParseMarkdownCodeOptions;
}

export const parseMarkdown = async (options: ParseMarkdownOptions) => {
  const renderer = new marked.Renderer();
  changeCodeCreation(renderer, options.code);

  return marked(options.mdText, {
    renderer,
    xhtml: true,
  });
};
