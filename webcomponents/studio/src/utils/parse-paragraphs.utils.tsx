import {convertStyle, Paragraph} from '@deckdeckgo/editor';
import {h, JSX} from '@stencil/core';
import {nanoid} from 'nanoid';
import {parseElements} from './parse-elements.utils';
import {contentNotEditableParagraph} from './parse-doc-elements.utils';

export const parseParagraph = ({
  paragraph,
  ignoreParagraphId = false
}: {
  paragraph: Paragraph;
  ignoreParagraphId?: boolean;
}): JSX.IntrinsicElements => {
  let content = undefined;

  // Create a div to parse back to JSX its children
  const div = document.createElement('div');

  if (paragraph.data.children?.length > 0) {
    div.innerHTML = paragraph.data.children.join('');
    content = parseElements({element: div, root: true, type: 'doc'});
  }

  const ParagraphElement: string = paragraph.data.nodeName;

  const attributes: Record<string, string | number | boolean | undefined | Record<string, string>> = {
    ...(paragraph.data.attributes || {}),
    ...(paragraph.data.attributes?.style && {style: convertStyle(paragraph.data.attributes.style as string)}),
    ...contentNotEditableParagraph(paragraph.data.nodeName)
  };

  const result: JSX.IntrinsicElements = (
    <ParagraphElement key={nanoid()} paragraph_id={ignoreParagraphId ? undefined : paragraph.id} {...attributes}>
      {content}
    </ParagraphElement>
  );

  return result;
};
