import marked from 'marked';

// Original source: https://github.com/markedjs/marked/issues/339
// our version:
// ![](https://www.nmattia.com/images/autoupdate-notifications.jpg "width:100px,height:20px")
export function changeImgCreation(renderer: marked.Renderer) {
  renderer.image = (src: string | null, title: string | null, alt: string) => {
    const style: string = checkForWidthAndHeight(title);
    return `<deckgo-lazy-img img-src="${sanitize(src)}" img-alt="${sanitize(alt)}" style="${style}"></deckgo-lazy-img>`;
  };
}

function sanitize(str: string) {
  return str.replace(/&<"/g, function (m) {
    if (m === '&') return '&amp;';
    if (m === '<') return '&lt;';
    return '&quot;';
  });
}

function checkForWidthAndHeight(title): string {
  const isWidthSet: RegExpExecArray | null = /width:\d+(?:px|em|ex|ch|rem|vw|vh|vmin|vmax|%)/.exec(title);
  const isHeightSet: RegExpExecArray | null = /height:\d+(?:px|em|ex|ch|rem|vw|vh|vmin|vmax|%)/.exec(title);

  let style: string = '';

  if (isWidthSet) {
    const width = /\d+(?:px|em|ex|ch|rem|vw|vh|vmin|vmax|%)/.exec(isWidthSet[0])[0];
    style += `--deckgo-lazy-img-width: ${width};`;
  }

  if (isHeightSet) {
    const height = /\d+(?:px|em|ex|ch|rem|vw|vh|vmin|vmax|%)/.exec(isHeightSet[0])[0];
    style += `--deckgo-lazy-img-height: ${height};`;
  }

  return style;
}
