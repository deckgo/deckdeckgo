import marked from 'marked';

// Source: https://github.com/markedjs/marked/issues/339
// ![](https://www.nmattia.com/images/autoupdate-notifications.jpg "width:100px,height:20px")
export function changeImgCreation(renderer: marked.Renderer) {
  renderer.image = (src: string | null, title: string | null, alt: string) => {
    const getWidthAndHeightStyle = checkForWidthAndHeight(title);
    if (getWidthAndHeightStyle.length) {
      return `<deckgo-lazy-img img-src="${sanitize(src)}" img-alt="${sanitize(alt)}" style="${getWidthAndHeightStyle}"></deckgo-lazy-img>`;
    }
    return `<deckgo-lazy-img img-src="${sanitize(src)}" img-alt="${sanitize(alt)}"></deckgo-lazy-img>`;
  };
}

function sanitize(str: string) {
  return str.replace(/&<"/g, function (m) {
    if (m === '&') return '&amp;';
    if (m === '<') return '&lt;';
    return '&quot;';
  });
}

function checkForWidthAndHeight(title) {
  const isWidthSet = /width:\d+px/.exec(title);
  const isHeightSet = /height:\d+px/.exec(title);
  let style: string = '';
  if(!!(isWidthSet && isHeightSet)){
    const width = /\d+px/.exec(isWidthSet[0])[0];
    const height = /\d+px/.exec(isHeightSet[0])[0];
    style += `--deckgo-lazy-img-width: ${width};`;
    style += `--deckgo-lazy-img-height: ${height};`;
  }
  return style;
};
