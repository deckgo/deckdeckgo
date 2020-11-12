import marked from 'marked';

// Source: https://github.com/markedjs/marked/issues/339
// ![](https://www.nmattia.com/images/autoupdate-notifications.jpg "=100px,20px")
// ![](https://www.nmattia.com/images/autoupdate-notifications.jpg "=100px")
// ![](https://www.nmattia.com/images/autoupdate-notifications.jpg)

export function changeImgCreation(renderer: marked.Renderer) {
  renderer.image = (src: string | null, title: string | null, alt: string) => {
    const exec = /=\s*(\d*(?:px|em|ex|ch|rem|vw|vh|vmin|vmax|%))\s*,*\s*(\d*(?:px|em|ex|ch|rem|vw|vh|vmin|vmax|%))*\s*$/.exec(title);

    let style: string = '';
    if (exec) {
      if (exec[1]) {
        style += `--deckgo-lazy-img-width: ${exec[1].replace(',', '')};`;
      }

      if (exec[2]) {
        style += `--deckgo-lazy-img-height: ${exec[2].replace(',', '')};`;
      }
    }

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
