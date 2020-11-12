import marked from 'marked';

// Source: https://github.com/markedjs/marked/issues/339
// ![img](../assets/image/some-image.png "=100x100")
// ![img](../assets/image/some-image.png =250x)

export function changeImgCreation(renderer: marked.Renderer) {
  renderer.image = (src, title, alt) => {
    const exec = /=\s*(\d*)\s*x*\s*(\d*)\s*$/.exec(title);

    let res: string = '<img src="' + sanitize(src) + '" alt="' + sanitize(alt);

    if (exec && exec[1]) {
      res += '" height="' + exec[1];
    }

    if (exec && exec[2]) {
      res += '" width="' + exec[2];
    }

    return res + '">';
  };
}

function sanitize(str: string) {
  return str.replace(/&<"/g, function (m) {
    if (m === '&') return '&amp;';
    if (m === '<') return '&lt;';
    return '&quot;';
  });
}
